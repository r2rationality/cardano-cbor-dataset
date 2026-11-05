module Coverage.Obligations.CborTerm where

import Coverage.Obligations.Cbor
import Coverage.Obligations.LiteralValue (LiteralValue (..))
import Coverage.Obligations.Primitive (Primitive (..))
import Coverage.Obligations.Shape (Shape (..))
import Coverage.Obligations.Step (Step (..))
import Coverage.Obligations.Trail (Absolute, Trail, steps)
import Data.ByteString qualified as BS

-- | This module provides functions for traversing CBOR values according to the structure defined by a HuddleSpec rule.

-- | Follow a rule path into a decoded value and return the values it reaches.
--
-- An empty list means the path cannot be resolved against this value, which is not the same as the constraint holding.
-- A path through a repeated entry reaches every occurrence: @{* k => v}@ is one entry in the tree and many at runtime,
-- and a constraint on it holds for each of them.
followPath :: Trail Absolute -> Cbor -> [Cbor]
followPath = followSteps . steps

-- | Follow steps into a value, from the top of the value down.
followSteps :: [Step] -> Cbor -> [Cbor]
followSteps [] value = [value]
followSteps (step : rest) value = case (step, value) of
  (StepChoice _ shape, _)
    | shapeFits shape value -> followSteps rest value
    | otherwise -> []
  (StepOccur _, _) -> followSteps rest value
  (StepRepeat _, _) -> followSteps rest value
  -- A map's key and value are consumed by the step that finds the entry, so a
  -- bare one reaching here came from an array or a group. There the member key
  -- is only a label and the entry is the value itself, so neither step moves.
  (StepKey, _) -> followSteps rest value
  (StepValue, _) -> followSteps rest value
  (StepControl, _) -> followSteps rest value
  (StepGenerator, _) -> followSteps rest value
  (StepValidator, _) -> followSteps rest value
  (StepTag wanted, CTag _ actual inner)
    | wanted == actual -> followSteps rest inner
  (StepArray index, CArray _ entries) -> from index entries >>= followSteps rest
  (StepArrayOf index full, CArray _ entries) -> aligned index full entries >>= followSteps rest
  (StepGroup index, CArray _ entries) -> from index entries >>= followSteps rest
  -- A keyed entry is found by its key. Its position in the schema is the order
  -- the specification lists it in, which says nothing about where it lands
  -- among whichever fields a sample happens to carry.
  (StepMapKey key, CMap _ pairs) -> maybeToList (find (isKey key . fst) pairs) >>= entrySide
  (StepMapAt index, CMap _ pairs) -> from index pairs >>= entrySide
  -- An open entry beside literal-keyed siblings is found by elimination: an
  -- entry the siblings do not claim can only belong to it.
  (StepMapAmong _ taken, CMap _ pairs) -> among taken pairs >>= entrySide
  _ -> []
  where
    isKey key candidate = matchesLiteral key candidate == Just True

    -- A repeated entry is the last one with a position in its container, so its occurrences are every element from
    -- its position on.
    from index entries
      | repeats = drop index entries
      | otherwise = maybeToList (entries !!? index)

    among taken pairs
      | repeats = unclaimed
      | otherwise = take 1 unclaimed
      where
        unclaimed = filter (not . claimed . fst) pairs
        claimed candidate = any (`isKey` candidate) taken

    repeats = case rest of
      StepRepeat _ : _ -> True
      _ -> False

    -- An array one element shorter than its full form has no optional element, and the elements after it moved back
    -- by one. The optional element is the one whose occurrence starts at zero.
    aligned index full entries
      | length entries == full = maybeToList (entries !!? index)
      | length entries == full - 1, not optionalHere = maybeToList (entries !!? (index - 1))
      | otherwise = []
    optionalHere = case rest of
      StepOccur 0 : _ -> True
      _ -> False

    -- A map entry is a pair and a value is not, so finding the entry and picking
    -- a side of it are consumed together. The wrappers that sit between them
    -- say nothing about the encoding, so they are stepped over here just as
    -- they are at the top.
    entrySide (key, entryValue) =
      case dropWhile transparent rest of
        StepKey : more -> followSteps more key
        StepValue : more -> followSteps more entryValue
        _ -> []

    transparent = \case
      StepOccur _ -> True
      StepRepeat _ -> True
      StepGenerator -> True
      StepValidator -> True
      StepControl -> True
      _ -> False

-- | Whether a decoded value is the literal the specification names.
--
-- 'Nothing' where the comparison is not settled: a negative literal's encoding and a float's precision both need more
-- care than guessing is worth, and a wrong answer here would be reported as coverage.
matchesLiteral :: LiteralValue -> Cbor -> Maybe Bool
matchesLiteral = \case
  LitUInt wanted -> \case
    CInteger _ number -> Just (number >= 0 && number == toInteger wanted)
    _ -> Just False
  LitText wanted -> \case
    CText _ payload -> Just (payload == wanted)
    _ -> Just False
  LitBytes wanted -> \case
    CBytes _ payload -> Just (payload == wanted)
    _ -> Just False
  LitBool wanted -> \case
    CBool actual -> Just (actual == wanted)
    _ -> Just False
  LitBignum wanted -> \case
    CInteger _ number -> Just (number == wanted)
    _ -> Just False
  -- A negative literal is stored as its magnitude, so the value is its
  -- negation: min_int64 resolves to VNInt 9223372036854775808.
  LitNInt wanted -> \case
    CInteger _ number -> Just (number == negate (toInteger wanted))
    _ -> Just False
  LitFloat _ -> const Nothing

-- | Whether a value could have been produced by an alternative of this shape.
shapeFits :: Shape -> Cbor -> Bool
shapeFits = \case
  ShapeArray discriminant -> \case
    CArray _ entries -> opensWith discriminant entries
    _ -> False
  ShapeMap -> \case
    CMap _ _ -> True
    _ -> False
  ShapeTag wanted -> \case
    CTag _ actual _ -> actual == wanted
    _ -> False
  ShapePrimitive primitive -> \value -> matchesPrimitive primitive value == Just True
  ShapeLiteral wanted -> \value -> matchesLiteral wanted value == Just True
  ShapeRange low high -> maybe False (\number -> low <= number && number <= high) . termInteger
  ShapeAnyOf shapes -> \value -> any (`shapeFits` value) shapes
  -- A branch that is not a container of its own: nothing to tell apart.
  ShapeOther -> const True
  -- Indistinguishable from a sibling: a value of the shape they share reaches both.
  ShapeAmbiguous shape -> shapeFits shape
  where
    opensWith Nothing _ = True
    opensWith (Just wanted) entries = case entries of
      CInteger _ number : _ -> number >= 0 && number == toInteger wanted
      _ -> False

-- | A value's elements, if it is an array. Kept apart from 'entryCount' so an obligation about an array's elements
-- cannot be answered by a map of the same size.
arrayEntries :: Cbor -> Maybe [Cbor]
arrayEntries = \case
  CArray _ entries -> Just entries
  _ -> Nothing

-- | How many entries a container holds, if the value is one.
entryCount :: Cbor -> Maybe Int
entryCount = \case
  CArray _ entries -> Just (length entries)
  CMap _ pairs -> Just (length pairs)
  _ -> Nothing

-- | The tag a value carries, if it is tagged.
termTag :: Cbor -> Maybe Word64
termTag = \case
  CTag _ number _ -> Just number
  _ -> Nothing

-- | The non-negative number an array opens with, which is how a union written as arrays discriminates its alternatives.
leadingUnsigned :: Cbor -> Maybe Word64
leadingUnsigned = \case
  CArray _ (CInteger _ number : _) | number >= 0 -> Just (fromIntegral number)
  _ -> Nothing

-- | The number a value carries, for the obligations that are about a number rather than a shape.
termInteger :: Cbor -> Maybe Integer
termInteger = \case
  CInteger _ number -> Just number
  -- A bignum keeps its tag here so the encoding can be checked, which leaves
  -- its value spelled out in the payload rather than decoded for us. Reading it
  -- back is what lets a bound be compared against a number too large for a uint
  -- head: @2^64@ is only expressible this way.
  CTag _ 2 (CBytes _ payload) -> Just (unsigned payload)
  CTag _ 3 (CBytes _ payload) -> Just (-1 - unsigned payload)
  _ -> Nothing
  where
    unsigned = BS.foldl' (\acc byte -> acc * 256 + toInteger byte) 0

-- | Whether a value is of the primitive type the specification names.
matchesPrimitive :: Primitive -> Cbor -> Maybe Bool
matchesPrimitive = \case
  PrimUInt -> \case
    CInteger _ number -> Just (number >= 0)
    _ -> Just False
  PrimNInt -> \case
    CInteger _ number -> Just (number < 0)
    _ -> Just False
  PrimInt -> \case
    CInteger _ _ -> Just True
    _ -> Just False
  PrimBytes -> \case
    CBytes _ _ -> Just True
    _ -> Just False
  PrimText -> \case
    CText _ _ -> Just True
    _ -> Just False
  PrimBool -> \case
    CBool _ -> Just True
    _ -> Just False
  PrimNil -> \case
    CNull -> Just True
    _ -> Just False
  PrimArray -> \case
    CArray _ _ -> Just True
    _ -> Just False
  PrimMap -> \case
    CMap _ _ -> Just True
    _ -> Just False

-- | Whether a map carries an entry under a key none of these name.
mapHasForeignKey :: [LiteralValue] -> Cbor -> Maybe Bool
mapHasForeignKey keys = \case
  CMap _ pairs -> Just (any foreign_ pairs)
  _ -> Nothing
  where
    foreign_ (key, _) = not (any (\known -> matchesLiteral known key == Just True) keys)

-- | Whether a map carries the same key twice.
--
-- Keys are compared by what they say, not by how they were written: two spellings of one key are the same key, which is
-- what a decoder building a map would find.
mapHasDuplicateKey :: Cbor -> Maybe Bool
mapHasDuplicateKey = \case
  CMap _ pairs -> Just (repeats pairs)
  _ -> Nothing
  where
    repeats pairs = let keys = map (toTerm . fst) pairs in length (ordNub keys) /= length keys

-- | Whether a map carries no entry under this key.
mapLacks :: LiteralValue -> Cbor -> Maybe Bool
mapLacks key = \case
  CMap _ pairs -> Just (none pairs)
  _ -> Nothing
  where
    none pairs = not (any (\(candidate, _) -> matchesLiteral key candidate == Just True) pairs)

-- | Whether a container uses the indefinite-length encoding.
isIndefinite :: Cbor -> Maybe Bool
isIndefinite = \case
  CArray len _ -> Just (len == Indefinite)
  CMap len _ -> Just (len == Indefinite)
  _ -> Nothing

-- | The width of the head of a value, if the value has a head that spells a number: a string, an array, a map, an
-- integer or a tag. A container or a string whose length is not stated has none: it is not the same as a minimal one.
headWidth :: Cbor -> Maybe Width
headWidth = \case
  CInteger width _ -> Just width
  CBytes (Definite width) _ -> Just width
  CText (Definite width) _ -> Just width
  CArray (Definite width) _ -> Just width
  CMap (Definite width) _ -> Just width
  CTag width _ _ -> Just width
  CBytes Indefinite _ -> Just Minimal
  CText Indefinite _ -> Just Minimal
  CArray Indefinite _ -> Just Minimal
  CMap Indefinite _ -> Just Minimal
  _ -> Nothing

-- | Whether the head of a value is wider than it needs. Nothing for a value that has no head to spell.
hasWideHead :: Cbor -> Maybe Bool
hasWideHead = fmap (/= Minimal) . headWidth

-- | Whether the head of a value is the 8-byte one for a number that fits in fewer bytes. Nothing for a value that has no
-- head to spell.
hasWidestHead :: Cbor -> Maybe Bool
hasWidestHead = fmap (== Widest) . headWidth
