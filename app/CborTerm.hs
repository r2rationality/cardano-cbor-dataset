-- | This module provides functions for traversing CBOR terms according to the structure defined by a HuddleSpec rule.
module CborTerm
  ( followPath,
    decodeTaggedTerm,
    arrayEntries,
    entryCount,
    isIndefinite,
    leadingUnsigned,
    mapHasForeignKey,
    mapHasDuplicateKey,
    repeatsAnEntry,
    withRepeatedEntry,
    mapLacks,
    matchesLiteral,
    matchesPrimitive,
    shapeFits,
    termInteger,
    termTag,
  )
where

import Codec.CBOR.Decoding
  ( Decoder,
    TokenType (..),
    decodeBreakOr,
    decodeListLen,
    decodeListLenIndef,
    decodeMapLen,
    decodeMapLenIndef,
    decodeTag,
    peekTokenType,
  )
import Codec.CBOR.Term (Term (..), decodeTerm)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy qualified as TL
import Obligations (LiteralValue (..), Primitive (..), Shape (..), Step (..))

-- | A term with the tags a bignum carries left on it.
--
-- @decodeTerm@ folds tag 2 and tag 3 into a plain integer, which is the right
-- reading of the value and the wrong one for checking the encoding: @big_uint@
-- is @#6.2(bounded_bytes)@, so a term arriving untagged cannot be told from one
-- the specification never tagged, and every obligation under the tag goes
-- unanswered.
--
-- Only the cases that would lose a tag are handled here. Everything else is
-- @decodeTerm@'s, so this cannot drift from cborg's reading of any other type.
-- Containers recurse because a bignum nested in an array or a map is lost just
-- as readily as one at the top.
--
-- The discriminator is 'TypeInteger', which cborg reports for a bignum and
-- nothing else: a plain integer is one of the four sized token types. So the
-- tag is still there to be read when we reach this case, and no backtracking is
-- needed to find out.
decodeTaggedTerm :: Decoder s Term
decodeTaggedTerm = do
  tokenType <- peekTokenType
  case tokenType of
    TypeInteger -> taggedTerm
    TypeTag -> taggedTerm
    TypeTag64 -> taggedTerm
    TypeListLen -> decodeListLen >>= fmap TList . flip replicateM decodeTaggedTerm
    TypeListLen64 -> decodeListLen >>= fmap TList . flip replicateM decodeTaggedTerm
    TypeListLenIndef -> decodeListLenIndef >> (TListI <$> untilBreak decodeTaggedTerm)
    TypeMapLen -> decodeMapLen >>= fmap TMap . flip replicateM pair
    TypeMapLen64 -> decodeMapLen >>= fmap TMap . flip replicateM pair
    TypeMapLenIndef -> decodeMapLenIndef >> (TMapI <$> untilBreak pair)
    _ -> decodeTerm
  where
    taggedTerm = TTagged . fromIntegral <$> decodeTag <*> decodeTaggedTerm
    pair = (,) <$> decodeTaggedTerm <*> decodeTaggedTerm

    untilBreak item = do
      done <- decodeBreakOr
      if done then pure [] else (:) <$> item <*> untilBreak item

-- | Follow a rule path into a decoded term and return the corresponding subterm
--
-- 'Nothing' means the path cannot be resolved against this term, which is not
-- the same as the constraint holding. A repeated schema entry is the usual
-- cause: @{+ k => v}@ is one entry in the tree and many at runtime, so a path
-- descending through one has no single position to land on.
followPath :: [Step] -> Term -> Maybe Term
followPath [] term = Just term
followPath (step : rest) term = case (step, term) of
  (StepChoice _ shape, _)
    | shapeFits shape term -> followPath rest term
    | otherwise -> Nothing
  (StepOccur _, _) -> followPath rest term
  -- A map's key and value are consumed by the step that finds the entry, so a
  -- bare one reaching here came from an array or a group. There the member key
  -- is only a label and the entry is the value itself, so neither step moves.
  (StepKey, _) -> followPath rest term
  (StepValue, _) -> followPath rest term
  (StepControl, _) -> followPath rest term
  (StepGenerator, _) -> followPath rest term
  (StepValidator, _) -> followPath rest term
  (StepTag wanted, TTagged actual inner)
    | wanted == actual -> followPath rest inner
  (StepArray index, TList entries) -> entries !!? index >>= followPath rest
  (StepArray index, TListI entries) -> entries !!? index >>= followPath rest
  (StepGroup index, TList entries) -> entries !!? index >>= followPath rest
  (StepGroup index, TListI entries) -> entries !!? index >>= followPath rest
  -- A keyed entry is found by its key. Its position in the schema is the order
  -- the specification lists it in, which says nothing about where it lands
  -- among whichever fields a sample happens to carry.
  (StepMapKey key, TMap pairs) -> entrySide (find (isKey key . fst) pairs)
  (StepMapKey key, TMapI pairs) -> entrySide (find (isKey key . fst) pairs)
  (StepMapAt index, TMap pairs) -> entrySide (pairs !!? index)
  (StepMapAt index, TMapI pairs) -> entrySide (pairs !!? index)
  -- An open entry beside literal-keyed siblings is found by elimination: an
  -- entry the siblings do not claim can only belong to it. The first such entry
  -- is taken, which is all a single path can name; a sample carrying several is
  -- answered by whichever comes first.
  (StepMapAmong _ taken, TMap pairs) -> entrySide (find (unclaimed taken . fst) pairs)
  (StepMapAmong _ taken, TMapI pairs) -> entrySide (find (unclaimed taken . fst) pairs)
  _ -> Nothing
  where
    -- A map entry is a pair and a term is not, so finding the entry and picking
    -- a side of it are consumed together. The wrappers that sit between them
    -- say nothing about the encoding, so they are stepped over here just as
    -- they are at the top.
    isKey key candidate = matchesLiteral key candidate == Just True

    unclaimed taken candidate = not (any (`isKey` candidate) taken)

    entrySide entry = do
      (key, value) <- entry
      case dropWhile transparent rest of
        StepKey : more -> followPath more key
        StepValue : more -> followPath more value
        _ -> Nothing

    transparent = \case
      StepOccur _ -> True
      StepGenerator -> True
      StepValidator -> True
      StepControl -> True
      _ -> False

-- | Whether a decoded term is the literal the specification names.
--
-- 'Nothing' where the comparison is not settled: a negative literal's encoding
-- and a float's precision both need more care than guessing is worth, and a
-- wrong answer here would be reported as coverage.
matchesLiteral :: LiteralValue -> Term -> Maybe Bool
matchesLiteral = \case
  LitUInt wanted -> \case
    TInt value -> Just (value >= 0 && fromIntegral value == wanted)
    TInteger value -> Just (value >= 0 && fromIntegral value == wanted)
    _ -> Just False
  LitText wanted -> \case
    TString value -> Just (value == wanted)
    TStringI value -> Just (TL.toStrict value == wanted)
    _ -> Just False
  LitBytes wanted -> \case
    TBytes value -> Just (value == wanted)
    TBytesI value -> Just (LBS.toStrict value == wanted)
    _ -> Just False
  LitBool wanted -> \case
    TBool value -> Just (value == wanted)
    _ -> Just False
  LitBignum wanted -> \case
    TInteger value -> Just (value == wanted)
    TInt value -> Just (toInteger value == wanted)
    _ -> Just False
  -- A negative literal is stored as its magnitude, so the value is its
  -- negation: min_int64 resolves to VNInt 9223372036854775808.
  LitNInt wanted -> \case
    TInt value -> Just (toInteger value == negate (toInteger wanted))
    TInteger value -> Just (value == negate (toInteger wanted))
    _ -> Just False
  LitFloat _ -> const Nothing

-- | Whether a term could have been produced by an alternative of this shape.
shapeFits :: Shape -> Term -> Bool
shapeFits = \case
  ShapeArray discriminant -> \case
    TList entries -> opensWith discriminant entries
    TListI entries -> opensWith discriminant entries
    _ -> False
  ShapeMap -> \case
    TMap _ -> True
    TMapI _ -> True
    _ -> False
  ShapeTag wanted -> \case
    TTagged actual _ -> actual == wanted
    _ -> False
  ShapePrimitive primitive -> \term -> matchesPrimitive primitive term == Just True
  ShapeLiteral wanted -> \term -> matchesLiteral wanted term == Just True
  ShapeRange low high -> \term -> maybe False (\value -> low <= value && value <= high) (termInteger term)
  ShapeAnyOf shapes -> \term -> any (`shapeFits` term) shapes
  -- A branch that is not a container of its own: nothing to tell apart.
  ShapeOther -> const True
  -- Indistinguishable from a sibling, so no term settles it either way.
  ShapeAmbiguous -> const False
  where
    opensWith Nothing _ = True
    opensWith (Just wanted) entries = case entries of
      TInt number : _ -> number >= 0 && fromIntegral number == wanted
      TInteger number : _ -> number >= 0 && fromIntegral number == wanted
      _ -> False

-- | A term's elements, if it is an array. Kept apart from 'entryCount' so an
-- obligation about an array's elements cannot be answered by a map of the same
-- size.
arrayEntries :: Term -> Maybe [Term]
arrayEntries = \case
  TList entries -> Just entries
  TListI entries -> Just entries
  _ -> Nothing

-- | How many entries a container holds, if the term is one.
entryCount :: Term -> Maybe Int
entryCount = \case
  TList entries -> Just (length entries)
  TListI entries -> Just (length entries)
  TMap pairs -> Just (length pairs)
  TMapI pairs -> Just (length pairs)
  _ -> Nothing

-- | The tag a term carries, if it is tagged.
termTag :: Term -> Maybe Word64
termTag = \case
  TTagged number _ -> Just number
  _ -> Nothing

-- | The non-negative number an array opens with, which is how a union written
-- as arrays discriminates its alternatives.
leadingUnsigned :: Term -> Maybe Word64
leadingUnsigned = \case
  TList entries -> opener entries
  TListI entries -> opener entries
  _ -> Nothing
  where
    opener = \case
      TInt number : _ | number >= 0 -> Just (fromIntegral number)
      TInteger number : _ | number >= 0 -> Just (fromIntegral number)
      _ -> Nothing

-- | The number a term carries, for the obligations that are about a value
-- rather than a shape.
termInteger :: Term -> Maybe Integer
termInteger = \case
  TInt number -> Just (toInteger number)
  TInteger number -> Just number
  -- A bignum keeps its tag here so the encoding can be checked, which leaves
  -- its value spelled out in the payload rather than decoded for us. Reading it
  -- back is what lets a bound be compared against a number too large for a uint
  -- head: @2^64@ is only expressible this way.
  TTagged 2 (TBytes payload) -> Just (unsigned payload)
  TTagged 3 (TBytes payload) -> Just (-1 - unsigned payload)
  _ -> Nothing
  where
    unsigned = BS.foldl' (\acc byte -> acc * 256 + toInteger byte) 0

-- | Whether a term is of the primitive type the specification names.
matchesPrimitive :: Primitive -> Term -> Maybe Bool
matchesPrimitive = \case
  PrimUInt -> \case
    TInt value -> Just (value >= 0)
    TInteger value -> Just (value >= 0)
    _ -> Just False
  PrimNInt -> \case
    TInt value -> Just (value < 0)
    TInteger value -> Just (value < 0)
    _ -> Just False
  PrimInt -> \case
    TInt _ -> Just True
    TInteger _ -> Just True
    _ -> Just False
  PrimBytes -> \case
    TBytes _ -> Just True
    TBytesI _ -> Just True
    _ -> Just False
  PrimText -> \case
    TString _ -> Just True
    TStringI _ -> Just True
    _ -> Just False
  PrimBool -> \case
    TBool _ -> Just True
    _ -> Just False
  PrimNil -> \case
    TNull -> Just True
    _ -> Just False
  PrimArray -> \case
    TList _ -> Just True
    TListI _ -> Just True
    _ -> Just False
  PrimMap -> \case
    TMap _ -> Just True
    TMapI _ -> Just True
    _ -> Just False

-- | Whether a map carries an entry under a key none of these name.
mapHasForeignKey :: [LiteralValue] -> Term -> Maybe Bool
mapHasForeignKey keys = \case
  TMap pairs -> Just (any foreign_ pairs)
  TMapI pairs -> Just (any foreign_ pairs)
  _ -> Nothing
  where
    foreign_ (key, _) = not (any (\known -> matchesLiteral known key == Just True) keys)

-- | Whether a map carries the same key twice.
--
-- Keys are compared as they were encoded, so two spellings of one value, a
-- small integer written in two widths say, are not called equal. That
-- understates rather than overstates: a sample built by repeating an entry
-- carries the same bytes both times.
mapHasDuplicateKey :: Term -> Maybe Bool
mapHasDuplicateKey = \case
  TMap pairs -> Just (repeats pairs)
  TMapI pairs -> Just (repeats pairs)
  _ -> Nothing
  where
    repeats pairs = let keys = map fst pairs in length (ordNub keys) /= length keys

-- | Whether a container carries the same entry twice.
--
-- A set's uniqueness is the one thing the tree cannot express, so this is what
-- a sample breaking it looks like from the outside. Entries are compared as
-- they were encoded, for the reason keys are.
repeatsAnEntry :: Term -> Maybe Bool
repeatsAnEntry = \case
  TList entries -> Just (repeats entries)
  TListI entries -> Just (repeats entries)
  TMap pairs -> Just (repeats (map fst pairs))
  TMapI pairs -> Just (repeats (map fst pairs))
  TTagged _ inner -> repeatsAnEntry inner
  _ -> Nothing
  where
    repeats entries = length (ordNub entries) /= length entries

-- | The same container with its first entry repeated, or 'Nothing' where there
-- is no entry to repeat.
--
-- This is the probe: a rule that accepts a term and refuses this one enforces
-- uniqueness, whatever its validator is written in. A rule of the same shape
-- with no validator accepts both, which is what tells @nonempty_set@ from
-- @nonempty_list@.
withRepeatedEntry :: Term -> Maybe Term
withRepeatedEntry = \case
  TList (entry : rest) -> Just (TList (entry : entry : rest))
  TListI (entry : rest) -> Just (TListI (entry : entry : rest))
  TMap (pair : rest) -> Just (TMap (pair : pair : rest))
  TMapI (pair : rest) -> Just (TMapI (pair : pair : rest))
  TTagged tag inner -> TTagged tag <$> withRepeatedEntry inner
  _ -> Nothing

-- | Whether a map carries no entry under this key.
mapLacks :: LiteralValue -> Term -> Maybe Bool
mapLacks key = \case
  TMap pairs -> Just (none pairs)
  TMapI pairs -> Just (none pairs)
  _ -> Nothing
  where
    none pairs = not (any (\(candidate, _) -> matchesLiteral key candidate == Just True) pairs)

-- | Whether a container uses the indefinite-length encoding.
isIndefinite :: Term -> Maybe Bool
isIndefinite = \case
  TListI _ -> Just True
  TMapI _ -> Just True
  TList _ -> Just False
  TMap _ -> Just False
  _ -> Nothing
