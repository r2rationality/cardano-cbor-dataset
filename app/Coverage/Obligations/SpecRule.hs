module Coverage.Obligations.SpecRule where

import Base.Paths (abort)
import Cardano.Ledger.Conway.HuddleSpec (conwayCDDL)
import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (isValid)
import Codec.CBOR.Cuddle.CDDL hiding (value)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.Huddle qualified as Cuddle
import Codec.CBOR.Cuddle.IndexMappable (mapIndex)
import Codec.CBOR.Term (Term, encodeTerm)
import Codec.CBOR.Write qualified as CBOR
import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Primitive
import Coverage.Obligations.Shape
import Coverage.Obligations.Step
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Cardano.Ledger.Binary.Cuddle (resolveHuddle)

-- | The full Huddle specification, as a resolved CDDL syntax tree.
type HuddleSpec = CTreeRoot MonoReferenced

-- | One node of a rule in a Huddle specification, as a resolved CDDL syntax tree. It can be the body of a whole
-- rule, or any element inside it.
type RuleNode = CTree MonoReferenced

-- | A rule of the specification: its name and its definition.
data SpecRule = SpecRule
  { specRuleName :: Name,
    specRuleBody :: RuleNode
  }

-- | The rule with this name in the specification, or a failure that names it.
requireRule :: HuddleSpec -> Name -> IO RuleNode
requireRule (CTreeRoot rules) name =
  maybe (abort $ "'" <> unName name <> "' does not name a rule of the specification") pure (Map.lookup name rules)

-- | Whether a rule admits exactly the terms its shape fits.
--
-- @coin@ is @uint@ and nothing more, so every uint is a coin. @hash32@ is bytes and also thirty two of them, so a byte
-- string can break the rule while still looking like one. The difference decides whether denying the rule at an
-- alternative found by that shape is also denying the shape, which is the difference between an obligation worth
-- stating and one no sample can meet.
--
-- Answered conservatively: anything not recognized here is taken to say more than its shape, which keeps an obligation
-- that might be met rather than dropping one that could have been.
shapeIsWholeRule :: Map Name RuleNode -> RuleNode -> Bool
shapeIsWholeRule rules = go mempty
  where
    go seen = \case
      -- A primitive admits every term of its type and asks nothing else.
      CTree.Postlude term -> isJust (primitiveNamed (show term))
      CTree.Literal _ -> True
      CTree.Range from to _ ->
        isJust ((,) <$> literalInteger rules from <*> literalInteger rules to)
      -- Only where the encoding settles the payload: carrying the tag is then
      -- all there is to being a value of the rule.
      CTree.Tag number _ -> encodingFixesPayload number
      CTree.Choice alternatives -> all (go seen) (NE.toList alternatives)
      CTree.CTreeE (MRuleRef ref)
        | ref `Set.member` seen -> False
        | otherwise -> maybe False (go (Set.insert ref seen)) (Map.lookup ref rules)
      CTree.CTreeE (MGenerator _ inner) -> go seen inner
      _ -> False

shapeOf :: Map Name RuleNode -> RuleNode -> Shape
shapeOf rules = go mempty
  where
    go seen = \case
      CTree.Array entries -> ShapeArray (leadingUInt rules entries)
      CTree.Map _ -> ShapeMap
      CTree.Tag number _ -> ShapeTag number
      CTree.Postlude term -> maybe ShapeOther ShapePrimitive (primitiveNamed (show term))
      CTree.Literal (Value variant _) -> ShapeLiteral (fromVariant variant)
      CTree.Choice alternatives -> ShapeAnyOf (map (go seen) (NE.toList alternatives))
      CTree.Range from to _ ->
        maybe ShapeOther (uncurry ShapeRange) ((,) <$> literalInteger rules from <*> literalInteger rules to)
      CTree.CTreeE (MGenerator _ inner) -> go seen inner
      CTree.CTreeE (MValidator _ inner) -> go seen inner
      -- A control narrows the values its target admits without changing what
      -- kind of value that is, so the shape is the target's. Left opaque, a
      -- constrained branch such as @bytes .size (0..64)@ would fit any term and
      -- answer for samples that took a sibling branch instead.
      CTree.Control _ target _ -> go seen target
      -- An alternative that is a bare reference has the shape of what it names.
      -- Without following it every such branch fits every term, so a branch
      -- resolves for a sample that took a different one and everything stated
      -- under it is answered about the wrong value.
      CTree.CTreeE (MRuleRef ref)
        | ref `Set.member` seen -> ShapeOther
        | otherwise -> maybe ShapeOther (go (Set.insert ref seen)) (Map.lookup ref rules)
      _ -> ShapeOther

-- | The shape of one choice alternative, looking through the wrappers that do not change the encoding. | The shapes an
-- array admits at each of its positions, where every entry contributes exactly one element.
--
-- 'Nothing' where it does not. A group splices an unknown number of entries in and a repeated entry has no fixed
-- position, so neither has a position to speak about, and guessing one would state an obligation about the wrong
-- element.
arraySignature :: Map Name RuleNode -> RuleNode -> Maybe [[Shape]]
arraySignature rules = container mempty
  where
    container seen = \case
      CTree.Array entries -> traverse (slot seen) entries
      CTree.CTreeE (MGenerator _ inner) -> container seen inner
      CTree.CTreeE (MValidator _ inner) -> container seen inner
      CTree.CTreeE (MRuleRef ref) -> follow seen ref (container (Set.insert ref seen))
      _ -> Nothing

    -- A member label never reaches the bytes, so a labelled entry is its value.
    -- An alternative at a position contributes each of its shapes, which is
    -- what lets @coin / nil@ be stated as two admitted combinations rather than
    -- as one opaque element.
    slot seen = \case
      CTree.KV _ value _ -> slot seen value
      CTree.Choice alternatives -> Just (ordNub (map (shapeOf rules) (NE.toList alternatives)))
      CTree.CTreeE (MGenerator _ inner) -> slot seen inner
      CTree.CTreeE (MValidator _ inner) -> slot seen inner
      CTree.CTreeE (MRuleRef ref) -> follow seen ref (slot (Set.insert ref seen))
      CTree.Occur _ _ -> Nothing
      CTree.Group _ -> Nothing
      other -> Just [shapeOf rules other]

    follow seen ref continue
      | ref `Set.member` seen = Nothing
      | otherwise = Map.lookup ref rules >>= continue

-- | The literal key of a map entry, looking through an optionality wrapper: @? 13 : x@ is an occurrence around a keyed
-- entry, not a different shape.
entryKey :: RuleNode -> Maybe LiteralValue
entryKey = \case
  CTree.KV key _ _ -> literalValue key
  CTree.Occur inner _ -> entryKey inner
  _ -> Nothing

-- | The discriminant an alternative opens with, looking through the group a @//@ alternative is spliced in as.
leadingUInt :: Map Name RuleNode -> [RuleNode] -> Maybe Word64
leadingUInt rules = \case
  CTree.Literal (Value (VUInt number) _) : _ -> Just number
  CTree.Group inner : _ -> leadingUInt rules inner
  -- info_action = 6 is a rule of its own, so the discriminant of the
  -- alternative that names it is one reference away.
  CTree.CTreeE (MRuleRef ref) : _ -> case Map.lookup ref rules of
    Just (CTree.Literal (Value (VUInt number) _)) -> Just number
    Just (CTree.Group inner) -> leadingUInt rules inner
    _ -> Nothing
  _ -> Nothing

-- | The bounds a control's argument states, as a closed range. An exact size is the range that starts and ends in the
-- same place.
controllerBounds :: RuleNode -> Maybe (Integer, Integer)
controllerBounds = \case
  CTree.Literal (Value (VUInt number) _) -> Just (toInteger number, toInteger number)
  CTree.Range from to _ -> (,) <$> literalUInt from <*> literalUInt to
  _ -> Nothing

literalUInt :: RuleNode -> Maybe Integer
literalUInt = \case
  CTree.Literal (Value (VUInt number) _) -> Just (toInteger number)
  _ -> Nothing

-- | The number a range endpoint names, following the reference when the bound is a rule of its own: @positive_int = 1
-- .. max_word64@ names one both ways.
--
-- A negative literal is stored as its magnitude, which @min_int64@ settles: it resolves to @VNInt 9223372036854775808@.
literalInteger :: Map Name RuleNode -> RuleNode -> Maybe Integer
literalInteger rules = \case
  CTree.Literal (Value variant _) -> fromVariantInteger variant
  CTree.CTreeE (MRuleRef ref) -> case Map.lookup ref rules of
    Just (CTree.Literal (Value variant _)) -> fromVariantInteger variant
    _ -> Nothing
  _ -> Nothing

literalRange :: Map Name RuleNode -> RuleNode -> RuleNode -> Maybe (Integer, Integer)
literalRange rules from to = (,) <$> literalInteger rules from <*> literalInteger rules to

fromVariantInteger :: ValueVariant -> Maybe Integer
fromVariantInteger = \case
  VUInt number -> Just (toInteger number)
  VNInt number -> Just (negate (toInteger number))
  VBignum number -> Just number
  _ -> Nothing

-- | The literal value a rule names, if it is a literal.
literalValue :: RuleNode -> Maybe LiteralValue
literalValue = \case
  CTree.Literal (Value variant _) -> Just (fromVariant variant)
  _ -> Nothing

-- | Lower bound, and upper bound when there is one.
occurrenceBounds :: OccurrenceIndicator -> (Word64, Maybe Word64)
occurrenceBounds = \case
  OIOptional -> (0, Just 1)
  OIZeroOrMore -> (0, Nothing)
  OIOneOrMore -> (1, Nothing)
  OIBounded lower upper -> (fromMaybe 0 lower, upper)

-- | How many elements one entry contributes to the container holding it.
--
-- A type is one element. A group splices its own entries in, so it contributes as many as they do, which is the same
-- sum one level down. A choice may do either depending on the alternative taken, so it gives up, as does a group that
-- reaches itself: a width that depends on itself is not a width.
entryWidth :: Map Name RuleNode -> RuleNode -> Maybe (Word64, Maybe Word64)
entryWidth rules = widthOf mempty
  where
    widthOf :: Set Name -> RuleNode -> Maybe (Word64, Maybe Word64)
    widthOf seen = \case
      CTree.Occur _ occurs -> Just (occurrenceBounds occurs)
      CTree.CTreeE (MRuleRef ref)
        | ref `Set.member` seen -> Nothing
        | otherwise -> case Map.lookup ref rules of
            Just (CTree.Group entries) -> spliced (Set.insert ref seen) entries
            Just (CTree.Choice alternatives) -> agreed (Set.insert ref seen) alternatives
            Just _ -> Just (1, Just 1)
            Nothing -> Nothing
      CTree.CTreeE (MGenerator _ inner) -> widthOf seen inner
      CTree.CTreeE (MValidator _ inner) -> widthOf seen inner
      CTree.Group entries -> spliced seen entries
      CTree.Choice alternatives -> agreed seen alternatives
      CTree.KV {} -> Just (1, Just 1)
      CTree.Literal _ -> Just (1, Just 1)
      CTree.Postlude _ -> Just (1, Just 1)
      CTree.Array _ -> Just (1, Just 1)
      CTree.Map _ -> Just (1, Just 1)
      CTree.Tag _ _ -> Just (1, Just 1)
      CTree.Range {} -> Just (1, Just 1)
      CTree.Control {} -> Just (1, Just 1)
      _ -> Nothing

    -- A choice contributes what its alternatives contribute, but only when they
    -- all contribute the same: @anchor\/ nil@ is one element whichever way it
    -- goes, while alternatives that splice different numbers of entries leave
    -- the container without a length.
    agreed seen alternatives = do
      widths <- traverse (widthOf seen) (toList alternatives)
      case ordNub widths of
        [width] -> Just width
        _ -> Nothing

    spliced seen entries = do
      widths <- traverse (widthOf seen) entries
      uppers <- traverse snd widths
      pure (sum (map fst widths), Just (sum uppers))

-- | Whether CBOR itself settles the type of what a tag may wrap.
--
-- Tags 2 and 3 are the bignums, and the encoding admits only a byte string under them.
encodingFixesPayload :: Word64 -> Bool
encodingFixesPayload number = number `elem` [2, 3]

-- | How long a container may be: the sum of what its entries contribute.
--
-- 'Nothing' when any entry's own width is unknown, so nothing can be said. An absent upper bound means a repeated entry
-- with no ceiling.
totalWidth ::
  Map Name RuleNode ->
  [RuleNode] ->
  Maybe (Word64, Maybe Word64)
totalWidth rules entries = do
  widths <- traverse (entryWidth rules) entries
  pure (sum (map fst widths), sum <$> traverse snd widths)

-- | How wide a rule may be printed before it is broken across lines.
widestRuleLine :: Int
widestRuleLine = 88

-- | One rule as CDDL text, to read beside the obligations derived from it.
--
-- Written from the resolved tree rather than the specification's source, so the names it uses are the names the page
-- lists. A folded rule has no row, so its name would send a reader looking for one that is not there: what it holds is
-- spelled out where it is used, which is also where its obligations are stated.
--
-- The rules to spell out are the ones @expanded@ accepts, and groups, which are spliced into whatever carries them.
--
-- References to ordinary rules are printed by name rather than followed. The page has a row for each of those, so
-- following them would repeat what is already one click away and bury the rule being read.
renderRule :: (Name -> Bool) -> Map Name RuleNode -> RuleNode -> Text
renderRule expanded rules = go (0 :: Int) (0 :: Int)
  where
    go depth indent = \case
      CTree.Postlude term -> maybe (show term) renderPrimitive (primitiveNamed (show term))
      CTree.Literal (Value variant _) -> renderLiteral (fromVariant variant)
      CTree.Range from to _ -> go depth indent from <> " .. " <> go depth indent to
      CTree.Array entries -> container indent "[" "]" (parts depth indent entries)
      CTree.Map entries -> container indent "{" "}" (parts depth indent entries)
      CTree.Group entries -> T.intercalate ", " (parts depth indent entries)
      CTree.KV key value _ -> go depth indent key <> " : " <> go depth indent value
      CTree.Occur item occurs -> occurrence (occurrenceBounds occurs) <> go depth indent item
      CTree.Choice alternatives -> choice indent (parts depth indent (toList alternatives))
      CTree.Tag number inner -> "#6." <> show number <> "(" <> go depth indent inner <> ")"
      CTree.Control op target controller ->
        -- CDDL writes its control operators in lower case.
        go depth indent target <> " ." <> T.toLower (show op) <> " " <> go depth indent controller
      CTree.CTreeE (MGenerator _ inner) -> go depth indent inner
      CTree.CTreeE (MValidator _ inner) -> go depth indent inner
      CTree.CTreeE (MRuleRef ref)
        -- No row to send the reader to, so the name would be a dead end: a
        -- folded rule has none by choice, and a group has none because it is
        -- spliced into whatever carries it. Both are spelled out instead, to a
        -- depth that keeps the text readable.
        | Just body <- Map.lookup ref rules,
          depth < 3,
          expanded ref || isGroup body ->
            go (depth + 1) indent body
        | otherwise -> unName ref
      _ -> "..."

    -- A group's entries are its container's entries, on the wire and here, so
    -- they are laid out as siblings rather than crammed into one line of their
    -- own.
    parts depth indent entries =
      map (go depth (indent + 1)) (concatMap spliceOut entries)

    spliceOut entry = case entry of
      CTree.Group inner -> concatMap spliceOut inner
      CTree.CTreeE (MRuleRef ref)
        | Just (CTree.Group inner) <- Map.lookup ref rules -> concatMap spliceOut inner
      _ -> [entry]

    -- One line where it fits, one entry per line where it does not. A rule the
    -- browser has to wrap is harder to read than one that chose its own breaks,
    -- and the wide ones are exactly the ones worth reading carefully.
    container indent open close entries
      | T.length inline <= widestRuleLine = inline
      | otherwise =
          open
            <> "\n"
            <> T.intercalate ",\n" [pad (indent + 1) <> entry | entry <- entries]
            <> "\n"
            <> pad indent
            <> close
      where
        inline = open <> T.intercalate ", " entries <> close

    -- Alternatives read down the page rather than across it, with the bar
    -- leading each line so the choice is visible at a glance.
    choice indent alternatives
      | T.length inline <= widestRuleLine = inline
      | otherwise = T.intercalate ("\n" <> pad (indent + 1) <> "/ ") alternatives
      where
        inline = T.intercalate " / " alternatives

    isGroup = \case
      CTree.Group _ -> True
      _ -> False

    pad level = T.replicate level "  "

    occurrence = \case
      (0, Just 1) -> "? "
      (0, Nothing) -> "* "
      (1, Nothing) -> "+ "
      (low, Just high) | low == high -> show low <> "* "
      (low, Nothing) -> show low <> "* "
      (low, Just high) -> show low <> "*" <> show high <> " "

-- | Whether a sub-term is a valid value of the rule named at this site.
--
-- cuddle exposes no way to run a rule against a term, so the term goes back through the public entry point: encode it
-- and validate those bytes. The re-encoding may choose a different length form from the original, which is harmless for
-- a reference check and would not be for one about the form.
isCddlValid :: HuddleSpec -> Name -> Term -> Bool
isCddlValid specification name term =
  case validateCBOR (CBOR.toStrictByteString (encodeTerm term)) name (mapIndex specification) of
    Right evidenced -> isValid evidenced
    Left _ -> False

-- ERA SPECIFICATIONS

-- | Extract a root CTree for the given era.
eraRoot :: Text -> Either Text HuddleSpec
eraRoot era = do
  huddle <- eraHuddle era
  first toText $ resolveHuddle huddle

-- | The era's Huddle value, resolved with the ledger's own helper, which is the same call @generate-cbor@ makes.
-- Measuring a different tree from the one that generates the data would be measuring the wrong thing.
eraHuddle :: Text -> Either Text Cuddle.Huddle
eraHuddle = \case
  "conway" -> Right conwayCDDL
  "dijkstra" -> Right dijkstraCDDL
  other -> Left $ "no Huddle specification for era '" <> other <> "'"

-- ENTRIES

-- | Whether an entry always contributes the same number of elements to its container.
fixedWidth :: Map Name RuleNode -> RuleNode -> Bool
fixedWidth rules entry = case entryWidth rules entry of
  Just (low, high) -> Just low == high
  Nothing -> False

-- | Whether exactly one entry of a container can change the length of the container.
lengthVariesOnOneEntry :: Map Name RuleNode -> [RuleNode] -> Bool
lengthVariesOnOneEntry rules entries = length (filter (not . fixedWidth rules) entries) == 1

-- | Whether an entry can repeat without an upper limit.
repeats :: Map Name RuleNode -> RuleNode -> Bool
repeats rules entry = case entryWidth rules entry of
  Just (_, Nothing) -> True
  _ -> False

-- | The entries a group puts in its container's place, directly or through a reference. Nothing for anything that
-- occupies one slot of its own.
splicedEntries :: Map Name RuleNode -> RuleNode -> Maybe [RuleNode]
splicedEntries rules = \case
  CTree.Group entries -> Just entries
  CTree.CTreeE (MRuleRef ref) -> case Map.lookup ref rules of
    Just (CTree.Group entries) -> Just entries
    _ -> Nothing
  _ -> Nothing

-- | Whether a name refers to a group, which splices its entries into whatever contains it.
refersToGroup :: Map Name RuleNode -> Name -> Bool
refersToGroup rules ref = case Map.lookup ref rules of
  Just (CTree.Group _) -> True
  _ -> False

-- | One more than the container holds with every open entry appearing just once, which is the smallest size that
-- cannot be reached without repeating one of them.
--
-- Nothing else asks for the repetition. @{+ reward_account => coin}@ is fulfilled by a map of one entry, and
-- @cost_models@ by a map of the four its specification lists, so a decoder that stops after the first open key would
-- never be caught. 'Nothing' where no entry is open, since then the lengths are bounded and already stated.
repeatedWidth :: Map Name RuleNode -> [RuleNode] -> Maybe Word64
repeatedWidth rules entries = do
  widths <- traverse (entryWidth rules) entries
  guard (any (isNothing . snd) widths)
  pure (sum [fromMaybe (max 1 low) high | (low, high) <- widths] + 1)

-- | The step that finds a map entry in a sample.
--
--   - A keyed entry is found in a sample by its key. Its position in the schema is the order the specification lists it
--     in, which has nothing to do with where it lands among whichever fields a sample happens to carry.
--   - A repeated entry has no single position either way.
--   - An open key in a map with nothing else in it takes every entry a sample carries. Beside literal-keyed siblings it
--     takes whatever they do not.
mapStep :: [RuleNode] -> Int -> RuleNode -> Step
mapStep siblings index entry = case entryKey entry of
  Just key -> StepMapKey key
  Nothing
    | [_] <- siblings -> StepMapAt index
    | otherwise -> StepMapAmong index (mapMaybe entryKey siblings)
