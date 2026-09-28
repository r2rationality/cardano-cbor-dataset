module Coverage.Obligations.Obligations where

import Codec.CBOR.Cuddle.CDDL (Name (..), OccurrenceIndicator, Value (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.CDDL.Custom.Validator (TermValidator)
import Codec.CBOR.Cuddle.CDDL.Resolve (XXCTree (..))
import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Obligation
import Coverage.Obligations.Primitive
import Coverage.Obligations.Shape
import Coverage.Obligations.SpecRule
import Coverage.Obligations.Step
import Coverage.Obligations.Trail
import Data.List ((\\))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Universum ((...))

-- | The obligations that one rule states.
--
-- It collects all the obligations stated by the rule, starting from a Trail, that is at the root of the rule
-- then recursively explores its elements to collect all nested obligations.
-- Eventually, obligations are deduplicated to ensure each unique obligation is only listed once.
ruleObligations :: HuddleSpec -> RuleNode -> [Obligation]
ruleObligations (CTreeRoot rules) = deduplicateObligations . obligationsUnder rules startTrail

-- | The obligations of an element of a rule, and of every element inside it, at every depth.
--
-- The trail goes from the rule down to the element, and each obligation gets a trail that starts with it.
obligationsUnder :: Map Name RuleNode -> Trail Absolute -> RuleNode -> [Obligation]
obligationsUnder rules path = \case
  CTree.Array entries -> arrayObligations rules path entries <> recurseArrayObligations rules path entries
  CTree.Map entries -> mapObligations rules path entries <> recurseMapObligations rules path entries
  CTree.Group entries -> recurseGroupObligations rules path entries
  CTree.KV key value _ -> recurseKeyObligations rules path key <> recurseValueObligations rules path value
  CTree.Occur item occurs -> recurseOccurObligations rules path item occurs
  CTree.Choice alternatives -> choiceObligations rules path (NE.toList alternatives)
  CTree.Tag number inner -> tagObligations path number <> recurseTagObligations rules path number inner
  CTree.CTreeE (MRuleRef ref) -> recurseReferencedRuleObligations rules path ref
  CTree.CTreeE (MValidator validator inner) -> validatorObligations path validator <> obligationsUnder rules (descend StepValidator path) inner
  CTree.Control op target ctl -> controlObligations path op target ctl <> recurseControlObligations rules path target
  -- Recurse directly into the inner element without adding any obligations at this level
  CTree.CTreeE (MGenerator _ inner) -> obligationsUnder rules (descend StepGenerator path) inner
  CTree.Enum inner -> obligationsUnder rules path inner
  CTree.Unwrap inner -> obligationsUnder rules path inner
  -- Non-recursive cases
  CTree.Range from to _ -> foldMap (rangeObligations path) (literalRange rules from to)
  CTree.Literal (Value variant _) -> valueObligations path (fromVariant variant)
  CTree.Postlude term -> foldMap (primitiveObligations path) (primitiveNamed (show term))

-- | Create obligations for an array: type, size, serialization
arrayObligations :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Obligation]
arrayObligations rules path entries =
  at (WrongType PrimArray)
    <> containerLength rules path entries
    <> at AcceptDefinite
    <> at AcceptIndefinite
  where
    at = pure ... obligationAt path

-- | Create obligations for array elements
--    - Some entries might be spliced into the array, like groups and references to groups
--    - Remove obligations that are implied by the arity of the array.
recurseArrayObligations :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Obligation]
recurseArrayObligations rules path entries = do
  let spliced (position, entry) = obligationsUnder rules (descend position path) entry
  let keep = not . impliedByArity rules entries path

  filter keep $ foldMap spliced (splicedPositions rules entries)

-- | Create obligations for a map: types, size, serialization, duplicate keys, key obligations
mapObligations :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Obligation]
mapObligations rules path entries =
  at (WrongType PrimMap)
    <> containerLength rules path entries
    <> at AcceptDefinite
    <> at AcceptIndefinite
    <> at DuplicateKey
    <> [ obligation
       | Just keys <- [traverse entryKey entries],
         not (null keys),
         obligation <- at (NoKeyMatches keys)
       ]
    <> concatMap (requiredKey path) (zip [0 ..] entries)
  where
    at = pure ... obligationAt path

-- | Create obligations for the entries of a map.
recurseMapObligations :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Obligation]
recurseMapObligations rules path entries =
  foldMap
    ( \(index, entry) ->
        obligationsUnder rules (descend (mapStep entries index entry) path) entry
    )
    (zip [0 ..] entries)

-- | Create obligations for the entries of a group.
recurseGroupObligations :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Obligation]
recurseGroupObligations rules path entries =
  concat [obligationsUnder rules (descend (StepGroup index) path) entry | (index, entry) <- zip [0 ..] entries]

-- | Create obligations for the key of a map entry.
recurseKeyObligations :: Map Name RuleNode -> Trail Absolute -> RuleNode -> [Obligation]
recurseKeyObligations rules path key =
  [obligation | keyIsEncoded path, obligation <- obligationsUnder rules (descend StepKey path) key]

-- | Create obligations for the value of a map entry.
recurseValueObligations :: Map Name RuleNode -> Trail Absolute -> RuleNode -> [Obligation]
recurseValueObligations rules path = obligationsUnder rules (descend StepValue path)

-- | Create obligations for an item under an occurrence indicator.
recurseOccurObligations :: Map Name RuleNode -> Trail Absolute -> RuleNode -> OccurrenceIndicator -> [Obligation]
recurseOccurObligations rules path item occurs =
  obligationsUnder rules (descend step path) item
  where
    step = case occurrenceBounds occurs of
      (low, Just high) | high <= 1 -> StepOccur low
      (low, _) -> StepRepeat low

-- | The obligations of a choice, and of each of its alternatives.
--
-- At the choice itself, a term that matches no alternative must be rejected. When alternatives are told apart by a tag
-- or by the first element of an array, a term with another tag or first element must be rejected too. Each alternative
-- then states its own obligations under its own 'StepChoice' step, see 'branchObligations'. Alternatives with the same
-- shape also get the obligations of 'elementObligations'.
choiceObligations :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Obligation]
choiceObligations rules path alternatives =
  at (NoBranchMatches shapes)
    <> [o | not (null tags), o <- at (NoTagMatches tags)]
    <> [o | not (null discriminants), o <- at (NoDiscriminantMatches discriminants)]
    <> elementObligations rules path alternatives shapes
    <> sameShapeReferences
    <> excludedNumbers path shapes
    <> concat
      [ branchObligations rules here alternative (obligationsUnder rules here alternative)
      | (index, alternative, shape) <- zip3 [0 ..] alternatives shapes,
        let here = descend (StepChoice index (tellsBranchApart shapes shape)) path
      ]
  where
    at = pure ... obligationAt path
    shapes = map (shapeOf rules) alternatives
    tags = [number | ShapeTag number <- shapes]
    -- Alternatives of the same shape cannot be told apart, so nothing is stated under them. When they are all
    -- references, a value of that shape must still be a valid value of one of the rules they name.
    sameShapeReferences =
      [ obligation
      | shape <- ordNub shapes,
        let sharing = [alternative | (alternative, other) <- zip alternatives shapes, other == shape],
        length sharing > 1,
        Just names <- [traverse reference sharing],
        obligation <- at (NoReferenceMatches shape names)
      ]
    reference = \case
      CTree.CTreeE (MRuleRef ref) -> Just ref
      _ -> Nothing
    discriminants = [number | ShapeArray (Just number) <- shapes]

-- | The obligations about the elements of array alternatives that have the same shape.
--
-- Such alternatives cannot be told apart in a decoded term, so nothing stated under one of them can be checked, see
-- 'tellsBranchApart'. But what they admit together is still exact. For example, with @[coin, nil] / [nil, coin]@:
--
-- * each admitted combination, @[coin, nil]@ and @[nil, coin]@, must be accepted;
-- * an array that matches no admitted combination must be rejected;
-- * each combination that is allowed position by position but not as a whole, @[coin, coin]@ and @[nil, nil]@, must
--   be rejected.
--
-- Nothing is stated when these alternatives are not all arrays of the same length, or when there are more than 12
-- combinations.
elementObligations :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Shape] -> [Obligation]
elementObligations rules path alternatives shapes = case overlapping of
  [] -> []
  _ -> case traverse (arraySignature rules) overlapping of
    Just signatures
      | [_] <- ordNub (map length signatures),
        let admitted = ordNub (concatMap sequence signatures),
        let excluded = ordNub (byPosition signatures) \\ admitted,
        not (null admitted),
        length admitted <= widestUsefulCombination,
        length excluded <= widestUsefulCombination ->
          concatMap (at . AcceptElements) admitted
            <> at (NoElementsMatch admitted)
            <> concatMap (at . WrongCombination) excluded
    _ -> []
  where
    at = pure . obligationAt path
    overlapping =
      [ alternative
      | (alternative, shape) <- zip alternatives shapes,
        length (filter (== shape) shapes) > 1
      ]
    -- Past this many combinations, each one is a separate sample to generate, and the obligations would grow faster
    -- than a corpus can answer them.
    widestUsefulCombination = 12

-- | The numbers that no alternative of a choice admits, when some alternatives are ranges or numbers.
--
-- Such an alternative is found by where a value falls, so it states no 'BelowRange' or 'AboveRange' of its own: a
-- value outside it can be a value of a sibling, see 'deniesBranchShape'. The values that no alternative admits are
-- then stated here, where every alternative is in view. For @nonzero_int64 = negative_int64 / positive_int64@:
--
-- * a value below @min_int64@ and a value above @max_int64@;
-- * the value 0, which is in the gap between the two alternatives.
--
-- 'NoBranchMatches' is not enough: any value of another type fulfils it, and none of these numbers.
--
-- Nothing is stated when an alternative can hold integers but has no known span, such as a bignum tag or a nested
-- choice. A bound at the limit of what CBOR encodes as an integer is not stated, because no integer is beyond it.
excludedNumbers :: Trail Absolute -> [Shape] -> [Obligation]
excludedNumbers path shapes = case traverse numberSpan shapes of
  Just spans
    | any isRangeOrNumber shapes,
      merged@((low, _) : _) <- mergeSpans (sort (catMaybes spans)) ->
        let high = maybe low snd (viaNonEmpty last merged)
         in [o | low > minCborInteger, o <- at (BelowRange low)]
              <> [o | high < maxCborInteger, o <- at (AboveRange high)]
              <> concat [at (InsideGap end start) | ((_, end), (start, _)) <- zip merged (drop 1 merged), start - end >= 2]
  _ -> []
  where
    at = pure ... obligationAt path
    -- The integers an alternative holds: 'Nothing' when it can hold integers with no known span, which stops the
    -- whole derivation, and 'Just Nothing' when it holds no integer at all.
    numberSpan = \case
      ShapeRange from to -> Just (Just (from, to))
      ShapeLiteral literal -> Just ((\value -> (value, value)) <$> literalNumber literal)
      ShapePrimitive primitive -> Just (primitiveRange primitive)
      ShapeTag number -> if encodingFixesPayload number then Nothing else Just Nothing
      ShapeArray _ -> Just Nothing
      ShapeMap -> Just Nothing
      _ -> Nothing
    isRangeOrNumber = \case
      ShapeRange _ _ -> True
      ShapeLiteral literal -> isJust (literalNumber literal)
      _ -> False
    literalNumber = \case
      LitUInt value -> Just (toInteger value)
      LitNInt value -> Just (negate (toInteger value))
      LitBignum value -> Just value
      _ -> Nothing
    maxCborInteger = toInteger (maxBound :: Word64)
    minCborInteger = negate (maxCborInteger + 1)

-- | Spans of integers, sorted by their start, with the spans that overlap or touch joined into one.
mergeSpans :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeSpans = \case
  (low, high) : (low', high') : rest
    | low' <= high + 1 -> mergeSpans ((low, max high high') : rest)
  numbers : rest -> numbers : mergeSpans rest
  [] -> []

-- | The obligations of one alternative of a choice, from the obligations found under it.
--
-- * An obligation that denies the shape that finds the alternative is removed, see 'deniesBranchShape'. A sample that
--   breaks it would no longer look like this alternative.
-- * 'AcceptBranch', which asks for a sample that takes the alternative, is added only when no other accepted
--   obligation already asks for one. For example, an array alternative states 'AcceptDefinite', and only a sample
--   that takes the alternative can meet it.
branchObligations :: Map Name RuleNode -> Trail Absolute -> RuleNode -> [Obligation] -> [Obligation]
branchObligations rules here alternative below
  | any (reachedBy here) kept = kept
  | otherwise = obligationAt here AcceptBranch : kept
  where
    kept = case latest here of
      Just (StepChoice _ shape) ->
        [ obligation
        | obligation <- below,
          not
            ( aboutTheBranchItself (obligationPath obligation)
                && deniesBranchShape rules (shared shape) alternative (obligationKind obligation)
            )
        ]
      _ -> below

    -- A term reaches an alternative of the same shape as a sibling by that shape too.
    shared = \case
      ShapeAmbiguous shape -> shape
      shape -> shape

    -- An obligation whose subject is the alternative itself, whether it is
    -- stated at the alternative or under steps that do not move: a control
    -- narrows what a value may be without being a different value, so
    -- @bytes .size (0..64)@ states its type one step down and still means
    -- the term the branch was found by.
    aboutTheBranchItself path =
      maybe False (all passesThrough . steps) (stepsBelowPrefix here path)

    passesThrough = \case
      StepControl -> True
      StepGenerator -> True
      StepValidator -> True
      StepOccur _ -> True
      StepRepeat _ -> True
      _ -> False

-- | Obligations related to a referenced rule that is not folded
ruleReferenceObligations :: Trail Absolute -> Name -> [Obligation]
ruleReferenceObligations path ref =
  [obligationAt path (AcceptReference ref) | mayGoUnreached path]
    <> [obligationAt path (WrongReference ref)]

-- Only where the sub-term really is one value of that rule. A group splices its entries in, and under a branch that
-- fits any term we cannot tell we are even looking at the right alternative.
recurseReferencedRuleObligations :: Map Name RuleNode -> Trail Absolute -> Name -> [Obligation]
recurseReferencedRuleObligations rules path ref
  | refersToGroup rules ref = []
  | underAnOpenBranch path = []
  -- A folded rule has no name left to refer to, so nothing here may use one: only that the entry was reached, and then what it holds.
  | foldedRule ref = foldedObligations path <> foldMap (go path) (Map.lookup ref rules)
  | otherwise =
      ruleReferenceObligations path ref
        <> [ o
           | inlinedRule ref,
             body <- maybeToList (Map.lookup ref rules),
             o <- go path body
           ]
  where
    go = obligationsUnder rules

-- | Obligations for a folded rule before we recurse into it
--   There is at least an obligation saying that it must be present
foldedObligations :: Trail Absolute -> [Obligation]
foldedObligations path = [obligationAt path AcceptPresent | mayGoUnreached path]

-- | Rules whose obligations are stated where they are referenced, in place of the reference obligations.
--
-- None of them has a corpus, because no node decodes them on their own, so a corpus would test nothing:
--
-- * A generic call like @nonempty_list<vkeywitness>@ exists for one field only. The ledger decodes it as
--   @NonEmpty (WitVKey Witness)@ and Amaru as @NonEmptyVector<Witness>@, both from inside the witness set.
-- * The three bignum rules are one type to both nodes, @Integer@ in the ledger and @BigInt@ in Amaru, so their own
--   corpora would test the same decoder three times.
-- * @required_signers@ is a named rule, but it is only one of those generic calls, referenced once.
--   @certificates@ and @proposal_procedures@ are the same kind of rule, but Conway has a corpus for each of them, so
--   they are not folded. Only the generic calls they hold, such as @%nonempty_oset<certificate>@, are folded.
-- * @transaction_index@ is a plain integer to both nodes, @Int@ in the ledger and @u16@ in Amaru, read inside the
--   block body.
--
-- The field that carries such a rule states what the rule asks for, next to the obligation that the field may be
-- absent. The element rule, such as @vkeywitness@, keeps its own obligations, because it is decoded alone.
--
-- Unlike 'inlinedRules', whose references keep their reference obligations and also repeat what the rule asks for.
-- None of these rules is recursive, so the walk still terminates.
foldedRules :: [Text]
foldedRules =
  [ "big_nint",
    "epoch_interval",
    "metadatum_label",
    "potential_languages",
    "sequence_number",
    "port",
    "transaction_index",
    "%nonempty_oset<certificate>",
    "%nonempty_oset<credential>",
    "%nonempty_oset<proposal_procedure>",
    "%nonempty_oset<sub_transaction>",
    "%nonempty_set<addr_keyhash>",
    "%nonempty_set<bootstrap_witness>",
    "%nonempty_set<native_script>",
    "%nonempty_set<plutus_data>",
    "%nonempty_set<plutus_v1_script>",
    "%nonempty_set<plutus_v2_script>",
    "%nonempty_set<plutus_v3_script>",
    "%nonempty_set<transaction_input>",
    "%nonempty_set<vkeywitness>",
    "required_signers",
    "big_uint",
    "info_action",
    "int64",
    "max_int64",
    "max_word64",
    "min_int64",
    "negative_int64",
    "positive_int",
    "positive_int64",
    "%nonempty_list<bootstrap_witness>",
    "%nonempty_list<native_script>",
    "%nonempty_list<plutus_data>",
    "%nonempty_list<vkeywitness>",
    "%set<addr_keyhash>",
    "%set<committee_cold_credential>",
    "%set<transaction_input>"
  ]

-- | Whether a rule's content belongs to the field that carries it and nowhere else.
foldedRule :: Name -> Bool
foldedRule name = unName name `elem` foldedRules

-- | Whether a rule states its obligations at the site that carries it, as well as under its own name.
inlinedRule :: Name -> Bool
inlinedRule name = unName name `elem` inlinedRules

inlinedRules :: [Text]
inlinedRules =
  [ "kes_signature",
    "redeemer_tag",
    "script_ref",
    "signkey_kes",
    "vrf_cert"
  ]

-- | Create obligations for a tag
tagObligations :: Trail Absolute -> Word64 -> [Obligation]
tagObligations path number =
  [ obligation
  | not (choiceChoseTag number path),
    obligation <-
      at (AcceptTag number)
        <> at (WrongTag number)
        -- The decoder folds a bignum that fits into a plain integer, so an untagged integer can be a valid bignum.
        <> [o | not (encodingFixesPayload number), o <- at (MissingTag number)]
  ]
  where
    at = pure ... obligationAt path

-- | Create obligations for the tagged type
--
-- When CBOR fixes the type of the payload, only its length is stated. For @big_uint = #6.2(bounded_bytes)@, the payload
-- is always a byte string, but @bounded_bytes@ still limits it to 64 bytes. The payload is read through its reference,
-- since there is no rule of its own to state it.
recurseTagObligations :: Map Name RuleNode -> Trail Absolute -> Word64 -> RuleNode -> [Obligation]
recurseTagObligations rules path number inner
  | encodingFixesPayload number = filter (isLength . obligationKind) (obligationsUnder rules here (referencedBody inner))
  | otherwise = obligationsUnder rules here inner
  where
    here = descend (StepTag number) path
    referencedBody = \case
      CTree.CTreeE (MRuleRef ref) | Just body <- Map.lookup ref rules -> body
      node -> node
    isLength = \case
      AcceptSize _ _ -> True
      ViolateSizeBelow _ -> True
      ViolateSizeAbove _ -> True
      _ -> False

-- | Create obligations for a control operator
--
-- An operator we do not know stops the derivation with an error. Its obligations are unknown, and dropping it would
-- report a rule as covered when part of it is not checked.
-- The controller says what the control is, so @.size 32@ makes 32 part of the constraint rather than a value a sample
-- could differ from.
controlObligations :: Trail Absolute -> CtlOp -> RuleNode -> RuleNode -> [Obligation]
controlObligations path op target controller
  | operator == "Cbor" =
      at AcceptCbor
        <> at ViolateCbor
        <> case controller of
          CTree.CTreeE (MRuleRef ref) -> at (WrongEmbedded ref)
          _ -> []
  | operator == "Size",
    boundsAValue target,
    Just (_, high) <- bounds =
      highBound (256 ^ high - 1)
  | operator == "Size",
    Just (low, high) <- bounds =
      at (AcceptSize low high)
        -- Nothing is shorter than nothing, so a lower bound of zero states
        -- no way of falling under it.
        <> [obligation | low > 0, obligation <- at (ViolateSizeBelow low)]
        <> at (ViolateSizeAbove high)
  | operator == "Le", Just (_, high) <- bounds = highBound high
  | otherwise = error ("unknown control operator " <> operator <> " at " <> renderTrail path)
  where
    at = pure ... obligationAt (descend StepControl path)
    operator = show op :: Text
    bounds = controllerBounds controller
    -- @uint .size 4@ bounds the value, @bytes .size 4@ bounds the length.
    -- The postlude term is compared as text for the same reason the
    -- operator is: nothing here should break when cuddle renames a
    -- constructor.
    boundsAValue = \case
      CTree.Postlude term -> show term `elem` (["PTUInt", "PTNInt", "PTInt"] :: [Text])
      _ -> False
    highBound limit = at (AcceptBoundary limit) <> at (ViolateHighBound limit)

-- | Create obligations for the controlled type
recurseControlObligations :: Map Name RuleNode -> Trail Absolute -> RuleNode -> [Obligation]
recurseControlObligations rules path = obligationsUnder rules (descend StepControl path)

-- | Create obligations for a validator
validatorObligations :: Trail Absolute -> TermValidator -> [Obligation]
validatorObligations path validator =
  [ (obligationAt path kind) {obligationValidator = Just (CustomValidator validator)}
  | kind <- [ValidatorSucceeds, ValidatorFails]
  ]

-- | Create obligations for a primitive range: its ends, the values inside and outside it, and a value that is not an
-- integer at all.
rangeObligations :: Trail Absolute -> (Integer, Integer) -> [Obligation]
rangeObligations path (low, high) =
  at (AcceptBoundary low)
    <> at (AcceptBoundary high)
    <> [o | high - low >= 2, o <- at (InsideRange low high)]
    <> at (BelowRange low)
    <> at (AboveRange high)
    <> [o | not (choiceChoseRange path), o <- at (WrongType PrimInt)]
  where
    at = pure ... obligationAt path

-- | Value obligations
valueObligations :: Trail Absolute -> LiteralValue -> [Obligation]
valueObligations path value =
  [ obligation
  | not (choiceChoseValue value path),
    obligation <- at (AcceptLiteral value) <> at (WrongLiteral value)
  ]
  where
    at = pure ... obligationAt path

-- | Create obligations for a primitive value
primitiveObligations :: Trail Absolute -> Primitive -> [Obligation]
primitiveObligations path primitive =
  at (AcceptType primitive)
    <> at (WrongType primitive)
    <> concat
      [ at (AcceptBoundary bound)
      | not (underAControl path),
        Just (low, high) <- [primitiveRange primitive],
        bound <- [low, high]
      ]
  where
    at = pure ... obligationAt path

-- * HELPERS

-- | Whether an accepted length already asks for this optional entry.
--
-- Reaching a length that only this entry can supply means carrying it, and a specification-valid sample has valid
-- sub-terms throughout, so asking again for a valid value there states nothing. Only where the entry is the only one
-- the length can vary on: with two of them a length says nothing about which was taken.
impliedByArity :: Map Name RuleNode -> [RuleNode] -> Trail Absolute -> Obligation -> Bool
impliedByArity rules entries path obligation = case obligationKind obligation of
  AcceptPresent -> onlyEntryWithALength
  AcceptReference _ -> onlyEntryWithALength
  _ -> False
  where
    onlyEntryWithALength =
      lengthVariesOnOneEntry rules entries
        && statesALength rules entries
        && maybe False directlyUnderAnEntry (stepsBelowPrefix path (obligationPath obligation))

-- | Whether a container states a length it may have.
statesALength :: Map Name RuleNode -> [RuleNode] -> Bool
statesALength rules entries = any (statesAnArity . obligationKind) (containerLength rules startTrail entries)
  where
    statesAnArity = \case
      AcceptArity _ -> True
      _ -> False

-- | The entries of a container, each with the step to its position in the bytes, not in the specification.
--
-- A group splices its entries into the container, so the entry after a group of three is three positions further on.
-- For example, a Conway certificate that carries @pool_params@ has ten elements, not two.
--
-- The positions stop at the first entry whose width is not fixed, because no entry after it has a known position. One
-- case is an exception: a single optional entry before fixed ones, such as @? bls_key@ in the Dijkstra @pool_params@.
-- The length of the array then tells whether it is present, so each entry from it on is given its position in the
-- full form, with the length of the full form.
splicedPositions :: Map Name RuleNode -> [RuleNode] -> [(Step, RuleNode)]
splicedPositions rules entries = case span fixed leaves of
  (before, absentable : after)
    | not truncated,
      entryWidth rules absentable == Just (0, Just 1),
      not (null after),
      all fixed after ->
        let full = sum (map widthOf leaves)
         in [ (if index < length before then StepArray offset else StepArrayOf offset full, entry)
            | (index, (offset, entry)) <- zip [0 :: Int ..] positioned
            ]
  -- The first entry of an open width still has a position, and the ones after it have none.
  (before, _ : _) -> [(StepArray offset, entry) | (offset, entry) <- take (length before + 1) positioned]
  (_, []) -> [(StepArray offset, entry) | (offset, entry) <- positioned]
  where
    (leaves, truncated) = flatten entries
    -- The entries with every group of a fixed width spliced in. A group of an open width is spliced too, but nothing
    -- after it has a known position, so the entries stop there.
    flatten [] = ([], False)
    flatten (entry : rest) = case splicedEntries rules entry of
      Just inner
        | fixed entry -> let (here, stopped) = flatten inner in if stopped then (here, True) else first (here <>) (flatten rest)
        | otherwise -> (fst (flatten inner), not (null rest))
      Nothing -> first (entry :) (flatten rest)
    fixed entry = case entryWidth rules entry of
      Just (low, Just high) -> low == high
      _ -> False
    widthOf entry = maybe 1 (fromIntegral . fromMaybe 1 . snd) (entryWidth rules entry)
    -- Each entry with its position, counting an optional one as present.
    positioned = zip (scanl (+) 0 (map widthOf leaves)) leaves

-- | The length obligations of a container, from the sum of the widths of its entries.
--
--  A length under the minimum and a length over the maximum must be rejected.
--  Each length in between must be accepted, when there are two to four of them.
--  A container with a repeated entry must also accept a length that repeats it.
--  An occurrence indicator bounds only its own entry, so only this sum bounds the whole container.
containerLength :: Map Name RuleNode -> Trail Absolute -> [RuleNode] -> [Obligation]
containerLength rules path entries = case totalWidth rules entries of
  Nothing -> []
  Just (low, high) ->
    [ obligation
    | Just top <- [high],
      let lengths = [low .. top],
      -- A fixed length is forced, so stating it says nothing the length forms do not. A wide range is not about the
      -- length at all: a map with thirty optional keys is interesting for which ones it carries.
      length lengths > 1,
      length lengths <= widestUsefulRange,
      size <- lengths,
      obligation <- at (AcceptArity (fromIntegral size))
    ]
      <> [obligation | low > 0, obligation <- at (TooFew low)]
      <> [obligation | Just top <- [high], obligation <- at (TooMany top)]
      <> [ obligation
         | size <- maybeToList (repeatedWidth rules entries),
           obligation <- at (AcceptArity (fromIntegral size))
         ]
  where
    at = pure ... obligationAt path
    -- how many distinct lengths a container may have before its length stops being the interesting thing about it.
    widestUsefulRange = 4

-- | The obligations of a required map entry: a valid sample has its key, and a sample without its key is rejected.
--
-- An entry is required when it has no occurrence indicator, such as @0 : inputs@ in @transaction_body@. Only an entry
-- with a literal key gets these obligations, because only a literal key can be missing. The ledger keys its maps with
-- small integers, so this covers every required entry.
requiredKey :: Trail Absolute -> (Int, RuleNode) -> [Obligation]
requiredKey path (index, entry) = case entry of
  CTree.KV {}
    | Just key <- entryKey entry ->
        [ Obligation keyPath path (AcceptRequiredKey key) Nothing,
          Obligation keyPath path (MissingRequiredKey key) Nothing
        ]
    where
      keyPath = descend StepKey (descend (mapStep [entry] index entry) path)
  _ -> []
