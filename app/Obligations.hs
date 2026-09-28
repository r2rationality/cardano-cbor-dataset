module Obligations where

import Codec.CBOR.Cuddle.CDDL (Name (..), Value (..), ValueVariant (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Data.ByteString qualified as BS
import Data.List (stripPrefix, (\\))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import HuddleRule (entryWidth, occurrenceBounds, totalWidth)

-- OBLIGATIONS

-- | A constraint that the generated dataset sample should satisfy.
data Obligation = Obligation
  { -- | The target rule
    obligationRule :: !Name,
    -- | The element in the rule that defines the obligation. For example the element defining the arity of an array.
    obligationPath :: ![Step],
    -- | The element in the rule that is constrained, the array itself.
    obligationSubject :: ![Step],
    -- | The kind of obligation that must be satisfied, for example violating the expected arity of an array.
    obligationKind :: !ObligationKind,
    -- | The rule this was inlined from, where it was. A rule that can never
    -- have samples of its own still states obligations, and they are measured
    -- at the sites that carry it; this is what lets those verdicts be reported
    -- back under the rule they belong to.
    obligationOrigin :: !(Maybe Name)
  }
  deriving (Eq, Show)

data ObligationKind
  = -- | Fewer entries than the lower bound allows.
    TooFew !Word64
  | -- | More entries than the upper bound allows.
    TooMany !Word64
  | -- | A length the container may have. One per permitted length, so that a
    -- container admitting several is exercised at each. A container of fixed
    -- length states none: reaching it in a valid sample forces the length, so
    -- the two length forms already record that it was reached.
    AcceptArity !Int
  | -- | A map entry that is not optional: it has to be carried, and omitting
    -- it has to fail.
    AcceptRequiredKey !LiteralValue
  | MissingRequiredKey !LiteralValue
  | -- | A value whose size the control admits, and one it does not. @.size@ on
    -- bytes or text bounds the length; both ends are carried so an exact size
    -- is just the degenerate range.
    AcceptSize !Integer !Integer
  | ViolateSizeBelow !Integer
  | ViolateSizeAbove !Integer
  | -- | A byte string whose contents decode as CBOR, and one that does not.
    AcceptCbor
  | ViolateCbor
  | ViolateLowBound !Integer
  | -- | @uint .size 4@ is not a length but a ceiling: the value has to fit in
    -- four bytes. It is carried as the largest value that does.
    ViolateHighBound !Integer
  | -- | An operator we do not classify. Carried as text so an unrecognized
    -- control is reported rather than dropped, and so nothing here depends on
    -- @CtlOp@'s constructor names.
    ViolateControl !Text
  | -- | Some sample takes this alternative of a choice. Nothing else records
    -- that a branch was reached at all.
    AcceptBranch
  | -- | A value matching none of the alternatives, which has to be refused.
    -- The shapes are carried so the matcher can tell whether a sample fits any
    -- of them; where they cannot discriminate, it says so rather than guessing.
    NoBranchMatches ![Shape]
  | -- | An array whose elements have these shapes, which has to decode.
    --
    -- Stated where alternatives overlap. Two branches admitting @[coin, nil]@
    -- and @[nil, coin]@ cannot be told apart by a sample of @[coin, coin]@,
    -- so nothing per branch can ask for the three combinations; what they
    -- jointly admit is exact, and it is asked for here instead.
    AcceptElements ![Shape]
  | -- | An array of the right width whose elements match none of the
    -- combinations any alternative admits, which has to be refused.
    -- 'NoBranchMatches' cannot stand for this: @[nil, nil]@ is an array, so it
    -- matches the branch shapes and fulfils nothing.
    NoElementsMatch ![[Shape]]
  | -- | An array each of whose elements is admitted at its own position, in a
    -- combination no alternative admits.
    --
    -- The near miss 'NoElementsMatch' lets through. Any wrongly typed array of
    -- the width fulfils that one, so @[bytes, bytes]@ answers for it and the
    -- case worth generating is never asked for. Naming the combination asks for
    -- it: a decoder that checks each element on its own and forgets that the
    -- alternatives constrain them jointly fails only here.
    WrongCombination ![Shape]
  | -- | A value that is tagged, but with none of the tags the alternatives
    -- name. The near miss: right shape, wrong number, which is what catches a
    -- decoder that never looks at the tag. 'NoBranchMatches' cannot stand for
    -- this, since any value at all fulfils that one.
    NoTagMatches ![Word64]
  | -- | An array opening with a value that discriminates none of the
    -- alternatives: the same near miss for a union written as arrays rather
    -- than as tags.
    NoDiscriminantMatches ![Word64]
  | -- | A map carrying the same key twice, which has to be refused.
    --
    -- Not derivable from the entries, because a CDDL map is a set of pairs and
    -- a repeated key is not expressible in it at all. The encoding can still
    -- carry one, so every map owes this whatever its entries say, and nothing
    -- else in the tree would ever ask for it.
    DuplicateKey
  | -- | A map carrying a key none of the entries name. Only stated where every
    -- entry is keyed by a literal: a map with an open entry, such as
    -- @* 3 .. 255 => x@, admits keys nothing here lists.
    NoKeyMatches ![LiteralValue]
  | -- | A value sitting exactly on a bound, which has to decode.
    --
    -- A bound is otherwise fulfilled by anything it admits, so a @coin@ of
    -- five answers for it as well as the largest one would. A decoder that narrows a value on the way in, a @uint@ read into a
    -- signed sixty four bit integer say, is wrong only at the end of the range,
    -- and nothing else asks to go there.
    AcceptBoundary !Integer
  | -- | A value strictly between the ends of a range, which has to decode.
    --
    -- Only where the range has an interior. Sitting on an end is what the pair
    -- of boundaries asks for, so stating it again here would be answered by the
    -- very samples those already demand.
    InsideRange !Integer !Integer
  | -- | A value below the lower bound of the range
    BelowRange !Integer
  | -- | A value above the upper bound of the range
    AboveRange !Integer
  | -- | A valid value at a reference site that may never be reached. Where the
    -- site is always reached, any valid sample carries one and stating it says
    -- nothing; where the entry is optional, nothing else asks whether the
    -- corpus ever exercises it.
    AcceptReference !Name
  | -- | A value at a reference site that is not a valid value of the rule
    -- named there. The referenced rule states its own constraints, but it
    -- usually has no corpus directory of its own, so nothing would ever try to
    -- fulfil them. There is no accepted counterpart: a specification-valid
    -- sample has valid sub-terms everywhere by construction, so asking for one
    -- would state nothing.
    WrongReference !Name
  | -- | The primitive type the specification names, and a value of another.
    -- Nothing else states this: a size control says how long a value is and
    -- never what it is, so without this a 32 character text would answer for a
    -- 32 byte hash.
    AcceptType !Primitive
  | WrongType !Primitive
  | -- | The literal the specification names, and any other value.
    AcceptLiteral !LiteralValue
  | WrongLiteral !LiteralValue
  | -- | The tag the rule names, and any other tag.
    AcceptTag !Word64
  | WrongTag !Word64
  | -- | This container must appear in definite form in some valid sample.
    AcceptDefinite
  | -- | And in indefinite form in some other one.
    AcceptIndefinite
  | -- | The rule has a custom validator that must succeed
    ValidatorSucceeds
  | -- | The rule has a custom validator that must fail
    ValidatorFails
  deriving (Eq, Show)

data Category = Accept | Reject deriving (Eq, Show)

obligationCategory :: ObligationKind -> Category
obligationCategory = \case
  TooFew _ -> Reject
  TooMany _ -> Reject
  AcceptArity _ -> Accept
  AcceptRequiredKey _ -> Accept
  MissingRequiredKey _ -> Reject
  AcceptSize _ _ -> Accept
  ViolateSizeBelow _ -> Reject
  ViolateSizeAbove _ -> Reject
  AcceptCbor -> Accept
  ViolateCbor -> Reject
  ViolateLowBound _ -> Reject
  ViolateHighBound _ -> Reject
  ViolateControl _ -> Reject
  AcceptBranch -> Accept
  NoBranchMatches _ -> Reject
  AcceptElements _ -> Accept
  NoElementsMatch _ -> Reject
  WrongCombination _ -> Reject
  NoTagMatches _ -> Reject
  NoDiscriminantMatches _ -> Reject
  NoKeyMatches _ -> Reject
  DuplicateKey -> Reject
  AcceptBoundary _ -> Accept
  InsideRange _ _ -> Accept
  BelowRange _ -> Reject
  AboveRange _ -> Reject
  AcceptReference _ -> Accept
  WrongReference _ -> Reject
  AcceptType _ -> Accept
  WrongType _ -> Reject
  AcceptLiteral _ -> Accept
  WrongLiteral _ -> Reject
  AcceptTag _ -> Accept
  WrongTag _ -> Reject
  AcceptDefinite -> Accept
  AcceptIndefinite -> Accept
  ValidatorSucceeds -> Accept
  ValidatorFails -> Reject

-- | Where in a rule an obligation sits
data Step
  = StepChoice !Int !Shape
  | StepArray !Int
  | StepMapKey !LiteralValue
  | StepMapAt !Int
  | -- | An entry whose key is open sitting among literal-keyed siblings. There
    -- is no way to pick it out of a decoded map: its key matches nothing in
    -- particular, and its position in the schema is not its position in the
    -- bytes.
    StepMapAmong !Int
  | StepGroup !Int
  | StepKey
  | StepValue
  | -- | An occurrence, carrying its lower bound: zero means whatever sits
    -- below it may never appear in any sample at all.
    StepOccur !Word64
  | StepTag !Word64
  | StepControl
  | StepGenerator
  | StepValidator
  deriving (Eq, Show, Ord)

-- | A CBOR primitive type, as the specification's postlude names it.
--
-- Named here rather than by importing cuddle's own type, for the same reason
-- the control operator is carried as text: nothing should break when a
-- constructor is renamed upstream.
data Primitive
  = PrimUInt
  | PrimNInt
  | PrimInt
  | PrimBytes
  | PrimText
  | PrimBool
  | PrimNil
  | -- | The two container major types. No postlude names them, but a container
    -- node has to state what it is: without this an array rule is satisfied by
    -- a map, since its length and its two length forms say nothing about which
    -- kind of container it is.
    PrimArray
  | PrimMap
  deriving (Eq, Show, Ord)

primitiveNamed :: Text -> Maybe Primitive
primitiveNamed = \case
  "PTUInt" -> Just PrimUInt
  "PTNInt" -> Just PrimNInt
  "PTInt" -> Just PrimInt
  "PTBytes" -> Just PrimBytes
  "PTText" -> Just PrimText
  "PTBool" -> Just PrimBool
  "PTNil" -> Just PrimNil
  _ -> Nothing

renderPrimitive :: Primitive -> Text
renderPrimitive = \case
  PrimUInt -> "uint"
  PrimNInt -> "nint"
  PrimInt -> "int"
  PrimBytes -> "bytes"
  PrimText -> "text"
  PrimBool -> "bool"
  PrimNil -> "nil"
  PrimArray -> "array"
  PrimMap -> "map"

-- | One element-shape combination, as it reads in an obligation.
renderShapes :: [Shape] -> Text
renderShapes shapes = "(" <> T.intercalate ", " (map renderShape shapes) <> ")"

renderShape :: Shape -> Text
renderShape = \case
  ShapeArray Nothing -> "array"
  ShapeArray (Just discriminant) -> "array opening with " <> show discriminant
  ShapeMap -> "map"
  ShapeTag number -> "tag " <> show number
  ShapePrimitive primitive -> renderPrimitive primitive
  ShapeOther -> "any value"
  ShapeAmbiguous -> "an indistinguishable alternative"

-- | A literal value the specification names.
--
-- Kept as a value rather than as its rendering so it can be compared against a
-- decoded term exactly: the uint @13@ and the text @\"13\"@ are different
-- literals and must not answer for each other.
data LiteralValue
  = LitUInt !Word64
  | LitNInt !Word64
  | LitBignum !Integer
  | LitText !Text
  | LitBytes !ByteString
  | LitBool !Bool
  | LitFloat !Double
  deriving (Eq, Show, Ord)

renderLiteral :: LiteralValue -> Text
renderLiteral = \case
  LitUInt number -> show number
  LitNInt number -> "-" <> show number
  LitBignum number -> show number
  LitText text -> show text
  LitBytes bytes -> "bytes of " <> show (BS.length bytes)
  LitBool value -> show value
  LitFloat value -> show value

literalValue :: CTree MonoReferenced -> Maybe LiteralValue
literalValue = \case
  CTree.Literal (Value variant _) -> Just (fromVariant variant)
  _ -> Nothing

fromVariant :: ValueVariant -> LiteralValue
fromVariant = \case
  VUInt number -> LitUInt number
  VNInt number -> LitNInt number
  VBignum number -> LitBignum number
  VText text -> LitText text
  VBytes bytes -> LitBytes bytes
  VBool value -> LitBool value
  VFloat16 value -> LitFloat (realToFrac value)
  VFloat32 value -> LitFloat (realToFrac value)
  VFloat64 value -> LitFloat value

-- | The literal key of a map entry, looking through an optionality wrapper:
-- @? 13 : x@ is an occurrence around a keyed entry, not a different shape.
entryKey :: CTree MonoReferenced -> Maybe LiteralValue
entryKey = \case
  CTree.KV key _ _ -> literalValue key
  CTree.Occur inner _ -> entryKey inner
  _ -> Nothing

-- | Enough of a choice alternative's shape to tell which branch a term took.
--
-- The generator picks a branch and the encoding keeps no record of which, so
-- navigation has to infer it. A container's major type settles it whenever the
-- alternatives differ in kind, which is the common case; two alternatives of
-- the same kind stay ambiguous and the validation trace is what would lift that.
data Shape
  = -- | An array, carrying the leading @uint@ when the alternative starts with
    -- one. Cardano's unions are all discriminated that way, and without it two
    -- array alternatives are indistinguishable: a two-element @drep@ would
    -- answer for the one-element branch as readily as its own.
    ShapeArray !(Maybe Word64)
  | ShapeMap
  | ShapeTag !Word64
  | -- | A branch that is a bare primitive. @anchor \/ nil@ is told apart by
    -- this and nothing else, and without it the nil branch resolves for a
    -- sample that took the other one.
    ShapePrimitive !Primitive
  | ShapeOther
  | -- | A branch sharing its shape with a sibling. Nothing in a decoded term
    -- says which of the two a sample took, so anything stated under it can only
    -- be reported as undecided.
    ShapeAmbiguous
  deriving (Eq, Show, Ord)

-- | The shape of one choice alternative, looking through the wrappers that do
-- not change the encoding.
-- | The shapes an array admits at each of its positions, where every entry
-- contributes exactly one element.
--
-- 'Nothing' where it does not. A group splices an unknown number of entries in
-- and a repeated entry has no fixed position, so neither has a position to
-- speak about, and guessing one would state an obligation about the wrong
-- element.
arraySignature :: Map Name (CTree MonoReferenced) -> CTree MonoReferenced -> Maybe [[Shape]]
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

-- | Every combination the alternatives admit somewhere, position by position.
--
-- Wider than what they admit as whole combinations, and the difference is the
-- interesting part: an element that is fine where it sits, in company no
-- alternative allows.
byPosition :: [[[Shape]]] -> [[Shape]]
byPosition signatures = sequence (map (ordNub . concat) (transpose signatures))

-- | The values at the ends of what a numeric type admits.
--
-- 'Nothing' for everything else: a boundary is only meaningful where the values
-- are ordered, and a bytes or text one is already stated as a size.
widestOf :: Primitive -> Maybe (Integer, Integer)
widestOf = \case
  PrimUInt -> Just (0, maxWord)
  PrimNInt -> Just (negate (maxWord + 1), -1)
  PrimInt -> Just (negate (maxWord + 1), maxWord)
  _ -> Nothing
  where
    maxWord = toInteger (maxBound :: Word64)

-- | Rules whose obligations are also stated wherever they are referenced.
--
-- A rule normally states its obligations once, under its own name, and is
-- measured against its own samples. These few can never have samples of their
-- own: @signkey_kes@ is secret key material no decoder reads, @vrf_cert@ is a
-- bare pair with no type behind it, @kes_signature@ is only ever decoded inside
-- a header, and @script_ref@ inside an output. The only place they can be
-- measured is inside whatever carries them, so their obligations are repeated
-- there with the carrier's path in front.
--
-- Kept as a written list rather than derived from the corpus: what a rule owes
-- should not change when a directory appears beside it. None of these is
-- recursive, so the walk still terminates.
inlinedRules :: [Text]
inlinedRules = ["kes_signature", "script_ref", "vrf_cert", "signkey_kes"]

-- | How many admitted combinations are worth listing. Past this the overlap is
-- wide enough that every combination is a separate sample to generate, and the
-- obligation set would grow faster than a corpus could answer it.
widestUsefulCombination :: Int
widestUsefulCombination = 12

shapeOf :: Map Name (CTree MonoReferenced) -> CTree MonoReferenced -> Shape
shapeOf rules = go mempty
  where
    go seen = \case
      CTree.Array entries -> ShapeArray (leadingUInt rules entries)
      CTree.Map _ -> ShapeMap
      CTree.Tag number _ -> ShapeTag number
      CTree.Postlude term -> maybe ShapeOther ShapePrimitive (primitiveNamed (show term))
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

-- | The discriminant an alternative opens with, looking through the group a
-- @//@ alternative is spliced in as.
leadingUInt :: Map Name (CTree MonoReferenced) -> [CTree MonoReferenced] -> Maybe Word64
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

-- | The bounds a control's argument states, as a closed range. An exact size is
-- the range that starts and ends in the same place.
controllerBounds :: CTree MonoReferenced -> Maybe (Integer, Integer)
controllerBounds = \case
  CTree.Literal (Value (VUInt number) _) -> Just (toInteger number, toInteger number)
  CTree.Range from to _ -> (,) <$> literalUInt from <*> literalUInt to
  _ -> Nothing

literalUInt :: CTree MonoReferenced -> Maybe Integer
literalUInt = \case
  CTree.Literal (Value (VUInt number) _) -> Just (toInteger number)
  _ -> Nothing

-- | The number a range endpoint names, following the reference when the bound
-- is a rule of its own: @positive_int = 1 .. max_word64@ names one both ways.
--
-- A negative literal is stored as its magnitude, which @min_int64@ settles: it
-- resolves to @VNInt 9223372036854775808@.
literalInteger :: Map Name (CTree MonoReferenced) -> CTree MonoReferenced -> Maybe Integer
literalInteger rules = \case
  CTree.Literal (Value variant _) -> fromVariantInteger variant
  CTree.CTreeE (MRuleRef ref) -> case Map.lookup ref rules of
    Just (CTree.Literal (Value variant _)) -> fromVariantInteger variant
    _ -> Nothing
  _ -> Nothing

fromVariantInteger :: ValueVariant -> Maybe Integer
fromVariantInteger = \case
  VUInt number -> Just (toInteger number)
  VNInt number -> Just (negate (toInteger number))
  VBignum number -> Just number
  _ -> Nothing

renderStep :: Step -> Text
renderStep = \case
  StepChoice index _ -> "choice[" <> show index <> "]"
  StepArray index -> "array[" <> show index <> "]"
  StepMapKey key -> "map{" <> renderLiteral key <> "}"
  StepMapAt index -> "map[" <> show index <> "]"
  StepMapAmong index -> "map[" <> show index <> "?]"
  StepGroup index -> "group[" <> show index <> "]"
  StepKey -> "key"
  StepValue -> "value"
  StepOccur _ -> "occur"
  StepTag number -> "tag" <> show number
  StepControl -> "control"
  StepGenerator -> "gen"
  StepValidator -> "validator"

-- | How many distinct lengths a container may have before its length stops
-- being the interesting thing about it.
widestUsefulRange :: Int
widestUsefulRange = 4

renderBounds :: Integer -> Integer -> Text
renderBounds low high
  | low == high = "of exactly " <> show low
  | otherwise = "from " <> show low <> " to " <> show high

renderPath :: [Step] -> Text
renderPath [] = "."
renderPath steps = T.intercalate "." (map renderStep steps)

-- | Obligations stated by one rule.
obligationsFor :: CTreeRoot MonoReferenced -> Name -> [Obligation]
obligationsFor (CTreeRoot rules) name = maybe [] (go []) (Map.lookup name rules)
  where
    -- Stated and subject coincide everywhere except an occurrence bound.
    at path kind = [Obligation name (reverse path) (reverse path) kind Nothing]
    -- Whether an accepted length already asks for this optional entry.
    --
    -- Reaching a length that only this entry can supply means carrying it, and
    -- a specification-valid sample has valid sub-terms throughout, so asking
    -- again for a valid value there states nothing. Only where the entry is the
    -- only one the length can vary on: with two of them a length says nothing
    -- about which was taken.
    impliedByArity entries path obligation = case obligationKind obligation of
      AcceptReference _ ->
        lengthVariesOnOneEntry entries
          && statesALength entries
          && maybe False directlyUnderAnEntry (stripPrefix (reverse path) (obligationPath obligation))
      _ -> False

    lengthVariesOnOneEntry entries = length (filter (not . fixedWidth) entries) == 1

    fixedWidth entry = case entryWidth rules entry of
      Just (low, high) -> Just low == high
      Nothing -> False

    statesALength entries = any (statesAnArity . obligationKind) (containerLength [] entries)
      where
        statesAnArity = \case
          AcceptArity _ -> True
          _ -> False

    directlyUnderAnEntry = \case
      StepArray _ : rest -> all staysOnTheEntry rest
      _ -> False

    -- A control already states the ceiling it allows, and the type's own is
    -- above it. Asking for the type's would ask for a value the specification
    -- refuses, so under one the type states no boundary of its own.
    boundedHere = \case
      StepControl : _ -> True
      _ -> False

    -- What a repeated entry owes at a position past the first, which is only
    -- what the entry says directly: restating a whole subtree at a second
    -- position would catch nothing the first does not.
    --
    -- Only where the entry is the container's sole one, because then every
    -- element a sample carries is that entry and the second position is simply
    -- its second occurrence. Beside other entries it would not be: @cost_models@
    -- ends with an open key, and a sample's second key may as well be the
    -- literal 1 that the entry before it names.
    repeatedTail siblings path step entry
      | siblings == 1, repeats entry = [o | o <- go (step : path) entry, statedHere o]
      | otherwise = []
      where
        statedHere obligation =
          maybe False (all staysOnTheEntry) $
            stripPrefix (reverse (step : path)) (obligationPath obligation)

    repeats entry = case entryWidth rules entry of
      Just (_, Nothing) -> True
      _ -> False

    staysOnTheEntry = \case
      StepOccur _ -> True
      StepKey -> True
      StepValue -> True
      StepControl -> True
      StepGenerator -> True
      StepValidator -> True
      _ -> False

    indexed path step entries =
      concat [go (step index : path) entry | (index, entry) <- zip [0 ..] entries]

    -- How long the container may be, from the sum of what its entries
    -- contribute. Three obligations where both ends are known: a length the
    -- container may have, and one either side of it. A fixed length is the
    -- degenerate case where the two ends coincide.
    --
    -- This is the only thing that bounds a container. An occurrence indicator
    -- bounds the entry it sits on, and reading it as a bound on the whole
    -- container is wrong wherever the container has other entries too.
    containerLength path entries = case totalWidth rules entries of
      Nothing -> []
      Just (low, high) ->
        [ obligation
        | Just top <- [high],
          let lengths = [low .. top],
          -- A fixed length is forced, so stating it says nothing the length
          -- forms do not. A wide range is not about the length at all: a map
          -- with thirty optional keys is interesting for which ones it carries.
          length lengths > 1,
          length lengths <= widestUsefulRange,
          size <- lengths,
          obligation <- at path (AcceptArity (fromIntegral size))
        ]
          <> [obligation | low > 0, obligation <- at path (TooFew low)]
          <> [obligation | Just top <- [high], obligation <- at path (TooMany top)]
          <> [ obligation
             | size <- maybeToList (repeatedWidth entries),
               obligation <- at path (AcceptArity (fromIntegral size))
             ]

    -- One more than the container holds with every open entry appearing just
    -- once, which is the smallest size that cannot be reached without repeating
    -- one of them.
    --
    -- Nothing else asks for the repetition. @{+ reward_account => coin}@ is
    -- fulfilled by a map of one entry, and @cost_models@ by a map of the four
    -- its specification lists, so a decoder that stops after the first open key
    -- would never be caught. 'Nothing' where no entry is open, since then the
    -- lengths are bounded and already stated.
    repeatedWidth entries = do
      widths <- traverse (entryWidth rules) entries
      guard (any (isNothing . snd) widths)
      pure (sum [fromMaybe (max 1 low) high | (low, high) <- widths] + 1)

    -- A map entry not wrapped in an occurrence is required, so leaving it out
    -- must fail. Only a literal key can be named, which covers every key that
    -- matters here: the ledger's maps are keyed by small uints.
    requiredKey path (index, entry) = case entry of
      CTree.KV _ _ _
        | Just key <- entryKey entry ->
            [ Obligation
                name
                (reverse (StepKey : mapStep 1 index entry : path))
                (reverse path)
                (AcceptRequiredKey key)
                Nothing
            ]
              <> [ Obligation
                     name
                     (reverse (StepKey : mapStep 1 index entry : path))
                     (reverse path)
                     (MissingRequiredKey key)
                     Nothing
                 ]
      _ -> []

    -- What a control asks for, when we recognize the operator. An operator we
    -- do not know still states something, so it falls through to the textual
    -- kind rather than being dropped.
    controlObligations path op target controller
      | operator == "Cbor" = at path AcceptCbor <> at path ViolateCbor
      | operator == "Size",
        boundsAValue target,
        Just (_, high) <- bounds =
          highBound (256 ^ high - 1)
      | operator == "Size",
        Just (low, high) <- bounds =
          at path (AcceptSize low high)
            -- Nothing is shorter than nothing, so a lower bound of zero states
            -- no way of falling under it.
            <> [obligation | low > 0, obligation <- at path (ViolateSizeBelow low)]
            <> at path (ViolateSizeAbove high)
      | operator == "Le", Just (_, high) <- bounds = highBound high
      | otherwise = at path (ViolateControl operator)
      where
        operator = show op :: Text
        bounds = controllerBounds controller
        -- @uint .size 4@ bounds the value, @bytes .size 4@ bounds the length.
        -- The postlude term is compared as text for the same reason the
        -- operator is: nothing here should break when cuddle renames a
        -- constructor.
        boundsAValue = \case
          CTree.Postlude term -> show term `elem` (["PTUInt", "PTNInt", "PTInt"] :: [Text])
          _ -> False
        highBound limit = at path (AcceptBoundary limit) <> at path (ViolateHighBound limit)

    -- Any accepted value on the branch is already a sample that took it, so
    -- where one sits on the branch itself, or under nothing but tags, saying
    -- the branch was reached adds nothing it does not already say and could
    -- never fail on its own. A container's definite and indefinite pair is the
    -- common case; a branch that is a bare @nil@ states its type instead, and
    -- that answers just as well.
    branchObligations here below
      | any (reachedBy here) below = below
      | otherwise = at here AcceptBranch <> below

    reachedBy here obligation =
      obligationKind obligation /= AcceptBranch
        && obligationCategory (obligationKind obligation) == Accept
        && reachesWithoutDescending (stripPrefix (reverse here) (obligationPath obligation))

    -- Steps that land on the same value the branch does. Reaching what is
    -- stated beyond one of them is reaching the branch; a step into a
    -- container or past an occurrence is not, since the sub-term it finds may
    -- be absent or may be one of several.
    reachesWithoutDescending = \case
      Just steps -> all staysOnTheValue steps
      Nothing -> False

    staysOnTheValue = \case
      StepTag _ -> True
      StepControl -> True
      StepGenerator -> True
      StepValidator -> True
      _ -> False

    -- An entry whose occurrence admits none of it may never appear in a
    -- sample, so whether the corpus reaches it at all is an open question.
    mayGoUnreached = any $ \case
      StepOccur 0 -> True
      _ -> False

    -- A rule in the inlined set states its obligations here, at the site that
    -- carries it, as well as under its own name.
    inlinedHere ref = unName ref `elem` inlinedRules

    splicesEntries ref = case Map.lookup ref rules of
      Just (CTree.Group _) -> True
      _ -> False

    underAnOpenBranch = any $ \case
      StepChoice _ ShapeOther -> True
      StepChoice _ ShapeAmbiguous -> True
      _ -> False

    -- Navigation picks a choice branch by what the term looks like, so a value
    -- the branch was picked by is not open: asking a sample to carry it repeats
    -- the branch obligation, and asking for anything else is unsatisfiable,
    -- because navigation refused every other value on the way in.
    choiceChoseTag number = \case
      StepChoice _ (ShapeTag chosen) : _ -> chosen == number
      _ -> False

    choiceChoseValue value path = case (value, span leadingPosition path) of
      (LitUInt number, (steps, StepChoice _ (ShapeArray (Just chosen)) : _)) ->
        not (null steps) && number == chosen
      _ -> False

    leadingPosition = \case
      StepArray 0 -> True
      StepGroup 0 -> True
      _ -> False

    -- A member key is worth stating only when the path did not already use it
    -- to find the entry. A keyed entry is found by its key, so asking for a key
    -- other than the one just looked up is unsatisfiable, and asking for the
    -- one just looked up is vacuous; what that entry really owes is the
    -- required-key pair, stated at the map. Under an array or a group the key
    -- is a label that never reaches the bytes at all. A repeated entry is found
    -- by position, so there its key is genuinely open.
    keyIsEncoded path = case dropWhile (not . isContainerStep) path of
      StepMapAt _ : _ -> True
      StepMapAmong _ : _ -> True
      _ -> False

    isContainerStep = \case
      StepMapKey _ -> True
      StepMapAt _ -> True
      StepMapAmong _ -> True
      StepArray _ -> True
      StepGroup _ -> True
      _ -> False

    -- A keyed entry is found in a sample by its key. Its position in the
    -- schema is the order the specification lists it in, which has nothing to
    -- do with where it lands among whichever fields a sample happens to carry.
    -- A repeated entry has no single position either way.
    -- An open key can be followed only when the schema map has nothing else in
    -- it, because then every entry a sample carries belongs to that one entry.
    -- Beside literal-keyed siblings it is not locatable at all.
    mapStep :: Int -> Int -> CTree MonoReferenced -> Step
    mapStep siblings index entry =
      maybe (if siblings == 1 then StepMapAt index else StepMapAmong index) StepMapKey (entryKey entry)

    -- What a choice admits when its alternatives overlap.
    --
    -- Alternatives sharing a shape cannot be told apart in a decoded term, so
    -- nothing stated under one of them can be answered. What they jointly
    -- admit is still exact, and saying it here, at the choice, is both
    -- checkable and the only thing that refuses a near miss: @[nil, nil]@ is an
    -- array, so every obligation about the branches lets it through.
    elementObligations :: [Step] -> [CTree MonoReferenced] -> [Shape] -> [Obligation]
    elementObligations path alternatives shapes = case overlapping of
      [] -> []
      _ -> case traverse (arraySignature rules) overlapping of
        Just signatures
          | [_] <- ordNub (map length signatures),
            let admitted = ordNub (concatMap sequence signatures),
            let excluded = ordNub (byPosition signatures) \\ admitted,
            not (null admitted),
            length admitted <= widestUsefulCombination,
            length excluded <= widestUsefulCombination ->
              concatMap (at path . AcceptElements) admitted
                <> at path (NoElementsMatch admitted)
                <> concatMap (at path . WrongCombination) excluded
        _ -> []
      where
        overlapping =
          [ alternative
          | (alternative, shape) <- zip alternatives shapes,
            length (filter (== shape) shapes) > 1
          ]

    go :: [Step] -> CTree MonoReferenced -> [Obligation]
    go path = \case
      -- Only where the sub-term really is one value of that rule. A group
      -- splices its entries in, so no single term answers to its name; and
      -- under a branch that fits any term we cannot tell we are even looking at
      -- the right alternative.
      CTree.CTreeE (MRuleRef ref)
        | splicesEntries ref -> []
        | underAnOpenBranch path -> []
        | otherwise ->
            [o | mayGoUnreached path, o <- at path (AcceptReference ref)]
              <> at path (WrongReference ref)
              <> [ o {obligationOrigin = Just ref}
                 | inlinedHere ref,
                   body <- maybeToList (Map.lookup ref rules),
                   o <- go path body
                 ]
      CTree.CTreeE (MGenerator _ inner) -> go (StepGenerator : path) inner
      CTree.CTreeE (MValidator _ inner) ->
        at path ValidatorSucceeds
          <> at path ValidatorFails
          <> go (StepValidator : path) inner
      CTree.Occur item occurs -> go (StepOccur (fst (occurrenceBounds occurs)) : path) item
      CTree.Array entries ->
        -- No accepted counterpart: one of the two length forms is fulfilled by
        -- any array that gets here, which is the same evidence.
        at path (WrongType PrimArray)
          <> at path AcceptDefinite
          <> at path AcceptIndefinite
          <> containerLength path entries
          <> [ o
             | o <-
                 indexed path StepArray entries
                   <> concatMap (repeatedTail (length entries) path (StepArray 1)) entries,
               not (impliedByArity entries path o)
             ]
      CTree.Map entries ->
        at path (WrongType PrimMap)
          <> at path AcceptDefinite
          <> at path AcceptIndefinite
          <> containerLength path entries
          <> at path DuplicateKey
          <> [ obligation
             | Just keys <- [traverse entryKey entries],
               not (null keys),
               obligation <- at path (NoKeyMatches keys)
             ]
          <> concatMap (requiredKey path) (zip [0 ..] entries)
          <> concat
            [ go (mapStep (length entries) index entry : path) entry
            | (index, entry) <- zip [0 ..] entries
            ]
          <> concatMap (repeatedTail (length entries) path (StepMapAt 1)) entries
      CTree.Group entries -> indexed path StepGroup entries
      CTree.KV key value _ ->
        [obligation | keyIsEncoded path, obligation <- go (StepKey : path) key]
          <> go (StepValue : path) value
      CTree.Control op target controller ->
        -- The controller says what the control is, so @.size 32@ makes 32 part
        -- of the constraint rather than a value a sample could differ from.
        let here = StepControl : path
         in controlObligations here op target controller <> go here target
      CTree.Range from to _ ->
        -- The endpoints are the bound, not values a sample could differ from,
        -- so they are carried here rather than walked into.
        case (,) <$> literalInteger rules from <*> literalInteger rules to of
          Just (low, high) ->
            at path (AcceptBoundary low)
              <> at path (AcceptBoundary high)
              <> [o | high - low >= 2, o <- at path (InsideRange low high)]
              <> at path (BelowRange low)
              <> at path (AboveRange high)
          Nothing -> []
      CTree.Literal (Value variant _) ->
        [ obligation
        | not (choiceChoseValue (fromVariant variant) path),
          obligation <- at path (AcceptLiteral value) <> at path (WrongLiteral value)
        ]
        where
          value = fromVariant variant
      CTree.Tag number inner ->
        [ obligation
        | not (choiceChoseTag number path),
          obligation <- at path (AcceptTag number) <> at path (WrongTag number)
        ]
          <> go (StepTag number : path) inner
      CTree.Choice alternatives ->
        at path (NoBranchMatches shapes)
          <> [o | not (null tags), o <- at path (NoTagMatches tags)]
          <> [o | not (null discriminants), o <- at path (NoDiscriminantMatches discriminants)]
          <> elementObligations path (NE.toList alternatives) shapes
          <> concat
            [ branchObligations here (go here alternative)
            | (index, alternative, shape) <- zip3 [0 ..] (NE.toList alternatives) shapes,
              let here = StepChoice index (tellsBranchApart shape) : path
            ]
        where
          shapes = map (shapeOf rules) (NE.toList alternatives)
          -- Two alternatives of the same shape cannot be told apart in a
          -- decoded term, so a path into either would resolve for a sample that
          -- took the other and answer about the wrong value. Such a branch is
          -- left opaque, which is what suppresses everything stated under it.
          tellsBranchApart shape
            | length (filter (== shape) shapes) == 1 = shape
            | otherwise = ShapeAmbiguous
          tags = [number | ShapeTag number <- shapes]
          discriminants = [number | ShapeArray (Just number) <- shapes]
      CTree.Enum inner -> go path inner
      CTree.Unwrap inner -> go path inner
      CTree.Postlude term
        | Just primitive <- primitiveNamed (show term),
          Just (low, high) <- widestOf primitive,
          not (boundedHere path) ->
            at path (AcceptType primitive)
              <> at path (WrongType primitive)
              <> at path (AcceptBoundary low)
              <> at path (AcceptBoundary high)
      CTree.Postlude term -> case primitiveNamed (show term) of
        -- A primitive we do not recognize, or one that admits anything, states
        -- nothing rather than something we cannot check.
        Nothing -> []
        Just primitive -> at path (AcceptType primitive) <> at path (WrongType primitive)

-- | Every rule's obligations, which is the whole spec exactly once.
allObligations :: CTreeRoot MonoReferenced -> [(Name, [Obligation])]
allObligations root@(CTreeRoot rules) = [(name, obligationsFor root name) | name <- Map.keys rules]

-- RENDERING

renderObligation :: Obligation -> Text
renderObligation obligation =
  T.justifyLeft 7 ' ' (renderCategory (obligationCategory kind))
    <> T.justifyLeft 45 ' ' (renderPath (obligationPath obligation))
    <> " "
    <> renderKind kind
  where
    kind = obligationKind obligation

renderCategory :: Category -> Text
renderCategory = \case
  Reject -> "reject"
  Accept -> "accept"

-- | What one obligation asks for, without the path or the category.
--
-- Kept apart from 'renderObligation' so a table can put each in its own column
-- rather than cutting a padded line back up.
renderKind :: ObligationKind -> Text
renderKind = describe
  where
    describe = \case
      TooFew lower ->
        "too few: " <> show (lower - 1) <> " entries where " <> show lower <> " is the minimum"
      TooMany upper ->
        "too many: " <> show (upper + 1) <> " entries where " <> show upper <> " is the maximum"
      AcceptArity size -> "this container with " <> show size <> " entries"
      AcceptRequiredKey key -> "the required key " <> renderLiteral key
      MissingRequiredKey key -> "omit the required key " <> renderLiteral key
      AcceptSize low high -> "a size " <> renderBounds low high
      ViolateSizeBelow low -> "a size below " <> show low
      ViolateSizeAbove high -> "a size above " <> show high
      AcceptCbor -> "bytes whose contents decode as CBOR"
      ViolateCbor -> "bytes whose contents do not decode as CBOR"
      ViolateLowBound low -> "a value below " <> show low
      ViolateHighBound high -> "a value above " <> show high
      ViolateControl op -> "violate the " <> op <> " control"
      AcceptBranch -> "a sample taking this alternative"
      NoBranchMatches shapes ->
        "a value matching none of the " <> show (length shapes) <> " alternatives"
      AcceptElements shapes -> "an array of " <> renderShapes shapes
      WrongCombination shapes ->
        "an array of " <> renderShapes shapes <> ", which no alternative admits"
      NoElementsMatch combinations ->
        "an array of "
          <> show (length (fromMaybe [] (viaNonEmpty head combinations)))
          <> " elements matching none of "
          <> T.intercalate " or " (map renderShapes combinations)
      NoTagMatches tags -> "a value tagged with none of " <> show tags
      NoDiscriminantMatches values -> "an array opening with none of " <> show values
      DuplicateKey -> "a map carrying the same key twice"
      NoKeyMatches keys ->
        "a map keyed by none of " <> T.intercalate ", " (map renderLiteral keys)
      AcceptBoundary value -> "the value " <> show value <> ", at the end of what is admitted"
      InsideRange low high -> "a value strictly between " <> show low <> " and " <> show high
      BelowRange low -> "a value below " <> show low
      AboveRange high -> "a value above " <> show high
      AcceptReference name -> "a valid " <> unName name <> " at an entry that may be absent"
      WrongReference name -> "a value that is not a valid " <> unName name
      AcceptType primitive -> "a value of type " <> renderPrimitive primitive
      WrongType primitive -> "a value that is not " <> renderPrimitive primitive
      AcceptLiteral value -> "the value " <> renderLiteral value
      WrongLiteral value -> "a value other than " <> renderLiteral value
      AcceptTag number -> "the tag " <> show number
      WrongTag number -> "a tag other than " <> show number
      AcceptDefinite -> "this container in definite form"
      AcceptIndefinite -> "this container in indefinite form"
      ValidatorSucceeds -> "a custom validator succeeds"
      ValidatorFails -> "a custom validator fails"
