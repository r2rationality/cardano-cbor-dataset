{-# OPTIONS_GHC -Wno-orphans #-}

module Coverage.Obligations.Obligation where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.Custom.Validator (TermValidator)
import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Primitive
import Coverage.Obligations.Shape
import Coverage.Obligations.SpecRule
import Coverage.Obligations.Trail
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (nubBy)
import Data.Text qualified as T
import Generation.SampleType
import Text.Show qualified

-- | A constraint that the generated dataset sample should satisfy.
data Obligation = Obligation
  { -- | The element in the rule that defines the obligation. For example the element defining the arity of an array.
    obligationPath :: Trail Absolute,
    -- | The element in the rule that is constrained, the array itself.
    obligationSubject :: Trail Absolute,
    -- | The kind of obligation that must be satisfied, for example violating the expected arity of an array.
    obligationKind :: ObligationKind,
    -- | The custom validator the obligation is about, for 'ValidatorSucceeds' and 'ValidatorFails'.
    obligationValidator :: Maybe CustomValidator
  }
  deriving (Eq, Show, Generic)

-- | A custom validator of the specification, which the measurement runs on a sample.
--
-- A function has no equality and no text, so two of them are always equal and show as a placeholder. Comparing or
-- showing obligations then compares and shows their data, which is what tells two obligations apart.
newtype CustomValidator = CustomValidator TermValidator

instance Eq CustomValidator where
  _ == _ = True

instance Show CustomValidator where
  showsPrec _ _ = Text.Show.showString "<custom validator>"

data ObligationKind
  = -- | A value above the upper bound of the range.
    AboveRange Integer
  | -- | A length the container may have.
    AcceptArity Int
  | -- | A value sitting exactly on a bound, which has to decode.
    AcceptBoundary Integer
  | -- | Some sample takes this alternative of a choice.
    AcceptBranch
  | -- | A byte string whose contents decode as CBOR.
    AcceptCbor
  | -- | This container must appear in definite form in some valid sample.
    AcceptDefinite
  | -- | An array whose elements have these shapes, which has to decode.
    --
    -- Stated where alternatives overlap. Two branches admitting @[coin, nil]@ and @[nil, coin]@ cannot be told apart by
    -- a sample of @[coin, coin]@, so nothing per branch can ask for the three combinations. What they jointly admit is
    -- exact, and it is asked for here instead.
    AcceptElements [Shape]
  | -- | This container must appear in indefinite form in some other valid sample.
    AcceptIndefinite
  | -- | The literal the specification names.
    AcceptLiteral LiteralValue
  | -- | A sample in which an optional entry is present.
    AcceptPresent
  | -- | A valid value with that rule reference.
    AcceptReference Name
  | -- | A map entry with the specified key.
    AcceptRequiredKey LiteralValue
  | -- | A value whose size the .size control admits.
    AcceptSize Integer Integer
  | -- | The tag the rule names.
    AcceptTag Word64
  | -- | The primitive type the specification names.
    AcceptType Primitive
  | -- | A value below the lower bound of the range.
    BelowRange Integer
  | -- | A map carrying the same key twice, which has to be refused.
    DuplicateKey
  | -- | A value strictly between the ends of a range, which has to decode.
    InsideRange Integer Integer
  | -- | A value strictly between two alternatives of a choice that are numbers, which no alternative admits.
    InsideGap Integer Integer
  | -- | A map without the required key.
    MissingRequiredKey LiteralValue
  | -- | A value without a tag, where the rule names one.
    MissingTag Word64
  | -- | A value matching none of the alternatives, which has to be refused.
    NoBranchMatches [Shape]
  | -- | An array opening with a value that discriminates none of the alternatives.
    NoDiscriminantMatches [Word64]
  | -- | An array of the right width but whose elements match none of the given combinations.
    NoElementsMatch [[Shape]]
  | -- | A value of the shape that several alternatives share, which is a valid value of none of the rules that they
    -- name. The shape cannot tell these alternatives apart, so nothing is stated under them.
    NoReferenceMatches Shape [Name]
  | -- | A map carrying a key none of the entries name.
    NoKeyMatches [LiteralValue]
  | -- | A value that is tagged, but with none of the tags the alternatives name. 'NoBranchMatches' cannot stand for
    -- this, since any value at all fulfils that one.
    NoTagMatches [Word64]
  | -- | Fewer entries than the lower bound allows.
    TooFew Word64
  | -- | More entries than the upper bound allows.
    TooMany Word64
  | -- | The rule has a custom validator that must fail.
    ValidatorFails
  | -- | The rule has a custom validator that must succeed.
    ValidatorSucceeds
  | -- | A byte string whose contents do not decode as CBOR.
    ViolateCbor
  | -- | @uint .size 4@ is not a length but a ceiling: the value has to fit in four bytes.
    ViolateHighBound Integer
  | -- | A value above the size that the .size control admits.
    ViolateSizeAbove Integer
  | -- | A value below the size that the .size control admits.
    ViolateSizeBelow Integer
  | -- | An array each of whose elements is admitted at its own position, in a combination no alternative admits.
    WrongCombination [Shape]
  | -- | A byte string whose contents decode as CBOR, but are not a valid value of the rule that a @.cbor@ control
    -- names.
    WrongEmbedded Name
  | -- | A value other than the literal the specification names.
    WrongLiteral LiteralValue
  | -- | A value that is not a valid value of the referenced rule.
    WrongReference Name
  | -- | A tag other than the one the rule names.
    WrongTag Word64
  | -- | A value of another primitive type than the one the specification names.
    WrongType Primitive
  deriving (Eq, Show, Generic)

-- | The JSON form of a kind is how a waiver in @corpus.json@ names the obligation it waives.
instance ToJSON ObligationKind

instance FromJSON ObligationKind

-- | A rule name is written as its text.
instance ToJSON Name where
  toJSON = toJSON . unName

instance FromJSON Name where
  parseJSON = fmap Name . parseJSON

-- | Categorize an obligation as either accepted or rejected.
sampleCategoryFor :: Obligation -> SampleCategory
sampleCategoryFor = categoryFor . obligationKind

categoryFor :: ObligationKind -> SampleCategory
categoryFor = \case
  AboveRange _ -> Reject
  AcceptArity _ -> Accept
  AcceptBoundary _ -> Accept
  AcceptBranch -> Accept
  AcceptCbor -> Accept
  AcceptDefinite -> Accept
  AcceptElements _ -> Accept
  AcceptIndefinite -> Accept
  AcceptLiteral _ -> Accept
  AcceptPresent -> Accept
  AcceptReference _ -> Accept
  AcceptRequiredKey _ -> Accept
  AcceptSize _ _ -> Accept
  AcceptTag _ -> Accept
  AcceptType _ -> Accept
  BelowRange _ -> Reject
  MissingTag _ -> Reject
  DuplicateKey -> Reject
  InsideRange _ _ -> Accept
  InsideGap _ _ -> Reject
  MissingRequiredKey _ -> Reject
  NoBranchMatches _ -> Reject
  NoDiscriminantMatches _ -> Reject
  NoElementsMatch _ -> Reject
  NoReferenceMatches _ _ -> Reject
  NoKeyMatches _ -> Reject
  NoTagMatches _ -> Reject
  TooFew _ -> Reject
  TooMany _ -> Reject
  ValidatorFails -> Reject
  ValidatorSucceeds -> Accept
  ViolateCbor -> Reject
  ViolateHighBound _ -> Reject
  ViolateSizeAbove _ -> Reject
  ViolateSizeBelow _ -> Reject
  WrongCombination _ -> Reject
  WrongEmbedded _ -> Reject
  WrongLiteral _ -> Reject
  WrongReference _ -> Reject
  WrongTag _ -> Reject
  WrongType _ -> Reject

-- | Create one obligation of a rule, at the end of a trail down the rule. The obligation is stated and checked at the same
-- place, which is the case everywhere except for a required key.
obligationAt :: Trail Absolute -> ObligationKind -> Obligation
obligationAt trail kind = Obligation trail trail kind Nothing

-- | A reject obligation at a choice alternative that denies the very shape the alternative is found by.
--
-- Navigation routes a term to an alternative by its shape, so a sample meeting one of these would no longer be
-- recognizable as having taken the branch: no sample can both break the constraint and show where it belongs.
-- @gov_action@'s last alternative is @[info_action]@ and @info_action@ is the literal 6, so a non-array, or an array
-- with no room for it, carries no trace of the alternative at all. Left in, these never reach a verdict and read as
-- work someone could do.
--
-- The rule named at the alternative states the same constraints under its own name, where its corpus is reached without
-- guessing a branch.
deniesBranchShape :: Map Name RuleNode -> Shape -> RuleNode -> ObligationKind -> Bool
deniesBranchShape rules shape alternative = \case
  WrongType PrimArray -> isArray shape
  WrongType PrimMap -> shape == ShapeMap
  -- An alternative that is a bare primitive is found by being one, so a
  -- term of another type is a term of a different alternative.
  WrongType primitive -> shape == ShapePrimitive primitive
  WrongTag wanted -> shape == ShapeTag wanted
  MissingTag wanted -> shape == ShapeTag wanted
  -- An alternative that is one value is found by that value, so any sample
  -- carrying another is a sample of a different alternative. The choice
  -- refuses it at the root, where every alternative is in view at once.
  WrongLiteral wanted -> shape == ShapeLiteral wanted
  -- Dropping below one entry drops the leading uint the alternative is told
  -- apart by. A longer alternative keeps it: three entries where four are
  -- required still open with the same discriminant. An alternative with no
  -- discriminant is found by being an array at all, and an empty one still
  -- is, so there the bound stays stated.
  TooFew low -> low <= 1 && discriminated shape
  -- Denying the rule the alternative names is denying the shape itself
  -- wherever the rule is no more than that shape, so no sample can both
  -- break the rule and still be seen to have taken this branch.
  WrongReference _ -> shapeIsWholeRule rules alternative
  -- An alternative that is a span is found by a value falling inside it, so
  -- a value outside the span is a value of some other alternative. What no
  -- alternative admits is stated once at the choice, where every span is in
  -- view, and not again under each of them.
  BelowRange low -> spans low shape
  AboveRange high -> spans high shape
  _ -> False
  where
    isArray = \case
      ShapeArray _ -> True
      _ -> False
    discriminated = \case
      ShapeArray (Just _) -> True
      _ -> False
    spans bound = \case
      ShapeRange low high -> low == bound || high == bound
      _ -> False

-- | Deduplicate obligations
--
-- Two obligations with the same path and the same kind ask the same question: one sample answers both, with the same
-- verdict. This happens when a folded rule reaches another folded rule. For example, @certificates@ reaches
-- @nonempty_oset<certificate>@, and both state that the entry carrying them can be absent. Counting both would make the
-- rule look as if it owed more than it does.
deduplicateObligations :: [Obligation] -> [Obligation]
deduplicateObligations = nubBy (\left right -> stated left == stated right)
  where
    stated obligation = (obligationPath obligation, obligationKind obligation)

-- | Whether an accepted obligation, other than the branch itself, is stated on the value that a branch at this path
-- finds, so that a sample answering it also took the branch.
reachedBy :: Trail Absolute -> Obligation -> Bool
reachedBy here obligation =
  obligationKind obligation /= AcceptBranch
    && sampleCategoryFor obligation == Accept
    && maybe False reachesWithoutDescending (stepsBelowPrefix here (obligationPath obligation))

-- * RENDERING

renderObligation :: Obligation -> Text
renderObligation obligation =
  T.justifyLeft 7 ' ' (renderSampleCategory (sampleCategoryFor obligation))
    <> T.justifyLeft 45 ' ' (renderTrail (obligationPath obligation))
    <> " "
    <> renderKind kind
  where
    kind = obligationKind obligation

-- | What one obligation asks for, without the path or the category.
--
-- Kept apart from 'renderObligation' so a table can put each in its own column rather than cutting a padded line back
-- up.
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
      WrongEmbedded name -> "bytes whose contents are not a valid " <> unName name
      ViolateHighBound high -> "a value above " <> show high
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
      NoReferenceMatches shape names ->
        "a value of shape " <> writeShape shape <> " that is a valid value of none of " <> T.intercalate ", " (map unName names)
      NoDiscriminantMatches values -> "an array opening with none of " <> show values
      DuplicateKey -> "a map carrying the same key twice"
      NoKeyMatches keys ->
        "a map keyed by none of " <> T.intercalate ", " (map renderLiteral keys)
      AcceptBoundary value -> "the value " <> show value <> ", at the end of what is admitted"
      InsideRange low high -> "a value strictly between " <> show low <> " and " <> show high
      InsideGap low high -> "a value strictly between " <> show low <> " and " <> show high <> ", which no alternative admits"
      BelowRange low -> "a value below " <> show low
      AboveRange high -> "a value above " <> show high
      AcceptReference name -> "a valid " <> unName name <> " at an entry that may be absent"
      AcceptPresent -> "a sample carrying this entry, which may be absent"
      WrongReference name -> "a value that is not a valid " <> unName name
      AcceptType primitive -> "a value of type " <> renderPrimitive primitive
      WrongType primitive -> "a value that is not " <> renderPrimitive primitive
      AcceptLiteral value -> "the value " <> renderLiteral value
      WrongLiteral value -> "a value other than " <> renderLiteral value
      AcceptTag number -> "the tag " <> show number
      WrongTag number -> "a tag other than " <> show number
      MissingTag number -> "a value without the tag " <> show number
      AcceptDefinite -> "this container in definite form"
      AcceptIndefinite -> "this container in indefinite form"
      ValidatorSucceeds -> "a custom validator succeeds"
      ValidatorFails -> "a custom validator fails"

-- | What an obligation asks for, in words. It names an obligation in a measurement on disk, where it reads better than
-- the kind and only needs to tell the obligations of one path apart.
newtype Demand = Demand {demandText :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | The demand of an obligation of this kind.
demandOf :: ObligationKind -> Demand
demandOf = Demand . renderKind

renderBounds :: Integer -> Integer -> Text
renderBounds low high
  | low == high = "of exactly " <> show low
  | otherwise = "from " <> show low <> " to " <> show high
