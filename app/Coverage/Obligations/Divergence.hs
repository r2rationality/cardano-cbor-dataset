module Coverage.Obligations.Divergence where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Obligation
import Coverage.Obligations.Trail (Absolute, Trail, renderTrail)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Set qualified as Set
import Generation.SampleType (SampleKey (..), SampleName (..), SampleType (..), renderSampleKey, sampleDirectory)

-- | A place where the Haskell node and the specification disagree, and why.
--
-- | Kind                      | Specification | Node    | `rule`, `path`, `obligation` | `samples`      |
-- | ------------------------- | ------------- | ------- | ---------------------------- | -------------- |
-- | `verified-by-parent`      | refuses       | accepts | required                     | manual-invalid |
-- | `incorrect-specification` | refuses       | accepts | required                     | manual-valid   |
-- | `decoder-exception`       | accepts       | refuses | none                         | manual-invalid |
-- | `unstated-constraint`     | accepts       | refuses | optional                     | manual-invalid |
--
-- Some kinds settle an obligation that the node does not enforce. Every kind names manual samples, and their directory
-- says what a conforming decoder for the rule does with them, so an implementation runs the samples without reading
-- the divergences. A reader who finds bytes that the specification accepts under @manual-invalid@, or bytes that it
-- refuses under @manual-valid@, finds the reason here.
data Divergence = Divergence
  { divergenceKind :: DivergenceKind,
    -- | The samples that show the divergence, named @<rule>/<sample type>/<stem>@
    divergenceSamples :: NonEmpty SampleKey,
    divergenceReason :: Text
  }
  deriving (Eq, Show)

-- | An obligation, named by its rule, by the path the coverage report shows, and by its kind.
data WaivedObligation = WaivedObligation
  { waivedRule :: Name,
    waivedPath :: Trail Absolute,
    waivedKind :: ObligationKind
  }
  deriving (Eq, Show)

-- | How the Haskell node and the specification disagree.
data DivergenceKind
  = -- | The decoder of the rule does not enforce the obligation, but the decoder of whatever carries it does.
    VerifiedByParent WaivedObligation
  | -- | Nothing enforces the obligation, and the specification is wrong to ask for it.
    IncorrectSpecification WaivedObligation
  | -- | The decoder raises an exception rather than a decode error for some valid input
    DecoderException
  | -- | The decoder checks a condition that the specification does not state, e.g. a denominator that is not zero
    UnstatedConstraint (Maybe WaivedObligation)
  deriving (Eq, Show)

-- | The obligation that a divergence of this kind settles, if the kind settles one.
waivedObligation :: DivergenceKind -> Maybe WaivedObligation
waivedObligation = \case
  VerifiedByParent named -> Just named
  IncorrectSpecification named -> Just named
  UnstatedConstraint named -> named
  _ -> Nothing

-- | Whether the specification accepts the samples of a divergence of this kind.
specificationAccepts :: DivergenceKind -> Bool
specificationAccepts = \case
  DecoderException -> True
  UnstatedConstraint _ -> True
  _ -> False

-- | The directory of the samples of a divergence of this kind.
--
-- The directory says what a conforming decoder for the rule does with the samples. Mostly that is what the Haskell node
-- does. A constraint verified by a parent is the exception: the Haskell node checks it in another decoder, and a
-- decoder for the rule can check it directly, so the samples are invalid ones that the Haskell decoder of the rule
-- accepts.
divergenceSampleType :: DivergenceKind -> SampleType
divergenceSampleType = \case
  VerifiedByParent _ -> ManualInvalid
  IncorrectSpecification _ -> ManualValid
  DecoderException -> ManualInvalid
  UnstatedConstraint _ -> ManualInvalid

-- | A kind as @corpus.json@ writes it.
renderDivergenceKind :: DivergenceKind -> Text
renderDivergenceKind = \case
  VerifiedByParent _ -> "verified-by-parent"
  IncorrectSpecification _ -> "incorrect-specification"
  DecoderException -> "decoder-exception"
  UnstatedConstraint _ -> "unstated-constraint"

-- | How a kind reads on a report line.
divergenceKindName :: DivergenceKind -> Text
divergenceKindName = \case
  VerifiedByParent _ -> "verification by parent decoder"
  IncorrectSpecification _ -> "incorrect specification"
  DecoderException -> "decoder exception"
  UnstatedConstraint _ -> "unstated constraint"

-- | A kind as JSON fields: its name, and the obligation of a kind that settles one.
divergenceKindPairs :: DivergenceKind -> [Pair]
divergenceKindPairs kind =
  ("kind" .= renderDivergenceKind kind) : foldMap obligationPairs (waivedObligation kind)
  where
    obligationPairs named = ["rule" .= waivedRule named, "path" .= waivedPath named, "obligation" .= waivedKind named]

-- | Read the fields written by 'divergenceKindPairs'. The obligation is required for a kind that settles one, optional
-- for an unstated constraint, and refused for the others.
parseDivergenceKind :: Object -> Parser DivergenceKind
parseDivergenceKind o =
  (o .: "kind" :: Parser Text) >>= \case
    "verified-by-parent" -> VerifiedByParent <$> obligation
    "incorrect-specification" -> IncorrectSpecification <$> obligation
    "decoder-exception" -> DecoderException <$ noObligation "decoder-exception"
    "unstated-constraint" -> UnstatedConstraint <$> maybeObligation
    other -> fail ("unknown divergence kind: " <> toString other)
  where
    obligation = WaivedObligation . Name <$> o .: "rule" <*> o .: "path" <*> o .: "obligation"
    named = traverse (o .:?) ["rule", "path", "obligation" :: Key] :: Parser [Maybe Value]
    maybeObligation = named >>= \fields -> if all isNothing fields then pure Nothing else Just <$> obligation
    noObligation kind =
      named >>= \fields ->
        unless (all isNothing fields) $
          fail ("a divergence of kind " <> kind <> " waives no obligation")

-- | A divergence as one line: its kind, and the obligation or the samples it is about.
renderDivergence :: Divergence -> Text
renderDivergence divergence =
  renderDivergenceKind kind <> " " <> case waivedObligation kind of
    Just named -> renderWaivedObligation named
    Nothing -> unwords (renderSampleKey <$> toList (divergenceSamples divergence))
  where
    kind = divergenceKind divergence

-- | An obligation as one line: the rule, the path and what the obligation asks for.
renderWaivedObligation :: WaivedObligation -> Text
renderWaivedObligation named =
  unName (waivedRule named) <> " " <> renderTrail (waivedPath named) <> ": " <> renderKind (waivedKind named)

instance ToJSON Divergence where
  toJSON divergence =
    object $
      divergenceKindPairs (divergenceKind divergence)
        <> ["samples" .= divergenceSamples divergence, "reason" .= divergenceReason divergence]

-- | The samples are required, and must be in the directory that says what the node does with them.
instance FromJSON Divergence where
  parseJSON = withObject "divergence" $ \o -> do
    kind <- parseDivergenceKind o
    let name = toString (renderDivergenceKind kind)
        directory = divergenceSampleType kind
    samples <- fromMaybe [] <$> o .:? "samples"
    named <- maybe (fail ("a divergence of kind " <> name <> " must name a sample")) pure (nonEmpty samples)
    for_ samples $ \sample ->
      unless (sampleNameType (sampleKeyName sample) == directory) $
        fail
          ( "a divergence of kind "
              <> name
              <> " names "
              <> toString (sampleDirectory directory)
              <> " samples only: "
              <> toString (renderSampleKey sample)
          )
    Divergence kind named <$> o .: "reason"

-- | The rules that a divergence is about: the rule of the obligation that it settles, or else the rules of its samples.
divergenceRules :: Divergence -> [Name]
divergenceRules divergence = case waivedObligation (divergenceKind divergence) of
  Just named -> [waivedRule named]
  Nothing -> ordNub (sampleKeyRule <$> toList (divergenceSamples divergence))

-- | Every sample that the divergences name.
divergentSamples :: [Divergence] -> Set SampleKey
divergentSamples divergences = Set.fromList (concatMap (toList . divergenceSamples) divergences)

-- | The samples under @manual-invalid@ that the Haskell decoder of the rule must decode, because a divergence says that
-- another decoder checks what they break.
acceptedSamples :: [Divergence] -> Set SampleKey
acceptedSamples divergences =
  Set.fromList
    [ sample
    | divergence <- divergences,
      let kind = divergenceKind divergence,
      not (specificationAccepts kind),
      divergenceSampleType kind == ManualInvalid,
      sample <- toList (divergenceSamples divergence)
    ]

-- | Whether a divergence settles an obligation of this rule.
waivesRule :: Name -> Divergence -> Bool
waivesRule rule divergence = (waivedRule <$> waivedObligation (divergenceKind divergence)) == Just rule

-- | The kind and the reason of the divergence that settles an obligation, if one does.
waived :: Obligation -> [Divergence] -> Maybe (DivergenceKind, Text)
waived obligation divergences =
  viaNonEmpty
    head
    [(divergenceKind divergence, divergenceReason divergence) | divergence <- divergences, matches divergence obligation]

-- | Whether the divergence settles the obligation: same path and same kind.
matches :: Divergence -> Obligation -> Bool
matches divergence obligation = case waivedObligation (divergenceKind divergence) of
  Just named -> waivedPath named == obligationPath obligation && waivedKind named == obligationKind obligation
  Nothing -> False
