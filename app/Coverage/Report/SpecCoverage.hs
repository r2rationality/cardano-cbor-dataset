module Coverage.Report.SpecCoverage where

import Base.Extra
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Divergence (Divergence, DivergenceKind, divergenceKindPairs, parseDivergenceKind, renderDivergenceKind)
import Coverage.Obligations.Obligation
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Generation.Dataset
import Generation.SampleType (SampleKey, SampleName, renderSampleKey)

-- | Coverage of a HuddleSpec
data SpecCoverage = SpecCoverage
  { rules :: [RuleCoverage],
    -- | The divergences in @corpus.json@
    declaredDivergences :: [Divergence],
    -- | The divergences in @corpus.json@ that waive an obligation that does not exist
    unmatchedDivergences :: [Divergence],
    -- | The differences between the divergences in @corpus.json@ and the manual samples that the specification
    -- contradicts
    divergenceMismatches :: [DivergenceMismatch]
  }
  deriving (Eq, Show)

-- | Return the number of samples covering the specification
coverageSamplesNb :: SpecCoverage -> Int
coverageSamplesNb = length . coverageSamples

coverageSamples :: SpecCoverage -> [SampleName]
coverageSamples specCoverage = concatMap ruleCoverageSamples (rules specCoverage)

manualSamplesNb :: SpecCoverage -> Int
manualSamplesNb = count isManualSample . coverageSamples

-- | Coverage of a single rule within a HuddleSpec
data RuleCoverage = RuleCoverage
  { ruleCoverageName :: Name,
    -- | The samples the rule was measured against.
    ruleCoverageSamples :: [SampleName],
    ruleCoverageObligations :: [ObligationCoverage],
    -- | The manual samples whose directory the specification contradicts: a manual-invalid sample that meets no
    -- obligation to reject, or a manual-valid sample that meets one. The node and the specification disagree on them.
    ruleCoverageContradicted :: [SampleName]
  }
  deriving (Eq, Show)

-- | A difference between the divergences that a corpus declares and the manual samples that the specification
-- contradicts. Never a coverage number, always a defect of the corpus.
data DivergenceMismatch
  = -- | The specification contradicts the directory of this manual sample, and no divergence gives the reason.
    UndeclaredDivergence SampleKey
  | -- | A divergence of this kind names this sample, but the specification does not judge it as the kind says.
    MisdeclaredDivergence SampleKey DivergenceKind
  deriving (Eq, Show)

-- | A mismatch as one line.
renderDivergenceMismatch :: DivergenceMismatch -> Text
renderDivergenceMismatch = \case
  UndeclaredDivergence sample ->
    renderSampleKey sample <> ": the specification contradicts its directory and no divergence explains it"
  MisdeclaredDivergence sample kind ->
    renderSampleKey sample <> ": the specification does not judge it as a divergence of kind " <> renderDivergenceKind kind <> " says"

instance ToJSON DivergenceMismatch where
  toJSON = \case
    UndeclaredDivergence sample -> object ["undeclared" .= sample]
    MisdeclaredDivergence sample kind -> object (("misdeclared" .= sample) : divergenceKindPairs kind)

instance FromJSON DivergenceMismatch where
  parseJSON = withObject "divergence mismatch" $ \o ->
    (UndeclaredDivergence <$> o .: "undeclared") <|> (MisdeclaredDivergence <$> o .: "misdeclared" <*> parseDivergenceKind o)

-- | Coverage of a single obligation within a rule
data ObligationCoverage = ObligationCoverage
  { coverageObligation :: Obligation,
    coverageEvidence :: Evidence
  }
  deriving (Eq, Show)

-- | What the measurement knows about one obligation, from which its verdict follows. A waived obligation looks at no
-- sample.
data Evidence
  = -- | A waiver settles the obligation, with its kind and its reason. Nothing is owed here, for a reason that is
    -- written down rather than measured. Which kind decides what the entry means.
    WaivedBy DivergenceKind Text
  | -- | What the candidate samples show.
    Sampled Found
  deriving (Eq, Show)

-- | What the candidate samples of an obligation show, from nothing to a witness. The cases exclude each other: a
-- witness is also a decided answer, and a decided answer needs a candidate, so each state is the strongest one reached.
--
-- 'NoCandidate' and 'Undecided' reach no verdict, so they are blind spots, not gaps. Never report them as gaps: saying
-- "not covered" about something never examined claims more than the run knows.
data Found
  = -- | No sample of the category the obligation is about. More samples of the right kind would settle it, and the
    -- matcher is not the limit.
    NoCandidate
  | -- | Candidates, but none of them gives a yes or a no: no sample reaches the subject. Often the subject is optional,
    -- such as an entry that may be absent, and a sample that holds it settles the obligation. Sometimes no sample can
    -- reach it, such as an alternative that cannot be told from its sibling.
    Undecided
  | -- | Candidates give an answer, and every answer is no. A debt the corpus still owes, and the only state that names
    -- work to do.
    Unmet
  | -- | This sample meets the obligation. One sample is the proof. Its key names the rule whose samples settled the
    -- obligation, which is not always the rule that states it.
    Met SampleKey
  deriving (Eq, Show)

-- | The verdict that the evidence gives an obligation. An untested verdict says why: no sample of the right category,
-- or no sample that reaches the place the obligation is about.
renderVerdict :: Evidence -> Text
renderVerdict = \case
  WaivedBy _ _ -> "waived"
  Sampled NoCandidate -> "untested, no samples"
  Sampled Undecided -> "untested, not reached"
  Sampled Unmet -> "outstanding"
  Sampled (Met _) -> "fulfilled"

-- | Whether a sample meets the obligation.
isFulfilled :: Evidence -> Bool
isFulfilled = \case
  Sampled (Met _) -> True
  _ -> False

-- | The evidence as JSON fields: the verdict, the witness of a fulfilled obligation, and the waiver of a waived one.
evidencePairs :: Evidence -> [Pair]
evidencePairs evidence =
  ("verdict" .= renderVerdict evidence) : case evidence of
    WaivedBy kind why -> ["divergence" .= object (("reason" .= why) : divergenceKindPairs kind)]
    Sampled (Met witness) -> ["witness" .= witness]
    Sampled _ -> []

-- | Read the fields written by 'evidencePairs'.
parseEvidence :: Object -> Parser Evidence
parseEvidence o =
  o .: "verdict" >>= \case
    "waived" -> o .: "divergence" >>= withObject "divergence" (\divergence -> WaivedBy <$> parseDivergenceKind divergence <*> divergence .: "reason")
    "fulfilled" -> Sampled . Met <$> o .: "witness"
    verdict ->
      maybe
        (fail ("unknown verdict: " <> toString verdict))
        pure
        (find ((== verdict) . renderVerdict) (Sampled <$> [NoCandidate, Undecided, Unmet]))
