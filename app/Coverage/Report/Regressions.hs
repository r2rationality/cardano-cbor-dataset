module Coverage.Report.Regressions where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Divergence (Divergence, renderDivergence)
import Coverage.Obligations.Obligation (Demand (..))
import Coverage.Obligations.Trail (renderTrail)
import Coverage.Report.CoverageData
import Coverage.Report.SpecCoverage (DivergenceMismatch, Evidence, isFulfilled, renderDivergenceMismatch, renderVerdict)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Generation.EraSpec (Era, renderEra)

-- | Coverage lost since the previous measurement of a corpus.
data Regression
  = -- | An obligation that a sample fulfilled is not fulfilled now. The new evidence, or nothing when the rule no longer
    -- states an obligation with this path and this demand.
    NoLongerFulfilled Era Name RecordedObligation (Maybe Evidence)
  | -- | A rule the previous measurement knows is no longer measured at all.
    RuleGone Era Name
  | -- | Divergences in @corpus.json@ that waive an obligation that does not exist. Never a coverage number, always a
    -- defect, so it fails the run on its own.
    UnmatchedDivergences Era (NonEmpty Divergence)
  | -- | Manual samples that the specification contradicts and that no divergence explains, or divergences that name
    -- samples the specification does not judge as their kind says. A defect of the corpus, so it fails the run on its
    -- own.
    DivergenceMismatches Era (NonEmpty DivergenceMismatch)
  deriving (Eq, Show)

renderRegression :: Regression -> Text
renderRegression = \case
  NoLongerFulfilled era rule obligation now ->
    renderEra era
      <> "/"
      <> unName rule
      <> " "
      <> renderTrail (recordedPath obligation)
      <> " ("
      <> demandText (recordedDemand obligation)
      <> "): fulfilled before, "
      <> maybe "no longer stated" (("now " <>) . renderVerdict) now
  RuleGone era rule -> renderEra era <> "/" <> unName rule <> ": no longer measured"
  UnmatchedDivergences era divergences ->
    renderEra era
      <> ": divergences in corpus.json that match no obligation: "
      <> T.intercalate "; " (renderDivergence <$> toList divergences)
  DivergenceMismatches era mismatches ->
    renderEra era
      <> ": divergences in corpus.json that do not match the samples: "
      <> T.intercalate "; " (renderDivergenceMismatch <$> toList mismatches)

-- | What a new measurement of a corpus lost against the previous one.
--
-- Only losses: an obligation that a sample fulfilled and that is not fulfilled now, or a rule that is no longer measured.
-- A new obligation is new work, not a loss. An obligation is matched by its path and its demand, so an obligation whose
-- path or demand reads differently now shows as lost.
regressionsAgainst :: CoverageSummary -> CoverageSummary -> [Regression]
regressionsAgainst previous current = unmatched <> mismatched <> gone <> lost
  where
    era = summaryEra current
    recorded = Map.fromList [(ruleName rule, ruleObligations rule) | rule <- summaryRules previous]
    measuredNow =
      Map.fromList
        [ (ruleName rule, Map.fromList [((recordedPath o, recordedDemand o), recordedEvidence o) | o <- ruleObligations rule])
        | rule <- summaryRules current
        ]

    unmatched = [UnmatchedDivergences era divergences | Just divergences <- [nonEmpty (summaryUnmatchedDivergences current)]]

    mismatched = [DivergenceMismatches era mismatches | Just mismatches <- [nonEmpty (summaryDivergenceMismatches current)]]

    gone = [RuleGone era rule | rule <- Map.keys recorded, not (Map.member rule measuredNow)]

    lost =
      [ NoLongerFulfilled era rule obligation evidenceNow
      | (rule, obligations) <- Map.toList recorded,
        Just evidences <- [Map.lookup rule measuredNow],
        obligation <- obligations,
        isFulfilled (recordedEvidence obligation),
        let evidenceNow = Map.lookup (recordedPath obligation, recordedDemand obligation) evidences,
        not (any isFulfilled evidenceNow)
      ]
