module Coverage.Report.RegressionsSpec where

import Coverage.Obligations.Obligation (Demand (..))
import Coverage.Obligations.Trail (Absolute, Trail, parseTrail)
import Coverage.Report.CoverageData
import Coverage.Report.Regressions
import Coverage.Report.SpecCoverage
import Generation.EraSpec (Era (..))
import Generation.SampleType (SampleCategory (..), SampleKey (..), SampleName (..), SampleType (..))
import Test.Hspec

spec = describe "Coverage lost against the previous corpus" $ do
  it "counts an obligation that was fulfilled and is not fulfilled now" $
    regressionsAgainst (corpus [fulfilled "choice[0:array]"]) (corpus [outstanding "choice[0:array]"])
      `shouldBe` [NoLongerFulfilled Conway "redeemers" (fulfilled "choice[0:array]") (Just (Sampled Unmet))]

  it "counts an obligation that was fulfilled and that the rule no longer states" $
    regressionsAgainst (corpus [fulfilled "choice[0:array]"]) (corpus [fulfilled "choice[1:map]"])
      `shouldBe` [NoLongerFulfilled Conway "redeemers" (fulfilled "choice[0:array]") Nothing]

  -- A new obligation is new work, not a loss.
  it "does not count a new obligation that nothing fulfils yet" $
    regressionsAgainst (corpus [fulfilled "choice[0:array]"]) (corpus [fulfilled "choice[0:array]", outstanding "choice[1:map]"])
      `shouldBe` []

  -- Another sample can fulfil the obligation now: it is still fulfilled.
  it "does not count an obligation fulfilled by another sample" $
    regressionsAgainst (corpus [fulfilled "choice[0:array]"]) (corpus [(fulfilled "choice[0:array]") {recordedEvidence = metBy "other"}])
      `shouldBe` []

  -- Two obligations can be stated at the same path, so the demand is part of what identifies one.
  it "matches an obligation by its path and its demand" $
    regressionsAgainst
      (corpus [fulfilled "choice[0:array]"])
      (corpus [(fulfilled "choice[0:array]") {recordedDemand = Demand "another demand"}])
      `shouldBe` [NoLongerFulfilled Conway "redeemers" (fulfilled "choice[0:array]") Nothing]

  -- Like a divergence that matches no obligation, a mismatch is a defect of the corpus, whatever the coverage numbers
  -- say.
  it "counts a difference between the divergences and the samples that the specification contradicts" $
    regressionsAgainst (corpus []) ((corpus []) {summaryDivergenceMismatches = [undeclared]})
      `shouldBe` [DivergenceMismatches Conway (undeclared :| [])]

-- * HELPERS

undeclared :: DivergenceMismatch
undeclared = UndeclaredDivergence (SampleKey "redeemers" (SampleName ManualInvalid "accepted"))

-- | A Conway corpus with one rule, redeemers, which states these obligations.
corpus :: [RecordedObligation] -> CoverageSummary
corpus obligations = CoverageSummary Conway [RuleSummary "redeemers" (CoverageCounts 0 0 0 0) "[+ redeemer]" obligations] 0 0 [] [] []

-- | An obligation at this path that a sample fulfils.
fulfilled :: Text -> RecordedObligation
fulfilled path = RecordedObligation Reject (trail path) (Demand "a value that is not array") (metBy "witness")

-- | An obligation at this path that no sample fulfils.
outstanding :: Text -> RecordedObligation
outstanding path = (fulfilled path) {recordedEvidence = Sampled Unmet}

metBy :: Text -> Evidence
metBy stem = Sampled (Met (SampleKey "redeemers" (SampleName ManualInvalid stem)))

trail :: Text -> Trail Absolute
trail = either error id . parseTrail
