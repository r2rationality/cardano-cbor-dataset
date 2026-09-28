module Generation.VerifySpec where

import Coverage.Obligations.Divergence
import Coverage.Obligations.Obligation (ObligationKind (..))
import Coverage.Obligations.Trail (trailOf)
import Data.Default (Default (..))
import Generation.Dataset (DatasetFile (..))
import Generation.DatasetConfig
import Generation.Rule
import Generation.SampleType (SampleKey (..), SampleName (..), SampleType (..))
import Generation.VerificationFailure
import Generation.Verify
import Test.Hspec

spec = describe "The divergences that verification applies" $ do
  -- The divergence says that the node accepts this sample, which the specification refuses.
  it "lets the node decode a manual-invalid sample that a divergence names" $
    applyDivergences withEmptyWithdrawals emptyWithdrawals (Just (VerificationFailure DecodeSucceeded "decoded")) `shouldBe` Nothing

  -- Otherwise the divergence is only a claim: nothing would notice that the node stopped accepting the sample.
  it "fails when the node refuses a sample that a divergence says it accepts" $
    applyDivergences withEmptyWithdrawals emptyWithdrawals Nothing `shouldSatisfy` isJust

  it "leaves a sample that no divergence names alone" $ do
    applyDivergences def emptyWithdrawals Nothing `shouldBe` Nothing
    applyDivergences def emptyWithdrawals (Just (VerificationFailure DecodeSucceeded "decoded")) `shouldSatisfy` isJust

-- * HELPERS

emptyWithdrawals :: DatasetFile
emptyWithdrawals = DatasetFile (makeRule "withdrawals" (const (Right ())) (const "") normalized) (SampleName ManualInvalid "empty")

-- | A corpus that declares that the decoder of withdrawals accepts an empty map, which the transaction body refuses.
withEmptyWithdrawals :: DatasetConfig
withEmptyWithdrawals =
  def
    { datasetDivergences =
        Just
          [ Divergence
              (VerifiedByParent (WaivedObligation "withdrawals" (trailOf []) (TooFew 1)))
              (SampleKey "withdrawals" (SampleName ManualInvalid "empty") :| [])
              "the transaction body refuses it"
          ]
    }
