module Generation.RuleSpec where

import Generation.Rule
import Generation.VerificationFailure
import Test.Hspec

spec = describe "The decoding of a sample" $ do
  -- A ledger decoder can raise an exception on input that it does not expect, such as the Conway block decoder on an
  -- unsorted list of invalid transactions. The node then refuses the block, so the sample is refused.
  it "counts an exception raised by the decoder as a refusal" $
    deserializeSample raisingRule "" >>= (`shouldSatisfy` isLeft)

  it "counts an exception raised by the decoder as a failure to decode a valid sample" $
    reserializeSample raisingRule "" >>= \case
      Left (VerificationFailure DecodeFailed _) -> pure ()
      other -> expectationFailure ("expected a failure to decode, got " <> show other)

-- * HELPERS

-- | A rule whose decoder raises an exception on every input.
raisingRule :: GenerationRule
raisingRule = makeRule "raising" (\_ -> error "unexpected input" :: Either Text ()) (const "") normalized
