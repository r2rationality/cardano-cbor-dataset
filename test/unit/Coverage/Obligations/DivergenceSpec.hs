{-# LANGUAGE QuasiQuotes #-}

module Coverage.Obligations.DivergenceSpec where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Divergence
import Coverage.Obligations.Obligation (ObligationKind (..))
import Coverage.Obligations.Trail (parseTrail)
import Data.Aeson (Value, eitherDecode, encode)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseEither, parseJSON)
import Generation.SampleType (SampleKey, SampleType (..), parseSampleKey)
import Test.Hspec

spec = describe "The divergences a corpus declares" $ do
  it "round trips a divergence of each kind" $
    for_ kinds $ \kind -> do
      let divergence = Divergence kind (sampleFor kind :| []) "the reason"
      eitherDecode (encode divergence) `shouldBe` Right divergence

  it "names a divergence kind that is not recognized" $
    [aesonQQ|{"kind": "verified-by-hope", "reason": "no reason"}|] `isRefusedWith` "unknown divergence kind: verified-by-hope"

  it "refuses a sample that is not named <rule>/<sample type>/<stem>" $
    [aesonQQ|{"kind": "decoder-exception", "samples": ["block/bare-empty"], "reason": "no reason"}|]
      `isRefusedWith` "not a sample: block/bare-empty"

  it "refuses a kind that waives an obligation without the obligation" $
    [aesonQQ|{"kind": "incorrect-specification", "reason": "no reason"}|] `isRefusedWith` "key \"rule\" not found"

  it "refuses an obligation for a kind that waives none" $
    [aesonQQ|{"kind": "decoder-exception", "rule": "block", "samples": ["block/manual-invalid/x"], "reason": "no reason"}|]
      `isRefusedWith` "a divergence of kind decoder-exception waives no obligation"

  -- A sample is the proof of a divergence, for every kind.
  it "refuses a divergence without samples, whatever its kind" $ do
    [aesonQQ|{"kind": "decoder-exception", "reason": "no reason"}|]
      `isRefusedWith` "a divergence of kind decoder-exception must name a sample"
    [aesonQQ|{"kind": "incorrect-specification", "rule": "metadatum", "path": ".", "obligation": {"tag": "DuplicateKey"}, "reason": "no reason"}|]
      `isRefusedWith` "a divergence of kind incorrect-specification must name a sample"

  -- The directory of a sample says what the node does with it, so each kind has its own.
  it "refuses a sample in the directory of the other outcome" $ do
    [aesonQQ|{"kind": "decoder-exception", "samples": ["block/manual-valid/x"], "reason": "no reason"}|]
      `isRefusedWith` "a divergence of kind decoder-exception names manual-invalid samples only: block/manual-valid/x"
    -- Nothing refuses the samples of an incorrect specification, so they are valid samples.
    [aesonQQ|{"kind": "incorrect-specification", "rule": "metadatum", "path": ".", "obligation": {"tag": "DuplicateKey"}, "samples": ["metadatum/manual-invalid/repeated-key"], "reason": "no reason"}|]
      `isRefusedWith` "a divergence of kind incorrect-specification names manual-valid samples only: metadatum/manual-invalid/repeated-key"
    -- A decoder for the rule can check what a parent checks, so its samples are invalid ones.
    [aesonQQ|{"kind": "verified-by-parent", "rule": "withdrawals", "path": ".", "obligation": {"tag": "TooFew", "contents": 1}, "samples": ["withdrawals/manual-valid/empty"], "reason": "no reason"}|]
      `isRefusedWith` "a divergence of kind verified-by-parent names manual-invalid samples only: withdrawals/manual-valid/empty"

-- * HELPERS

-- | One kind of each.
kinds :: [DivergenceKind]
kinds =
  [ VerifiedByParent emptyCertificates,
    IncorrectSpecification emptyCertificates,
    DecoderException,
    UnstatedConstraint Nothing,
    UnstatedConstraint (Just emptyCertificates)
  ]
  where
    emptyCertificates = WaivedObligation (Name "certificates") (either error id (parseTrail "validator.gen.choice[1:array]")) (TooFew 1)

-- | A sample in the directory that a divergence of this kind needs.
sampleFor :: DivergenceKind -> SampleKey
sampleFor kind = sample ("certificates/" <> directory <> "/bare-empty")
  where
    directory = if divergenceSampleType kind == ManualValid then "manual-valid" else "manual-invalid"

-- | A sample named by its key.
sample :: Text -> SampleKey
sample = either error id . parseSampleKey

-- | A divergence written as this JSON object is refused, with an error that contains this message.
isRefusedWith :: Value -> String -> Expectation
isRefusedWith json message =
  case parseEither (parseJSON @Divergence) json of
    Left failure -> failure `shouldContain` message
    Right accepted -> expectationFailure ("accepted " <> show accepted)
