module ObligationCoverageSpec (spec) where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Data.ByteString qualified as BS
import ObligationCoverage
  ( Coverage (..),
    Report (..),
    Sample (..),
    Source (..),
    Status (..),
    coverageOf,
    renderCause,
  )
import Obligations
  ( Obligation (..),
    ObligationKind (..),
    Step (..),
    obligationsFor,
    renderPath,
  )
import Reachability (eraRoot)
import Test.Hspec

spec :: Spec
spec = describe "Fulfilling the redeemers lower bounds" $ do
  -- The corpus holds five empty arrays and no empty map. Before the branch
  -- shape was checked an empty array satisfied both bounds, so the map branch
  -- read as covered when nothing in the corpus can cover it.
  it "credits an empty array to the array branch alone" $
    statuses [emptyArray] `shouldBe` [("choice[0]", "fulfilled"), ("choice[1]", "untested (no path)")]

  it "credits an empty map to the map branch alone" $
    statuses [emptyMap] `shouldBe` [("choice[0]", "untested (no path)"), ("choice[1]", "fulfilled")]

  it "credits each branch once both are present" $
    statuses [emptyArray, emptyMap] `shouldBe` [("choice[0]", "fulfilled"), ("choice[1]", "fulfilled")]

  -- A rejected sample that breaks something else leaves this bound measurably
  -- unmet, which is the difference between a gap and a question the check
  -- cannot answer. A well formed sample would not do: the specification accepts
  -- it, so it is no candidate for breaking anything.
  it "reports a bound as outstanding when a rejected sample leaves it unbroken" $
    statuses [malformedRedeemer]
      `shouldBe` [("choice[0]", "outstanding"), ("choice[1]", "untested (no path)")]

-- HELPERS

-- | An indefinite array holding nothing, which @[+ redeemer]@ forbids.
emptyArray :: Sample
emptyArray = invalidSample "empty-array" [0x9f, 0xff]

-- | A definite map holding nothing, which the map branch forbids and which the
-- generator cannot produce: its mandatory entry is not a decision point.
emptyMap :: Sample
emptyMap = invalidSample "empty-map" [0xa0]

-- | @[[0]]@: an array of one element, so the lower bound holds, but the element
-- is not a redeemer, so the specification rejects the sample anyway.
malformedRedeemer :: Sample
malformedRedeemer = invalidSample "malformed-redeemer" [0x81, 0x81, 0x00]

invalidSample :: Text -> [Word8] -> Sample
invalidSample name bytes =
  Sample {sampleName = name, sampleSource = FromInvalid, sampleBytes = BS.pack bytes}

-- | The two lower bounds of @redeemers@, with their status, keyed by where the
-- bound is stated.
statuses :: [Sample] -> [(Text, Text)]
statuses samples =
  [ (renderPath (obligationPath (coverageObligation entry)), label (coverageStatus entry))
  | entry <- reportCoverage (coverageOf conwayRoot redeemers lowerBounds samples)
  ]
  where
    lowerBounds =
      [ obligation
      | obligation <- obligationsFor conwayRoot redeemers,
        TooFew _ <- [obligationKind obligation],
        -- One per branch: each alternative forbids an empty container of its
        -- own kind, and the two are what an empty array and an empty map
        -- separately fulfil.
        [StepChoice _ _] <- [obligationPath obligation]
      ]
    label = \case
      Fulfilled -> "fulfilled"
      Outstanding -> "outstanding"
      Untested cause -> "untested (" <> renderCause cause <> ")"

redeemers :: Name
redeemers = Name "redeemers"

conwayRoot :: CTreeRoot MonoReferenced
conwayRoot = either (error . toText) id (eraRoot "conway")
