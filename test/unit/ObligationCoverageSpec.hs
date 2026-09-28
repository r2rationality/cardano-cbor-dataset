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
    SpecDefect (..),
    Status (..),
    Waiver (..),
    coverageGiven,
    coverageOf,
    renderCause,
    waiverName,
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

  -- The decoder for a rule is sometimes looser than the specification, with the
  -- constraint enforced by whatever carries the rule instead. Such a sample
  -- settles the obligation without covering it, and saying so is the difference
  -- between a decision taken and a debt still owed.
  it "records a waiver rather than coverage when only the decoder's leniency answers a bound" $
    statuses [waivedEmptyMap]
      `shouldBe` [ ("choice[0]", "untested (no path)"),
                   ("choice[1]", "verification by parent decoder (enforced at the field that carries it)")
                 ]

  it "prefers a sample the decoder refuses over a waiver for the same bound" $
    statuses [waivedEmptyMap, emptyMap]
      `shouldBe` [("choice[0]", "untested (no path)"), ("choice[1]", "fulfilled")]

  -- An obligation the specification should not be stating is settled by saying
  -- so, with no sample: bytes the ledger is right to accept are not a
  -- divergence worth keeping, so there is nothing for a corpus to hold.
  it "settles an obligation the era declares the specification wrong to state" $
    declared [(IncorrectSpecification, deniesTheEmptyMap)] []
      `shouldBe` [ ("choice[0]", "untested (no samples)"),
                   ("choice[1]", "incorrect specification (an empty redeemers map is what the ledger writes)")
                 ]

  -- The entry outliving what it spoke for is as much a defect as a missing one,
  -- so a declaration matching nothing stops the run rather than passing.
  it "reports a declaration that names no obligation of the rule" $
    findings [(IncorrectSpecification, deniesTheEmptyMap {defectDemand = "a demand no obligation makes"})] `shouldBe` 1

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

-- | The same empty map, waived: the specification forbids it and the decoder
-- takes it, for the written reason.
waivedEmptyMap :: Sample
waivedEmptyMap =
  (invalidSample "waived-empty-map" [0xa0])
    { sampleSource = FromWaived "enforced at the field that carries it"
    }

-- | A declaration that redeemers' map branch should not forbid an empty map.
deniesTheEmptyMap :: SpecDefect
deniesTheEmptyMap =
  SpecDefect
    { defectRule = "redeemers",
      defectPath = "choice[1]",
      defectDemand = "too few: 0 entries where 1 is the minimum",
      defectReason = "an empty redeemers map is what the ledger writes"
    }

invalidSample :: Text -> [Word8] -> Sample
invalidSample name bytes =
  Sample {sampleName = name, sampleSource = FromInvalid, sampleBytes = BS.pack bytes}

-- | The two lower bounds of @redeemers@, with their status, keyed by where the
-- bound is stated.
statuses :: [Sample] -> [(Text, Text)]
statuses = declared []

declared :: [(Waiver, SpecDefect)] -> [Sample] -> [(Text, Text)]
declared defects samples =
  [ (renderPath (obligationPath (coverageObligation entry)), label (coverageStatus entry))
  | entry <- reportCoverage (coverageGiven defects conwayRoot redeemers lowerBounds samples)
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
      Waived kind why -> waiverName kind <> " (" <> why <> ")"
      Outstanding -> "outstanding"
      Untested cause -> "untested (" <> renderCause cause <> ")"

-- | How many things the run turned up that are not about coverage.
findings :: [(Waiver, SpecDefect)] -> Int
findings defects =
  length (reportFindings (coverageGiven defects conwayRoot redeemers lowerBoundsOf []))
  where
    lowerBoundsOf =
      [ obligation
      | obligation <- obligationsFor conwayRoot redeemers,
        TooFew _ <- [obligationKind obligation],
        [StepChoice _ _] <- [obligationPath obligation]
      ]

redeemers :: Name
redeemers = Name "redeemers"

conwayRoot :: CTreeRoot MonoReferenced
conwayRoot = either (error . toText) id (eraRoot "conway")
