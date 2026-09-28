module Coverage.Report.MeasureSpec where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Coverage.Obligations.Divergence
import Coverage.Obligations.Obligation
import Coverage.Obligations.Obligations
import Coverage.Obligations.ObligationsSpec (obligationsFor)
import Coverage.Obligations.Shape (Shape (..))
import Coverage.Obligations.SpecRule (HuddleSpec, eraRoot)
import Coverage.Obligations.Step
import Coverage.Obligations.Trail
import Coverage.Report.Measure
import Coverage.Report.Samples
import Coverage.Report.SpecCoverage
import Coverage.Report.TermChecker
import Data.ByteString qualified as BS
import Generation.DatasetConfig
import Generation.SampleType (SampleCategory (..), SampleKey (..), SampleName (..), SampleType (..))
import Test.Hspec

spec = describe "Fulfilling the redeemers lower bounds" $ do
  -- The corpus holds five empty arrays and no empty map. Before the branch
  -- shape was checked an empty array satisfied both bounds, so the map branch
  -- read as covered when nothing in the corpus can cover it.
  it "credits an empty array to the array branch alone" $
    statuses [emptyArray] `shouldBe` [("choice[0:array]", "fulfilled"), ("choice[1:map]", "untested, not reached")]

  it "credits an empty map to the map branch alone" $
    statuses [emptyMap] `shouldBe` [("choice[0:array]", "untested, not reached"), ("choice[1:map]", "fulfilled")]

  it "credits each branch once both are present" $
    statuses [emptyArray, emptyMap] `shouldBe` [("choice[0:array]", "fulfilled"), ("choice[1:map]", "fulfilled")]

  -- A rejected sample that breaks something else leaves this bound measurably
  -- unmet, which is the difference between a gap and a question the check
  -- cannot answer. A well formed sample would not do: the specification accepts
  -- it, so it is no candidate for breaking anything.
  it "reports a bound as outstanding when a rejected sample leaves it unbroken" $
    statuses [malformedRedeemer]
      `shouldBe` [("choice[0:array]", "outstanding"), ("choice[1:map]", "untested, not reached")]

  -- An obligation the specification should not be stating is settled by saying
  -- so, with no sample: bytes the ledger is right to accept are not a
  -- divergence worth keeping, so there is nothing for a corpus to hold.
  it "settles an obligation the era declares the specification wrong to state" $
    declared [deniesTheEmptyMap] []
      `shouldBe` [ ("choice[0:array]", "untested, no samples"),
                   ("choice[1:map]", "incorrect specification (an empty redeemers map is what the ledger writes)")
                 ]

  -- The entry outliving what it spoke for is as much a defect as a missing one,
  -- so a declaration matching nothing stops the run rather than passing.
  it "reports a declaration that names no obligation of the rule" $
    findUnmatchedDivergences [redeemersCoverage] [deniesTheEmptyMap, waiving "redeemers" (TooMany 1000)]
      `shouldBe` [waiving "redeemers" (TooMany 1000)]

  -- A divergence for a rule the corpus has no directory for matches no measured
  -- obligation: a misspelled rule is not ignored.
  it "reports a divergence whose rule has no directory in the corpus" $
    findUnmatchedDivergences [redeemersCoverage] [deniesTheEmptyMap, waiving "redeemer" (TooFew 1)]
      `shouldBe` [waiving "redeemer" (TooFew 1)]

  describe "The manual samples that the specification contradicts" $ do
    -- 0 is a coin, -1 is not: only the first one needs a reason to be under manual-invalid, and only the second one
    -- needs a reason to be under manual-valid.
    it "records a manual sample whose directory the specification contradicts" $
      ruleCoverageContradicted (coinAssessed [invalidSample "coin" "zero" [0x00], invalidSample "coin" "minus-one" [0x20], validSample "coin" "minus-one" [0x20], validSample "coin" "one" [0x01]])
        `shouldBe` [SampleName ManualInvalid "zero", SampleName ManualValid "minus-one"]

    -- The node accepts it, but the specification does not: it proves nothing about what a decoder must accept.
    it "does not take a manual-valid sample that the specification refuses as a witness" $
      [ renderVerdict (coverageEvidence entry)
      | entry <- ruleCoverageObligations (coinAssessed [validSample "coin" "minus-one" [0x20]]),
        sampleCategoryFor (coverageObligation entry) == Accept
      ]
        `shouldSatisfy` notElem "fulfilled"

    it "reports a contradicted sample that no divergence names" $
      findDivergenceMismatches [coinCoverage [invalidCoin "zero"]] [] `shouldBe` [UndeclaredDivergence (invalidCoin "zero")]

    -- The divergence outlives what it explains, for example after a change of the specification.
    it "reports a divergence that names a sample the specification does not judge as its kind says" $
      findDivergenceMismatches [coinCoverage []] [naming (UnstatedConstraint Nothing) (invalidCoin "minus-one")]
        `shouldBe` [MisdeclaredDivergence (invalidCoin "minus-one") (UnstatedConstraint Nothing)]

    it "accepts a divergence of each kind that names a sample the specification judges as the kind says" $ do
      findDivergenceMismatches [coinCoverage [invalidCoin "zero"]] [naming (UnstatedConstraint Nothing) (invalidCoin "zero")] `shouldBe` []
      findDivergenceMismatches [coinCoverage [validCoin "minus-one"]] [naming (IncorrectSpecification coinBelowRange) (validCoin "minus-one")] `shouldBe` []
      -- The specification refuses it, as its directory says, and the Haskell decoder of the rule accepts it.
      findDivergenceMismatches [coinCoverage []] [naming (VerifiedByParent coinBelowRange) (invalidCoin "minus-one")] `shouldBe` []

  -- Each kind of evidence gives one verdict, whatever the order of the cases.
  describe "The verdict on an obligation" $
    it "follows from the evidence" $
      map
        renderVerdict
        [ WaivedBy (IncorrectSpecification coinBelowRange) "the reason",
          Sampled NoCandidate,
          Sampled Undecided,
          Sampled Unmet,
          Sampled (Met (SampleKey "redeemers" (SampleName ManualInvalid "witness")))
        ]
        `shouldBe` ["waived", "untested, no samples", "untested, not reached", "outstanding", "fulfilled"]

-- * HELPERS

-- | An indefinite array holding nothing, which @[+ redeemer]@ forbids.
emptyArray :: Sample
emptyArray = invalidSample redeemers "empty-array" [0x9f, 0xff]

-- | A definite map holding nothing, which the map branch forbids and which the generator cannot produce: its mandatory
-- entry is not a decision point.
emptyMap :: Sample
emptyMap = invalidSample redeemers "empty-map" [0xa0]

-- | @[[0]]@: an array of one element, so the lower bound holds, but the element is not a redeemer, so the specification
-- rejects the sample anyway.
malformedRedeemer :: Sample
malformedRedeemer = invalidSample redeemers "malformed-redeemer" [0x81, 0x81, 0x00]

-- | A declaration that redeemers' map branch should not forbid an empty map.
deniesTheEmptyMap :: Divergence
deniesTheEmptyMap = waiving "redeemers" (TooFew 1)

-- | A divergence of an incorrect specification, for an obligation of this kind of this rule, on the map branch.
waiving :: Name -> ObligationKind -> Divergence
waiving rule kind =
  Divergence
    { divergenceKind = IncorrectSpecification (WaivedObligation rule (trailOf [StepChoice 1 ShapeMap]) kind),
      divergenceSamples = SampleKey rule (SampleName ManualValid "repeated-key") :| [],
      divergenceReason = "an empty redeemers map is what the ledger writes"
    }

invalidSample :: Name -> Text -> [Word8] -> Sample
invalidSample = manualSample ManualInvalid

validSample :: Name -> Text -> [Word8] -> Sample
validSample = manualSample ManualValid

manualSample :: SampleType -> Name -> Text -> [Word8] -> Sample
manualSample kind rule name bytes =
  Sample (SampleKey rule (SampleName kind name)) (either error id (decodeCbor (BS.pack bytes)))

-- | The two lower bounds of @redeemers@, with their status, keyed by where the bound is stated.
statuses :: [Sample] -> [(Text, Text)]
statuses = declared []

declared :: [Divergence] -> [Sample] -> [(Text, Text)]
declared defects samples =
  [ (renderTrail (obligationPath (coverageObligation entry)), label (coverageEvidence entry))
  | entry <- ruleCoverageObligations (assessCoverage (newTermChecker conwayRoot) redeemers lowerBounds defects samples)
  ]
  where
    lowerBounds =
      [ obligation
      | obligation <- obligationsFor conwayRoot redeemers,
        TooFew _ <- [obligationKind obligation],
        -- One per branch: each alternative forbids an empty container of its
        -- own kind, and the two are what an empty array and an empty map
        -- separately fulfil.
        [StepChoice _ _] <- [steps (obligationPath obligation)]
      ]
    label = \case
      WaivedBy kind why -> divergenceKindName kind <> " (" <> why <> ")"
      evidence -> renderVerdict evidence

-- | The coverage of the two lower bounds of redeemers, measured against no sample.
redeemersCoverage :: RuleCoverage
redeemersCoverage = assessCoverage (newTermChecker conwayRoot) redeemers lowerBoundsOf [] []
  where
    lowerBoundsOf =
      [ obligation
      | obligation <- obligationsFor conwayRoot redeemers,
        TooFew _ <- [obligationKind obligation],
        [StepChoice _ _] <- [steps (obligationPath obligation)]
      ]

-- | The coverage of coin, measured against these manual samples.
coinAssessed :: [Sample] -> RuleCoverage
coinAssessed = assessCoverage (newTermChecker conwayRoot) "coin" (obligationsFor conwayRoot "coin") []

-- | The coverage of coin, with these manual samples that the specification contradicts.
coinCoverage :: [SampleKey] -> RuleCoverage
coinCoverage samples = RuleCoverage "coin" [] [] (map sampleKeyName samples)

invalidCoin :: Text -> SampleKey
invalidCoin = SampleKey "coin" . SampleName ManualInvalid

validCoin :: Text -> SampleKey
validCoin = SampleKey "coin" . SampleName ManualValid

-- | The obligation of coin to refuse a value below 0.
coinBelowRange :: WaivedObligation
coinBelowRange = WaivedObligation "coin" (trailOf []) (BelowRange 0)

-- | A divergence of this kind that names this sample.
naming :: DivergenceKind -> SampleKey -> Divergence
naming kind sample = Divergence kind (sample :| []) "the reason"

redeemers :: Name
redeemers = Name "redeemers"

conwayRoot :: HuddleSpec
conwayRoot = either (error . toText) id (eraRoot "conway")
