module Coverage.Report.TermCheckerSpec where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Term (Term (..))
import Coverage.Obligations.Obligation
import Coverage.Obligations.Obligations (ruleObligations)
import Coverage.Obligations.SpecRule (HuddleSpec, eraRoot)
import Coverage.Obligations.Trail
import Coverage.Report.TermChecker
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

spec = do
  validatorSpec
  referenceSpec

validatorSpec = describe "A custom validator that must fail" $ do
  -- The plutus_v1 scripts of a witness set form a set, whose validator refuses
  -- a repeated script. It also refuses a malformed script, which says nothing
  -- about the set: only a repeat of valid scripts shows the validator's rule.
  it "fails on a repeat of valid entries" $
    failsOnlyAsAWhole conway scriptsFail (witnessSet [TBytes "\1", TBytes "\1"]) `shouldBe` Just True

  it "does not fail on valid entries that differ" $
    failsOnlyAsAWhole conway scriptsFail (witnessSet [TBytes "\1", TBytes "\2"]) `shouldBe` Just False

  it "does not count a failure that a malformed entry causes, even when the entry is repeated" $
    failsOnlyAsAWhole conway scriptsFail (witnessSet [TInt 0, TInt 0]) `shouldBe` Just False

  it "has nothing to say about a value that the sample does not hold" $
    failsOnlyAsAWhole conway scriptsFail (TMap []) `shouldBe` Nothing

-- Cuddle does not run the custom validators, so it accepts an empty nonempty_set. The obligations of the rule refuse it.
referenceSpec = describe "A reference to a rule with a custom validator" $ do
  it "is not met by a value that only the validator refuses" $
    meetsObligations dijkstraChecker "invalid_transactions" (TList []) `shouldBe` False

  it "is met by a value that the validator accepts" $
    meetsObligations dijkstraChecker "invalid_transactions" (TList [TInt 0]) `shouldBe` True

  -- Cuddle refuses it too, but the obligations must refuse it alone.
  it "is not met by a value without the tag of the rule" $
    meetsObligations dijkstraChecker "unit_interval" (TInt 0) `shouldBe` False

  -- data = #6.24(bytes .cbor plutus_data): the bytes must decode, and the value they hold must be a plutus_data.
  it "is not met by embedded bytes that decode to a value of another rule" $ do
    meetsObligations dijkstraChecker "data" (TTagged 24 (TBytes "\x61\x61")) `shouldBe` False
    meetsObligations dijkstraChecker "data" (TTagged 24 (TBytes "\x00")) `shouldBe` True

  -- url = text .size (0 .. 128), and the size of a text string is the length of its UTF-8 encoding.
  it "measures the size of a text string in bytes, not in characters" $ do
    meetsObligations dijkstraChecker "url" (TString (T.replicate 65 "\233")) `shouldBe` False
    meetsObligations dijkstraChecker "url" (TString (T.replicate 64 "\233")) `shouldBe` True

  -- guards = nonempty_set<addr_keyhash> / nonempty_oset<credential>: the two alternatives have the same shape.
  it "is not met by a value of the shared shape that is valid for none of the alternatives" $ do
    meetsObligations dijkstraChecker "guards" (TList [TBytes "\1\2"]) `shouldBe` False
    meetsObligations dijkstraChecker "guards" (TList [TBytes (BS.replicate 28 1)]) `shouldBe` True

-- * HELPERS

dijkstraChecker :: TermChecker
dijkstraChecker = newTermChecker (either error id (eraRoot "dijkstra"))

-- | A witness set holding these plutus_v1 scripts.
witnessSet :: [Term] -> Term
witnessSet scripts = TMap [(TInt 3, TList scripts)]

-- | The obligation that the validator of the plutus_v1 scripts of a witness set fails.
scriptsFail :: Obligation
scriptsFail = case Map.lookup (Name "transaction_witness_set") rules of
  Nothing -> error "conway has no transaction_witness_set"
  Just body ->
    case [o | o <- ruleObligations conway body, obligationKind o == ValidatorFails, renderTrail (obligationPath o) == "map{3}.occur0.value"] of
      [obligation] -> obligation
      other -> error ("expected one obligation, found " <> show (length other))
  where
    CTreeRoot rules = conway

conway :: HuddleSpec
conway = either error id (eraRoot "conway")
