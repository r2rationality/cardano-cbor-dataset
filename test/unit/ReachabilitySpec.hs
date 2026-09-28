{-# LANGUAGE OverloadedStrings #-}

module ReachabilitySpec (spec) where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Reachability qualified as R
import Test.Hspec

spec :: Spec
spec = describe "Conway reachability" $ do
  it "reaches the Conway specification from its declared roots" $ do
    result <- conwayFrom conwayRoots
    R.missingRoots result `shouldBe` Set.empty
    -- huddle_root_defs is a synthetic parent of the declared roots.
    R.unreached result `shouldBe` Set.singleton (Name "huddle_root_defs")
    R.fromRoots result
      `shouldBe` Set.delete (Name "huddle_root_defs") (Map.keysSet (R.all result))

  it "traverses transaction_body dependencies, including wrapped generators" $ do
    result <- conwayFrom ["transaction_body"]
    R.missingRoots result `shouldBe` Set.empty
    for_
      [ "transaction_body",
        "transaction_input",
        "transaction_output",
        "value",
        "coin",
        "policy_id",
        "asset_name",
        "plutus_data",
        "native_script",
        "script_pubkey",
        "gov_action"
      ]
      $ \name ->
        R.fromRoots result `shouldSatisfy` Set.member (Name name)
    R.unreached result `shouldSatisfy` Set.member (Name "block")

  it "reports an unknown root instead of claiming coverage" $ do
    result <- conwayFrom ["TransactionBody"]
    R.missingRoots result `shouldBe` Set.singleton (Name "TransactionBody")
    R.fromRoots result `shouldBe` Set.empty
    R.unreached result `shouldBe` Map.keysSet (R.all result)

-- HELPERS

conwayFrom :: [String] -> IO R.Reachability
conwayFrom roots = case R.eraReachability "conway" roots of
  Left message -> expectationFailure message >> fail message
  Right result -> pure result

-- Declared by conwayCDDL in the pinned Cardano Ledger checkout.
conwayRoots :: [String]
conwayRoots =
  [ "asset_name",
    "block",
    "certificate",
    "kes_signature",
    "language",
    "policy_id",
    "potential_languages",
    "redeemer",
    "signkey_kes",
    "transaction"
  ]
