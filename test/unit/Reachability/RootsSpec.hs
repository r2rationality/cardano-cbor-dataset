module Reachability.RootsSpec (spec) where

import Reachability (eraReachability, reachabilityStats)
import Test.Hspec

spec :: Spec
spec = describe "Conway root selection" $ do
  it "does not count reachable rules twice for duplicate roots" $
    (reachabilityStats <$> eraReachability "conway" ["transaction_body", "transaction_body"])
      `shouldBe` (reachabilityStats <$> eraReachability "conway" ["transaction_body"])
