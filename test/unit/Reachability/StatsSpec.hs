module Reachability.StatsSpec (spec) where

import Reachability (ReachabilityStats (..), eraReachability, reachabilityStats)
import Test.Hspec

spec :: Spec
spec = describe "Conway coverage baseline" $ do
  it "reaches 128 of 167 rules from transaction_body in the pinned ledger" $
    (reachabilityStats <$> eraReachability "conway" ["transaction_body"])
      `shouldBe` Right (ReachabilityStats 167 128 39 0)

  it "reports no coverage when no roots are selected" $
    (reachabilityStats <$> eraReachability "conway" [])
      `shouldBe` Right (ReachabilityStats 167 0 167 0)
