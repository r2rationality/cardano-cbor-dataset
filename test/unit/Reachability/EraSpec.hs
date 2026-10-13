module Reachability.EraSpec (spec) where

import Reachability (eraReachability, reachabilityStats)
import Test.Hspec

spec :: Spec
spec = describe "Era selection" $ do
  it "rejects an unsupported era instead of using Conway rules" $
    (reachabilityStats <$> eraReachability "unknown" ["transaction_body"])
      `shouldBe` Left "no Huddle specification for era 'unknown'"
