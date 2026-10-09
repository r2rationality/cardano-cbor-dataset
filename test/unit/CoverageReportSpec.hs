-- | The coverage page, built from numbers rather than from a corpus.
--
-- Measuring the real datasets takes about a minute, nearly all of it running
-- the specification validator over twenty seven thousand samples. The page
-- itself is a pure function of the counts, so iterating on it does not need any
-- of that: these examples render it from a fixture and finish in milliseconds,
-- which is what makes a watching build useful while the markup is in flux.
--
-- The rendered page is written out beside the test so it can be opened and
-- looked at, which is the part a assertion cannot do.
module CoverageReportSpec (spec) where

import CoverageReport
  ( EraCoverage (..),
    LedgerPin (..),
    ObligationLine (..),
    RuleCoverage (..),
    Standing (..),
    writeCoveragePage,
  )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "The coverage page" $ do
  it "shows one section per dataset, and a selector to pick between them" $ do
    page <- render [conway, dijkstra]
    T.count "<section data-era=" page `shouldBe` 2
    page `shouldContainText` "<option value=\"conway\">conway-123-100</option>"
    page `shouldContainText` "<option value=\"dijkstra\">dijkstra-123-100</option>"

  it "names the dataset without a selector when there is only one" $ do
    page <- render [conway]
    page `shouldNotContainText` "<select"
    page `shouldContainText` "conway-123-100"

  it "offers a filter and a narrowing, with a count of what is shown" $ do
    page <- render [conway]
    page `shouldContainText` "<input id=\"filter\" type=\"search\""
    page `shouldContainText` "id=\"only-owing\""
    page `shouldContainText` "class=\"shown\""

  it "counts rule rows only, never the obligation rows nested inside them" $ do
    page <- render [conway]
    page `shouldContainText` "table.rules > tbody > tr:not(.detail)"

  it "hides a narrowed row rather than dimming it, so the table stays short" $ do
    page <- render [conway]
    page `shouldContainText` "tbody > tr[hidden] { display: none; }"

  it "links each dataset to the specification it was derived from, at the pinned commit" $ do
    page <- render [conway]
    page
      `shouldContainText` ( "https://github.com/IntersectMBO/cardano-ledger/blob/"
                              <> commit
                              <> "/eras/conway/impl/cddl/lib/Cardano/Ledger/Conway/HuddleSpec.hs"
                          )

  it "measures obligations against everything the specification asks for" $ do
    page <- render [conway]
    -- 12 fulfilled of 12 + 5 + 3 stated by measured rules, plus 15 and 5 stated
    -- by rules nothing measured: a rule with no corpus still owes what it states.
    page `shouldContainText` "<b>12 / 40</b><span>obligations fulfilled</span>"

  it "counts a rule as fulfilled only when it owes nothing at all" $ do
    page <- render [conway]
    -- Only `coin` owes nothing: `redeemers` and `script` still have work, and
    -- the two with no corpus are not fulfilled, they are unknown.
    page `shouldContainText` "<b>1 / 5</b><span>rules fulfilled</span>"

  it "says why a rule has no numbers, rather than showing it as all zeros" $ do
    page <- render [conway]
    page `shouldContainText` "exercised inside other rules, never measured"
    page `shouldContainText` "no corpus root reaches this rule"

  it "says nothing about findings, which are a defect report and not coverage" $ do
    page <- render [conway]
    page `shouldNotContainText` "findings"

  it "totals what the specification states, not only what was measured" $ do
    page <- render [conway]
    page `shouldContainText` "<th>total</th><th class=\"n\">40</th><th class=\"n\">12</th>"

  it "escapes a rule name that is a generic instance" $ do
    page <- render [conway {eraRules = [measured "%set<transaction_input>" 1 0 0]}]
    page `shouldContainText` "%set&lt;transaction_input&gt;"
    page `shouldNotContainText` "%set<transaction_input>"

-- FIXTURES

conway :: EraCoverage
conway =
  EraCoverage
    { eraName = "conway",
      eraCorpus = "conway-123-100",
      eraRules =
        [ measured "coin" 4 0 0,
          measured "redeemers" 5 2 1,
          measured "script" 3 3 2,
          RuleCoverage "transaction_id" 15 Reached Nothing [],
          RuleCoverage "language" 5 Unreached Nothing []
        ],
      eraSamples = 400,
      eraHandWritten = 12,
      eraFindings = 0
    }

dijkstra :: EraCoverage
dijkstra = conway {eraName = "dijkstra", eraCorpus = "dijkstra-123-100"}

measured :: Text -> Int -> Int -> Int -> RuleCoverage
measured name fulfilled outstanding untested =
  RuleCoverage
    { ruleName = name,
      ruleStated = fulfilled + outstanding + untested,
      ruleStanding = Measured fulfilled outstanding 0 untested,
      ruleText = Just (name <> " = uint"),
      ruleDetail =
        replicate fulfilled (line "fulfilled")
          <> replicate outstanding (line "outstanding")
          <> replicate untested (line "untested, no path")
    }
  where
    line verdict = ObligationLine "reject" "." "a value that is not array" verdict Nothing Nothing

commit :: Text
commit = "6176e413b2be9880c23cedbdaa899f9752693b22"

-- | Render through the real writer, so the file on disk is what is asserted
-- against rather than a string the test happened to build.
render :: [EraCoverage] -> IO Text
render eras = withSystemTempDirectory "coverage-page" $ \directory -> do
  let path = directory </> "coverage.html"
  writeCoveragePage path (Just (LedgerPin commit)) eras
  TIO.readFile path

shouldContainText :: (HasCallStack) => Text -> Text -> Expectation
shouldContainText page fragment =
  unless (fragment `T.isInfixOf` page) $
    expectationFailure (toString ("the page does not contain " <> fragment))

shouldNotContainText :: (HasCallStack) => Text -> Text -> Expectation
shouldNotContainText page fragment =
  when (fragment `T.isInfixOf` page) $
    expectationFailure (toString ("the page contains " <> fragment))
