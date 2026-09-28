module Coverage.Report.CoveragePageSpec where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Divergence (Divergence (..), DivergenceKind (..), WaivedObligation (..))
import Coverage.Obligations.Obligation (Demand (..), ObligationKind (..))
import Coverage.Obligations.Primitive (Primitive (..))
import Coverage.Obligations.Trail (trailOf)
import Coverage.Report.CoverageData
import Coverage.Report.CoveragePage
import Coverage.Report.Regressions
import Coverage.Report.SpecCoverage
import Data.Text qualified as T
import Generation.EraSpec (Era (..))
import Generation.SampleType (SampleCategory (..), SampleKey (..), SampleName (..), SampleType (..))
import Test.Hspec

-- | The coverage page, built from numbers rather than from a corpus.
--
-- Measuring the real datasets takes about a minute, nearly all of it running the specification validator over twenty
-- seven thousand samples. The page itself is a pure function of the counts, so iterating on it does not need any of
-- that: these examples render it from a fixture and finish in milliseconds, which is what makes a watching build useful
-- while the markup is in flux.
--
-- The rendered page is written out beside the test so it can be opened and looked at, which is the part a assertion
-- cannot do.
spec = describe "The coverage page" $ do
  it "shows one section per dataset, and a selector to pick between them" $ do
    page <- render [conway, dijkstra]
    T.count "<section data-era=" page `shouldBe` 2
    page `shouldContainText` "<option value=\"conway\">conway</option>"
    page `shouldContainText` "<option value=\"dijkstra\">dijkstra</option>"

  it "names the dataset without a selector when there is only one" $ do
    page <- render [conway]
    page `shouldNotContainText` "<select"
    page `shouldContainText` "<b>conway</b>"

  it "offers a filter and a narrowing, with a count of what is shown" $ do
    page <- render [conway]
    page `shouldContainText` "<input id=\"filter\" type=\"search\""
    page `shouldContainText` "id=\"only-owing\""
    page `shouldContainText` "id=\"only-divergent\""
    page `shouldContainText` "class=\"shown\""

  -- The label of the divergent only filter is also a link to the divergences table of the dataset on show.
  it "counts the divergences of each rule, and marks a rule that has some" $ do
    page <- render [conway {summaryDivergences = [Divergence DecoderException (coinSample :| []) "the reason"]}]
    page `shouldContainText` "<tr class=\"complete expandable divergent\"><td>coin</td>"
    page `shouldContainText` "<th class=\"n\">divergences</th>"
    -- 20 obligations, 12 fulfilled, 5 outstanding, then the one divergence.
    page `shouldContainText` "<th>total</th><th class=\"n\">20</th><th class=\"n\">12</th><th class=\"n\">5</th><th class=\"n\">1</th>"

  it "shows only the divergent obligations of a rule when the divergent only filter is on" $ do
    page <- render [conway]
    page `shouldContainText` "row.hidden = onlyDivergent.checked && !row.classList.contains('waived');"

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

  it "measures obligations against the obligations of the measured rules" $ do
    page <- render [conway]
    -- 12 fulfilled of 4 + 8 + 8 obligations.
    page `shouldContainText` "<b>12 / 20</b><span>obligations fulfilled</span>"

  it "counts a rule as fulfilled only when it owes nothing at all" $ do
    page <- render [conway]
    -- Only `coin` owes nothing: `redeemers` and `script` still have work.
    page `shouldContainText` "<b>1 / 3</b><span>rules fulfilled</span>"

  it "says nothing about findings, which are a defect report and not coverage" $ do
    page <- render [conway]
    page `shouldNotContainText` "findings"

  it "totals the obligations of the measured rules" $ do
    page <- render [conway]
    page `shouldContainText` "<th>total</th><th class=\"n\">20</th><th class=\"n\">12</th>"

  it "escapes a rule name that is a generic instance" $ do
    page <- render [conway {summaryRules = [measured "%set<transaction_input>" 1 0 0]}]
    page `shouldContainText` "%set&lt;transaction_input&gt;"
    page `shouldNotContainText` "%set<transaction_input>"

  it "calls a waived obligation divergent, in the legend too" $ do
    page <- render [conway]
    page `shouldContainText` "<span class=\"swatch waived\"></span>divergent</dt>"
    page `shouldNotContainText` "</span>waived</dt>"

  -- A reader who sees bytes that the specification accepts in a directory of invalid samples finds the reason on the
  -- obligation that they settle, with a sample that shows the divergence.
  it "shows the kind, the reason and a sample of the divergence on the obligation that it settles" $ do
    let settled = WaivedObligation "coin" (trailOf []) (WrongType PrimArray)
        divergence = Divergence (UnstatedConstraint (Just settled)) (coinSample :| []) "the decoder checks it"
        coin = (measured "coin" 0 0 0) {ruleObligations = [RecordedObligation Reject (trailOf []) (Demand "a value that is not array") (WaivedBy (divergenceKind divergence) "the decoder checks it")]}
    page <- render [conway {summaryRules = [coin], summaryDivergences = [divergence]}]
    page `shouldContainText` "a value that is not array<span class=\"why\">unstated constraint: the decoder checks it</span>"
    page `shouldContainText` "<td class=\"witness\"><a href=\"../conway/coin/manual-invalid/bignum.input.cbor\">"

  -- A divergence that settles no obligation, such as a decoder exception, has no row of its own.
  it "shows a divergence that settles no obligation above the obligations of its rule" $ do
    page <- render [conway {summaryDivergences = [Divergence DecoderException (coinSample :| []) "the indexes must be sorted"]}]
    page `shouldContainText` "<p class=\"divergence\"><span class=\"why\">decoder exception: the indexes must be sorted</span>"
    page `shouldNotContainText` "table class=\"divergences\""

-- * HELPERS

coinSample :: SampleKey
coinSample = SampleKey "coin" (SampleName ManualInvalid "bignum")

unsorted :: SampleKey
unsorted = SampleKey "block" (SampleName ManualInvalid "invalid-transactions-unsorted")

conway :: CoverageSummary
conway =
  CoverageSummary
    { summaryEra = Conway,
      summaryRules =
        [ measured "coin" 4 0 0,
          measured "redeemers" 5 2 1,
          measured "script" 3 3 2
        ],
      summarySamples = 400,
      summaryManual = 12,
      summaryDivergences = [],
      summaryUnmatchedDivergences = [],
      summaryDivergenceMismatches = []
    }

dijkstra :: CoverageSummary
dijkstra = conway {summaryEra = Dijkstra}

measured :: Text -> Int -> Int -> Int -> RuleSummary
measured name fulfilled outstanding untested =
  RuleSummary
    { ruleName = Name name,
      ruleCounts = CoverageCounts fulfilled outstanding 0 untested,
      ruleText = name <> " = uint",
      ruleObligations =
        replicate fulfilled (line (Sampled (Met (SampleKey (Name name) (SampleName ManualInvalid "not-array")))))
          <> replicate outstanding (line (Sampled Unmet))
          <> replicate untested (line (Sampled Undecided))
    }
  where
    line = RecordedObligation Reject (trailOf []) (Demand "a value that is not array")

commit :: Text
commit = "6176e413b2be9880c23cedbdaa899f9752693b22"

-- | Render the page with a fixed ledger pin, so the examples do not depend on the commit the Dockerfile pins today.
render :: [CoverageSummary] -> IO Text
render = pure . coveragePage (Just (LedgerPin commit))

shouldContainText :: (HasCallStack) => Text -> Text -> Expectation
shouldContainText page fragment =
  unless (fragment `T.isInfixOf` page) $
    expectationFailure (toString ("the page does not contain " <> fragment))

shouldNotContainText :: (HasCallStack) => Text -> Text -> Expectation
shouldNotContainText page fragment =
  when (fragment `T.isInfixOf` page) $
    expectationFailure (toString ("the page contains " <> fragment))
