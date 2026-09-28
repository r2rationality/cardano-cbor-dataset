module Coverage.Report.CoveragePage where

import Base.Paths
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Divergence (Divergence (..), WaivedObligation (..), divergenceKindName, divergenceRules, waivedObligation)
import Coverage.Obligations.Obligation (Demand (..), renderKind)
import Coverage.Obligations.Trail (renderTrail)
import Coverage.Report.CoverageData
import Coverage.Report.SpecCoverage
import Data.Text qualified as T
import Data.Traversable (for)
import Generation.Dataset (renderSampleName)
import Generation.EraSpec (renderEra)
import Generation.SampleType (SampleKey (..), renderSampleCategory)
import Path
import Path.IO qualified as PathIO

-- | Write the page from the measurement each corpus carries. Every corpus is measured against the one ledger the build
-- pins, so the pin is read here, once for all of them.
writeCoveragePage :: Path Abs File -> [Path Abs Dir] -> IO ()
writeCoveragePage path corpora = do
  eras <- for corpora requireCorpusCoverage
  pin <- absoluteFile ledgerPinFile >>= readLedgerPin
  PathIO.ensureDir (Path.parent path)
  writeText path (coveragePage pin eras)
  putTextLn $ "Wrote " <> display path

coveragePage :: Maybe LedgerPin -> [CoverageSummary] -> Text
coveragePage pin eras =
  T.unlines $
    [ "<!doctype html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"utf-8\">",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
      "<title>Specification coverage</title>",
      "<style>",
      pageStyle,
      "</style>",
      "</head>",
      "<body>",
      "<h1>Specification coverage</h1>",
      lede pin,
      legend,
      controls eras
    ]
      <> concatMap (eraSection pin) eras
      <> [selectorScript, "</body>", "</html>"]

-- | What the page is, in the fewest words that make the numbers mean something.
--
-- The first mention of the specification is a link, because a reader who wants to know what an obligation was derived
-- from should be one click away from the source rather than hunting for it.
lede :: Maybe LedgerPin -> Text
lede pin =
  "<p class=\"lede\">The "
    <> maybe "HuddleSpec" specificationHref pin
    <> " defines the structure of valid on-chain data. From it we derive "
    <> "<em>obligations</em>: each one names a specific case the dataset should "
    <> "exercise with at least one sample. An obligation may call for a sample that "
    <> "is valid or for one that is invalid. The report below lists every obligation "
    <> "and whether the dataset fulfills it.</p>"
  where
    specificationHref pinned =
      "<a href=\"" <> specificationUrl "conway" pinned <> "\">HuddleSpec</a>"

-- | Which dataset to look at, where there is more than one.
--
-- Every section is in the page and visible by default; the script hides the ones not chosen. That way a reader with no
-- scripting sees all of them rather than an empty page, which is the failure that matters here. | Picking a dataset,
-- and narrowing what the table shows.
--
-- Both are one row because they answer the same question: which rules am I looking at. The filters are checkboxes
-- rather than a mode.
controls :: [CoverageSummary] -> Text
controls eras =
  T.unlines
    [ "<div class=\"controls\">",
      selector eras,
      "<p class=\"pick\"><label class=\"pick-label\" for=\"filter\">Filter</label>",
      "<input id=\"filter\" type=\"search\" placeholder=\"rule name\" autocomplete=\"off\"></p>",
      "<p class=\"toggles\">",
      "<label><input type=\"checkbox\" id=\"only-owing\"> outstanding only</label>",
      "<label><input type=\"checkbox\" id=\"only-divergent\"> divergent only</label>",
      "<span class=\"shown\"></span></p>",
      "</div>"
    ]

selector :: [CoverageSummary] -> Text
selector eras = case eras of
  [only] -> "<p class=\"pick\"><span class=\"pick-label\">Dataset</span><b>" <> escape (renderEra (summaryEra only)) <> "</b></p>"
  _ ->
    T.unlines
      [ "<p class=\"pick\"><label class=\"pick-label\" for=\"dataset\">Dataset</label>",
        "<select id=\"dataset\">",
        T.unlines (map option eras),
        "</select></p>"
      ]
  where
    option era =
      "<option value=\""
        <> escape (renderEra (summaryEra era))
        <> "\">"
        <> escape (renderEra (summaryEra era))
        <> "</option>"

selectorScript :: Text
selectorScript =
  T.unlines
    [ "<script>",
      "(function () {",
      "  document.querySelectorAll('tr.expandable').forEach(function (row) {",
      "    row.addEventListener('click', function (event) {",
      "      if (event.target.closest('a')) return;",
      "      var detail = row.nextElementSibling;",
      "      if (detail && detail.classList.contains('detail')) {",
      "        detail.hidden = !detail.hidden;",
      "        row.classList.toggle('open');",
      "      }",
      "    });",
      "  });",
      "  var pick = document.getElementById('dataset');",
      "  var sections = document.querySelectorAll('section[data-era]');",
      "  function show(era) {",
      "    sections.forEach(function (section) {",
      "      section.hidden = section.dataset.era !== era;",
      "    });",
      "    narrow();",
      "  }",
      "  var filter = document.getElementById('filter');",
      "  var onlyOwing = document.getElementById('only-owing');",
      "  var onlyDivergent = document.getElementById('only-divergent');",
      "  var shown = document.querySelector('.shown');",
      "  function narrow() {",
      "    var needle = filter.value.trim().toLowerCase();",
      "    var visible = 0, total = 0;",
      "    sections.forEach(function (section) {",
      "      if (section.hidden) return;",
      "      section.querySelectorAll('table.rules > tbody > tr:not(.detail)').forEach(function (row) {",
      "        var name = row.cells[0].textContent.trim();",
      "        var standing = row.className.split(' ')[0];",
      "        var keep = !needle || name.toLowerCase().indexOf(needle) !== -1;",
      "        if (keep && onlyOwing.checked) keep = standing === 'owing';",
      "        if (keep && onlyDivergent.checked) keep = row.classList.contains('divergent');",
      "        total += 1;",
      "        if (keep) visible += 1;",
      "        row.hidden = !keep;",
      "        var detail = row.nextElementSibling;",
      "        if (detail && detail.classList.contains('detail') && !keep) {",
      "          detail.hidden = true;",
      "          row.classList.remove('open');",
      "        }",
      "      });",
      "    });",
      "    shown.textContent = visible === total ? '' : visible + ' of ' + total + ' rules';",
      "    document.querySelectorAll('table.obligations > tbody > tr').forEach(function (row) {",
      "      row.hidden = onlyDivergent.checked && !row.classList.contains('waived');",
      "    });",
      "  }",
      "  filter.addEventListener('input', narrow);",
      "  onlyOwing.addEventListener('change', narrow);",
      "  onlyDivergent.addEventListener('change', narrow);",
      "  if (pick) { pick.addEventListener('change', function () { show(pick.value); }); show(pick.value); }",
      "  else { narrow(); }",
      "})();",
      "</script>"
    ]

-- | Where the obligations of this era come from, at the version they came from.
specificationLink :: Text -> LedgerPin -> Text
specificationLink era pin =
  "derived from <a href=\""
    <> specificationUrl era pin
    <> "\">"
    <> escape era
    <> "/HuddleSpec.hs</a> at "
    <> T.take 9 (ledgerCommit pin)

specificationUrl :: Text -> LedgerPin -> Text
specificationUrl era pin =
  "https://github.com/IntersectMBO/cardano-ledger/blob/"
    <> ledgerCommit pin
    <> "/eras/"
    <> era
    <> "/impl/cddl/lib/Cardano/Ledger/"
    <> capitalize era
    <> "/HuddleSpec.hs"
  where
    capitalize name = T.toUpper (T.take 1 name) <> T.drop 1 name

legend :: Text
legend =
  T.unlines
    [ "<dl class=\"legend\">",
      "<dt><span class=\"swatch fulfilled\"></span>fulfilled</dt>",
      "<dd>At least one sample fulfills the obligation.</dd>",
      "<dt><span class=\"swatch outstanding\"></span>outstanding</dt>",
      "<dd>Measured against every candidate sample, and none of them met it.</dd>",
      "<dt><span class=\"swatch waived\"></span>divergent</dt>",
      "<dd>Settled by a divergence between the Haskell node and the specification, with the reason on the row.</dd>",
      "<dt><span class=\"swatch untested\"></span>untested</dt>",
      "<dd>Could not be measured against any sample.</dd>",
      "</dl>"
    ]

eraSection :: Maybe LedgerPin -> CoverageSummary -> [Text]
eraSection pin era =
  [ "<section data-era=\"" <> escape (renderEra (summaryEra era)) <> "\">",
    -- The dataset is named by the control above, so the section states only
    -- what the control cannot: which specification the obligations came from.
    "<p class=\"source\">" <> foldMap (specificationLink (renderEra (summaryEra era))) pin <> "</p>",
    provenance era,
    summaryCards era,
    "<table class=\"rules\">",
    -- One head holding both rows: the totals belong beside the column names,
    -- and a stray row between a closed head and the body is folded into the
    -- body by the browser, where the filter would count it as a rule.
    "<thead><tr><th>rule</th><th class=\"n\">obligations</th><th class=\"n\">fulfilled</th>"
      <> "<th class=\"n\">outstanding</th><th class=\"n\">divergences</th><th class=\"n\">untested</th>"
      <> "<th class=\"bar\"></th></tr>",
    totalsRow era,
    "</thead>",
    "<tbody>"
  ]
    <> concatMap (ruleRow (renderEra (summaryEra era)) (summaryDivergences era)) (sortOn ordering (summaryRules era))
    <> [ "</tbody>",
         "</table>"
       ]
    <> ["</section>"]
  where
    -- Worst first, then by name: a rule that owes samples is what a reader came
    -- for, and the ones already settled can stay at the bottom.
    ordering rule =
      ( countOwing (ruleCounts rule) == 0,
        negate (ruleOutstanding rule + ruleUntested rule),
        ruleName rule
      )

-- | A link to a sample, relative so that it works from a file opened off disk: the page sits in a reports directory
-- beside the corpora.
sampleLink :: Text -> SampleKey -> Text
sampleLink corpus key =
  let sample = unName (sampleKeyRule key) <> "/" <> renderSampleName (sampleKeyName key)
   in "<a href=\"../" <> escape corpus <> "/" <> escape sample <> "\">" <> escape sample <> "</a>"

-- | The numbers a reader wants before any table.
--
-- Rules first, because the specification is the denominator, then obligations, then whether the derivation and the
-- specification ever disagreed. | How much of the corpus someone had to write.
--
-- Small on purpose: it is context for the numbers above it, not one of them.
provenance :: CoverageSummary -> Text
provenance era =
  "<p class=\"provenance\">"
    <> show (summarySamples era)
    <> " samples ("
    <> show (summaryManual era)
    <> " manual = "
    <> show share
    <> "%)</p>"
  where
    share :: Int
    share
      | summarySamples era == 0 = 0
      | otherwise = (100 * summaryManual era) `div` summarySamples era

summaryCards :: CoverageSummary -> Text
summaryCards era =
  T.unlines
    [ "<div class=\"cards\">",
      card (total ruleSettled) stated "obligations fulfilled",
      card fulfilledRules (length (summaryRules era)) "rules fulfilled",
      "</div>"
    ]
  where
    -- The obligations of the measured rules.
    stated = sum (map ruleObligationsNb (summaryRules era))
    total field = sum (map field (summaryRules era))
    fulfilledRules =
      length
        [ rule
        | rule <- summaryRules era,
          countOwing (ruleCounts rule) == 0
        ]
    -- The card is its own bar: the share fulfilled is painted behind the
    -- numbers, so the two headline ratios can be read at a glance and still be
    -- read exactly. A gradient rather than a child element keeps the fill out
    -- of the markup, where it would have to be kept in step with the text.
    card :: Int -> Int -> Text -> Text
    card done out_of label =
      "<div class=\"card\" style=\"--fill:"
        <> show (share done out_of)
        <> "%\"><b>"
        <> show done
        <> " / "
        <> show out_of
        <> "</b><span>"
        <> label
        <> "</span></div>"
    share _ 0 = 0 :: Int
    share done out_of = (100 * done) `div` out_of

ruleRow :: Text -> [Divergence] -> RuleSummary -> [Text]
ruleRow corpus divergences rule =
  [ "<tr class=\""
      <> rowClass
      <> expandable
      <> divergent
      <> "\"><td>"
      <> escape (unName (ruleName rule))
      <> "</td>"
      <> number (ruleObligationsNb rule)
      <> body
      <> "</tr>"
  ]
    <> detailRow corpus divergences rule
  where
    expandable
      | null (ruleObligations rule) = ""
      | otherwise = " expandable"

    -- The divergences about the rule, whether or not they settle one of its obligations.
    divergenceCount = length (filter ((ruleName rule `elem`) . divergenceRules) divergences)
    divergent
      | divergenceCount > 0 = " divergent"
      | otherwise = ""

    counts = ruleCounts rule
    -- Fulfilled counts the waived too: both are obligations with nothing left
    -- to do, and the bar shows how many of them are owed to a divergence
    -- rather than to a sample.
    body =
      number (countFulfilled counts + countWaived counts)
        <> number (countOutstanding counts)
        <> number divergenceCount
        <> number (countUntested counts)
        <> "<td class=\"bar\">"
        <> bar rule
        <> "</td>"
    rowClass
      | countOwing counts == 0 = "complete"
      | otherwise = "owing"
    number value = "<td class=\"n\">" <> show value <> "</td>"

detailRow :: Text -> [Divergence] -> RuleSummary -> [Text]
detailRow corpus divergences rule
  | null (ruleObligations rule) = []
  | otherwise =
      [ "<tr class=\"detail\" hidden><td colspan=\"7\">"
          <> "<pre class=\"cddl\">"
          <> escape (unName (ruleName rule))
          <> " = "
          <> escape (ruleText rule)
          <> "</pre>"
          <> foldMap (unsettledDivergence corpus) (filter settlesNothing divergences)
          <> "<table class=\"obligations\">"
          <> "<thead><tr><th class=\"verdict\">verdict</th><th class=\"category\">case</th>"
          <> "<th class=\"path\">where it is stated</th><th class=\"demand\">what it asks for</th>"
          <> "<th class=\"witness\">witness</th></tr></thead><tbody>"
          <> foldMap (obligationRow corpus divergences (ruleName rule)) (ruleObligations rule)
          <> "</tbody></table></td></tr>"
      ]
  where
    settlesNothing divergence =
      isNothing (waivedObligation (divergenceKind divergence)) && ruleName rule `elem` divergenceRules divergence

-- | A divergence that settles no obligation of the rule, such as a decoder exception, with its reason and samples. It
-- has no obligation row to sit on, so it comes before the obligations.
unsettledDivergence :: Text -> Divergence -> Text
unsettledDivergence corpus divergence =
  "<p class=\"divergence\"><span class=\"why\">"
    <> escape (divergenceKindName (divergenceKind divergence) <> ": " <> divergenceReason divergence)
    <> "</span> "
    <> T.intercalate ", " (sampleLink corpus <$> toList (divergenceSamples divergence))
    <> "</p>"

obligationRow :: Text -> [Divergence] -> Name -> RecordedObligation -> Text
obligationRow corpus divergences rule obligation =
  "<tr class=\""
    <> verdictClass
    <> "\"><td class=\"verdict\">"
    <> escape verdict
    <> "</td><td class=\"category\">"
    <> escape (renderSampleCategory (recordedCategory obligation))
    <> "</td><td class=\"path\">"
    <> escape (renderTrail (recordedPath obligation))
    <> "</td><td class=\"demand\">"
    <> escape (demandText (recordedDemand obligation))
    <> note
    <> "</td><td class=\"witness\">"
    <> witnessLink
    <> "</td></tr>"
  where
    evidence = recordedEvidence obligation
    -- The page names a waived obligation by the divergence that settles it.
    verdict = case evidence of
      WaivedBy _ _ -> "divergent"
      _ -> renderVerdict evidence
    -- The page sits in a reports directory beside the corpora, so a witness is
    -- one level up and then the path the report already prints. Relative so the
    -- link works from a file opened off disk, not only from a server.
    -- The witness already names the rule whose samples settled it, which is not
    -- always the rule this row sits under.
    -- A divergent obligation is witnessed by a sample that shows the divergence.
    witnessLink = case evidence of
      Sampled (Met key) -> sampleLink corpus key
      WaivedBy _ _ -> foldMap (sampleLink corpus . head . divergenceSamples) settling
      _ -> ""
    settling = find settles divergences
    settles divergence = case waivedObligation (divergenceKind divergence) of
      Just named ->
        waivedRule named == rule
          && waivedPath named == recordedPath obligation
          && renderKind (waivedKind named) == demandText (recordedDemand obligation)
      Nothing -> False
    -- A divergence is only as good as the sentence behind it, so the kind and
    -- the sentence are on the row rather than a click away.
    note = case evidence of
      WaivedBy kind why -> "<span class=\"why\">" <> escape (divergenceKindName kind <> ": " <> why) <> "</span>"
      _ -> ""
    verdictClass = case evidence of
      Sampled (Met _) -> "complete"
      WaivedBy _ _ -> "waived"
      Sampled Unmet -> "owing"
      Sampled _ -> "reached"

-- | Three widths that always add to the full bar, so rules can be compared by eye down the column without reading a
-- single number.
bar :: RuleSummary -> Text
bar rule
  | ruleObligationsNb rule == 0 = ""
  | otherwise =
      "<span class=\"track\">"
        <> segment "fulfilled" (ruleFulfilled rule)
        <> segment "outstanding" (ruleOutstanding rule)
        <> segment "waived" (ruleWaived rule)
        <> segment "untested" (ruleUntested rule)
        <> "</span>"
  where
    segment _ 0 = ""
    segment kind value =
      "<span class=\""
        <> kind
        <> "\" style=\"width:"
        <> show (percent value)
        <> "%\"></span>"
    percent value = (100 * value) `div` ruleObligationsNb rule

totalsRow :: CoverageSummary -> Text
totalsRow era =
  "<tr class=\"totals\"><th>total</th>"
    <> number (sum (map ruleObligationsNb (summaryRules era)))
    <> number (total ruleSettled)
    <> number (total ruleOutstanding)
    <> number (length (summaryDivergences era))
    <> number (total ruleUntested)
    <> "<th class=\"bar\"></th></tr>"
  where
    total field = sum (map field (summaryRules era))
    number value = "<th class=\"n\">" <> show value <> "</th>"

escape :: Text -> Text
escape =
  T.replace ">" "&gt;"
    . T.replace "<" "&lt;"
    . T.replace "&" "&amp;"

-- | One stylesheet in the page, because a report that has to be served with a second file beside it stops being
-- something you can mail to someone.
pageStyle :: Text
pageStyle =
  T.unlines
    [ ":root {",
      "  color-scheme: light dark;",
      "  --ink: #16181d; --dim: #5c6370; --rule: #e2e5ea; --ground: #fbfbfc; --panel: #fff;",
      "  --fulfilled: #2f855a; --waived: #2b6cb0; --outstanding: #c05621;",
      "  --untested: #8a94a6; --alarm: #c53030;",
      "  --filled: #d4ece0;",
      -- One measure for every block, so their left and right edges agree.
      "  --page: 1040px;",
      -- One measure for every block on the page, so their edges agree.
      "  --page: 1040px;",
      "}",
      "@media (prefers-color-scheme: dark) {",
      "  :root {",
      "    --ink: #e6e8ec; --dim: #99a1b0; --rule: #2b3038; --ground: #14161a; --panel: #1b1e24;",
      "    --fulfilled: #57b98a; --waived: #6aa9e0; --outstanding: #e08a4e;",
      "    --untested: #6f7988; --alarm: #e06c6c;",
      "    --filled: #21402f;",
      "  }",
      "}",
      "* { box-sizing: border-box; }",
      "body {",
      "  margin: 0; padding: 32px 20px 64px; background: var(--ground); color: var(--ink);",
      "  font: 14px/1.55 ui-sans-serif, -apple-system, 'Segoe UI', Roboto, sans-serif;",
      "}",
      "h1 { font-size: 22px; margin: 0 0 6px; }",
      "h2 { font-size: 17px; margin: 0; }",
      "h1, .lede, .legend, .controls, .source, .cards, table.rules {",
      "  width: 100%; max-width: var(--page); }",
      ".lede { color: var(--dim); margin: 0 0 20px; }",
      ".lede a { color: inherit; }",
      -- Sits just under the dataset control, because it says which
      -- specification that choice selected. The gap goes below it instead, to
      -- separate the two from the report they describe.
      ".source { color: var(--dim); font-size: 12px; margin: 0 0 32px;",
      "  font-family: ui-monospace, SFMono-Regular, monospace; }",
      ".source a { color: inherit; }",
      "section { margin-top: 10px; }",
      "section[hidden] { display: none; }",
      ".controls { display: flex; flex-wrap: wrap; align-items: baseline; gap: 10px 28px;",
      "  margin: 24px 0 0; }",
      ".pick { display: flex; align-items: baseline; gap: 10px; margin: 0; }",
      ".pick input[type=search] { font: inherit; font-size: 15px; padding: 7px 12px;",
      "  border-radius: 7px; border: 1px solid var(--rule); background: var(--panel);",
      "  color: var(--ink); min-width: 190px; }",
      ".toggles { display: flex; align-items: center; gap: 18px; margin: 0; color: var(--dim); }",
      ".toggles label { display: flex; align-items: center; gap: 6px; cursor: pointer; }",
      ".shown { font-variant-numeric: tabular-nums; }",
      "tbody > tr[hidden] { display: none; }",
      ".pick-label { font-size: 15px; font-weight: 600; color: var(--ink); }",
      ".pick b { font-size: 16px; font-family: ui-monospace, SFMono-Regular, monospace; }",
      ".pick select { font: inherit; font-size: 16px; padding: 7px 12px; border-radius: 7px;",
      "  border: 1px solid var(--rule); background: var(--panel); color: var(--ink); }",
      ".pick select:focus-visible { outline: 2px solid var(--fulfilled); outline-offset: 2px; }",
      ".legend { display: grid; grid-template-columns: max-content 1fr; gap: 4px 14px;",
      "  margin: 0 0 8px; padding: 12px 16px; background: var(--panel);",
      "  border: 1px solid var(--rule); border-radius: 8px; }",
      ".legend dd { white-space: nowrap; }",
      ".legend dt { font-weight: 600; white-space: nowrap; }",
      ".legend dd { margin: 0; color: var(--dim); }",
      "@media (max-width: 860px) { .legend dd { white-space: normal; } }",
      ".obligations tr.waived td.verdict { color: var(--waived); }",
      ".provenance { margin: -14px 0 14px; color: var(--dim); font-size: 12px; }",
      ".cddl { margin: 0 0 10px; padding: 8px 10px; background: var(--ground);",
      "  border: 1px solid var(--rule); border-radius: 4px; overflow-x: auto;",
      "  font-size: 12px; line-height: 1.5; white-space: pre-wrap; word-break: break-word; }",
      ".why { display: block; margin-top: 2px; color: var(--dim); }",
      "p.divergence { margin: 0 0 8px; }",
      ".swatch { display: inline-block; width: 10px; height: 10px; border-radius: 2px; margin-right: 7px; }",
      ".swatch.fulfilled { background: var(--fulfilled); }",
      ".swatch.waived { background: var(--waived); }",
      ".swatch.outstanding { background: var(--outstanding); }",
      ".swatch.untested { background: var(--untested); }",
      -- Two cards sharing the row equally, so the two fills can be compared by
      -- length rather than by reading both numbers.
      ".cards { display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
      "  margin-bottom: 16px; }",
      "@media (max-width: 600px) { .cards { grid-template-columns: 1fr; } }",
      ".card { position: relative; border: 1px solid var(--rule); border-radius: 8px;",
      "  padding: 10px 14px 14px; overflow: hidden;",
      "  background: linear-gradient(to right, var(--filled) var(--fill, 0%),",
      "    var(--panel) var(--fill, 0%)); }",
      -- The same green the table bars use, kept off the text: at full strength
      "  -- behind a 12px label it reads at 1.3:1, which is not readable.",
      ".card::after { content: ''; position: absolute; left: 0; bottom: 0; height: 4px;",
      "  width: var(--fill, 0%); background: var(--fulfilled); }",
      ".card b { display: block; font-size: 19px; font-variant-numeric: tabular-nums; }",
      ".card span { color: var(--dim); font-size: 12px; }",
      ".card.alarm { border-color: var(--alarm); }",
      ".card.alarm b { color: var(--alarm); }",
      "table { border-collapse: collapse; width: 100%; background: var(--panel);",
      "  border: 1px solid var(--rule); border-radius: 8px; overflow: hidden; }",
      "th, td { padding: 5px 10px; border-bottom: 1px solid var(--rule); text-align: left; }",
      "thead th { font-size: 12px; font-weight: 600; color: var(--dim); }",
      -- In the head rather than the foot, and stuck there, so the totals stay
      -- in view instead of being a hundred rows down.
      "thead { position: sticky; top: 0; background: var(--panel); z-index: 1; }",
      "tr.totals th { font-weight: 600; color: var(--ink); font-size: 13px;",
      "  border-bottom: 2px solid var(--rule); }",
      "td.n, th.n { text-align: right; font-variant-numeric: tabular-nums; width: 92px; }",
      "td:first-child { font-family: ui-monospace, SFMono-Regular, monospace; }",
      -- Child combinators throughout: a descendant selector would reach into the
      -- obligations table nested in a detail row and bullet its cells too.
      "table.rules > tbody > tr:not(.detail) > td:first-child::before {",
      "  content: '\\2022'; margin-right: 7px; }",
      "table.rules > tbody > tr.complete > td:first-child::before { color: var(--fulfilled); }",
      "table.rules > tbody > tr.owing > td:first-child::before { color: var(--outstanding); }",
      "td.says { color: var(--dim); font-style: italic; }",
      "tr.expandable { cursor: pointer; }",
      "tr.expandable:hover td { background: var(--ground); }",
      "tr.expandable td:first-child::after { content: ' \\25b8'; color: var(--dim); }",
      "tr.expandable.open td:first-child::after { content: ' \\25be'; }",
      "tr.detail > td { padding: 0; background: var(--ground); }",
      "table.obligations { border: none; border-radius: 0; width: 100%; background: none; }",
      "table.obligations td { border-bottom: 1px solid var(--rule); padding: 3px 10px;",
      "  font-size: 12px; vertical-align: top; }",
      "table.obligations th { font-size: 11px; font-weight: 600; color: var(--dim);",
      "  text-transform: uppercase; letter-spacing: .05em; padding: 6px 10px 4px;",
      "  border-bottom: 1px solid var(--rule); text-align: left; }",
      "table.obligations a { color: inherit; }",
      "table.obligations tr:last-child td { border-bottom: none; }",
      "td.verdict { width: 150px; white-space: nowrap; }",
      "tr.complete td.verdict { color: var(--fulfilled); }",
      "tr.owing td.verdict { color: var(--outstanding); }",
      "tr.reached td.verdict { color: var(--untested); }",
      "td.category { width: 58px; color: var(--dim); }",
      "td.path { width: 280px; font-family: ui-monospace, SFMono-Regular, monospace;",
      "  word-break: break-all; }",
      "td.witness { color: var(--dim); font-family: ui-monospace, SFMono-Regular, monospace;",
      "  word-break: break-all; }",
      ".bar { width: 180px; }",
      ".track { display: flex; height: 7px; border-radius: 4px; overflow: hidden; background: var(--rule); }",
      ".track .fulfilled { background: var(--fulfilled); }",
      ".track .outstanding { background: var(--outstanding); }",
      ".track .untested { background: var(--untested); }",
      "@media (max-width: 720px) {",
      "  .bar { display: none; }",
      "  td.n, th.n { width: auto; }",
      "}"
    ]
