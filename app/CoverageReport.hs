{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A page showing how much of each era's specification its corpus fulfils,
-- and a baseline that stops the number going backwards.
--
-- The page is for reading and the baseline is for diffing, so both are written
-- from one measurement rather than computed twice. A regression is coverage
-- lost, never coverage not yet gained: deriving a new obligation raises what a
-- rule owes without lowering what it has met, so work on the derivation never
-- trips the gate, while a sample that stops counting always does.
module CoverageReport
  ( measureCoverage,
    writeCoveragePage,
    writeCoverageData,
    readCoverageData,
    EraCoverage (..),
    RuleCoverage (..),
    ObligationLine (..),
    Standing (..),
    LedgerPin (..),
    readLedgerPin,
    Regression (..),
    readBaseline,
    writeBaseline,
    regressionsAgainst,
    renderRegression,
  )
where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value,
    eitherDecodeFileStrict',
    object,
    (.=),
  )
import Data.Aeson.Encode.Pretty
  ( Config (confCompare, confIndent, confTrailingNewline),
    Indent (Spaces),
    defConfig,
    encodePretty',
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withObject, (.:))
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isHexDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import ObligationCoverage
  ( Coverage (..),
    Judged (..),
    Measurement (..),
    Report (..),
    Status (..),
    measureDataset,
    renderCause,
    specificationRules,
    waiverName,
  )
import Obligations
  ( obligationCategory,
    obligationKind,
    obligationOrigin,
    obligationPath,
    renderCategory,
    renderKind,
    renderPath,
    renderRule,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

-- MEASURING

-- | How the corpus stands towards one rule of the specification.
data Standing
  = -- | The rule has samples of its own, so every obligation it states has been
    -- put to them one by one.
    Measured !Int !Int !Int !Int
  | -- | No samples of its own, but a corpus root reaches it, so its values are
    -- decoded inside whatever contains them. Exercised, never measured.
    Reached
  | -- | Nothing in the corpus reaches it. The only standing that is a hole.
    Unreached
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | One rule of the specification, and what the corpus knows about it.
data RuleCoverage = RuleCoverage
  { ruleName :: !Text,
    -- | Obligations the rule states, whether or not anything measured them.
    ruleStated :: !Int,
    ruleStanding :: !Standing,
    -- | The rule as CDDL, from the resolved tree, so the names in it are the
    -- names the page lists. Absent where nothing was measured.
    ruleText :: !(Maybe Text),
    -- | Every obligation the rule states, with what became of it. Empty where
    -- nothing measured them, since there would be no verdict to show.
    ruleDetail :: ![ObligationLine]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | One obligation as a row: where it is stated, what it asks for, and what
-- the corpus had to say about it.
data ObligationLine = ObligationLine
  { lineCategory :: !Text,
    linePath :: !Text,
    lineDemand :: !Text,
    lineVerdict :: !Text,
    lineWitness :: !(Maybe Text),
    -- | Why a waived obligation is not enforced here, and where it is enforced
    -- instead. Absent on every other verdict.
    lineNote :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

ruleFulfilled :: RuleCoverage -> Int
ruleFulfilled rule = case ruleStanding rule of
  Measured fulfilled _ _ _ -> fulfilled
  _ -> 0

ruleOutstanding :: RuleCoverage -> Int
ruleOutstanding rule = case ruleStanding rule of
  Measured _ outstanding _ _ -> outstanding
  _ -> 0

-- | Obligations the specification states, that the decoder for this rule does
-- not enforce, with a recorded reason naming where it is enforced instead.
ruleWaived :: RuleCoverage -> Int
ruleWaived rule = case ruleStanding rule of
  Measured _ _ waived _ -> waived
  _ -> 0

ruleUntested :: RuleCoverage -> Int
ruleUntested rule = case ruleStanding rule of
  Measured _ _ _ untested -> untested
  _ -> 0

-- | Obligations with nothing left to do: met by a sample, or waived because the
-- decoder for this rule does not enforce them and the reason beside the sample
-- says so.
--
-- What the headline counts, and deliberately not what the baseline counts: a
-- waiver is a decision taken rather than coverage gained, so letting one into
-- 'ruleFulfilled' would let a waiver hide a sample that stopped working.
ruleSettled :: RuleCoverage -> Int
ruleSettled rule = ruleFulfilled rule + ruleWaived rule

-- | One era's corpus, measured against the whole of its specification.
data EraCoverage = EraCoverage
  { eraName :: !Text,
    eraCorpus :: !Text,
    eraRules :: ![RuleCoverage],
    -- | Every sample the corpus holds, and how many of them were written by
    -- hand. Generation reaches most of what a specification asks for; what it
    -- cannot reach has to be written, and the share says how much of the
    -- corpus rests on someone having thought of a case.
    eraSamples :: !Int,
    eraHandWritten :: !Int,
    eraFindings :: !Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Measure one era's corpus, given the rules its generator is able to root.
--
-- The root list is the difference between a rule the corpus has not covered yet
-- and one it can never cover. A rule with no root of its own is measured only
-- where it is used, so unless some other rule borrows its obligations there is
-- nothing to measure and it is left out of the page entirely rather than
-- swelling the denominator with constraints nobody will ever answer.
measureCoverage :: Text -> FilePath -> [Text] -> IO EraCoverage
measureCoverage era corpusDir roots = do
  measured <- measureDataset Nothing (toString era) corpusDir
  let CTreeRoot definitions = measuredRoot measured
      reports = measuredRules measured
      byName =
        Map.fromList
          [ (unName (reportRule' report), (standingOf report, detailOf report))
          | report <- reports
          ]
      -- A rule that can never have samples of its own is still measured, at
      -- every site that carries it. Those verdicts are gathered back here and
      -- reported under the rule they belong to, so its row says whether its
      -- obligations are met rather than that nothing looked.
      borrowed =
        Map.fromListWith
          (<>)
          [ (unName origin, [entry])
          | report <- reports,
            entry <- reportCoverage report,
            Just origin <- [obligationOrigin (coverageObligation entry)]
          ]
      judged = map judgedName (concatMap reportJudged reports)
      -- A sample names the directory it came from, and only the hand written
      -- categories are not generated.
      handWritten name =
        any (`T.isPrefixOf` name) ["manual-valid/", "manual-invalid/", "verification-deferred/"]
      standingFor name reached
        | Just own <- Map.lookup name byName = own
        | Just entries <- Map.lookup name borrowed =
            (standingOfEntries entries, map (lineOf (unName (Name name))) entries)
        | otherwise = (if reached then Reached else Unreached, [])
      -- A rule is on the page when the corpus could speak for it: it has a root
      -- of its own, whether or not samples exist yet, or another rule carries
      -- its obligations. Anything else would be stated and never answered.
      rootNames = Set.fromList roots
      measurable name = Set.member name rootNames || Map.member name borrowed
  pure
    EraCoverage
      { eraName = era,
        eraCorpus = toText (lastSegment corpusDir),
        eraRules =
          [ RuleCoverage
              { ruleName = name,
                -- What a rule owes is what was measured for it, which is the
                -- derived count for all but a rule carried by others: that one
                -- states its obligations once and answers them separately at
                -- every carrier, so counting the derivation would leave the row
                -- claiming more met than it asks for.
                ruleStated = if null detail then stated else length detail,
                ruleStanding = standing,
                ruleText = [renderRule definitions body | not (null detail), Just body <- [Map.lookup (Name name) definitions]] & viaNonEmpty head,
                ruleDetail = detail
              }
          | (name, stated, reached) <-
              specificationRules (measuredRoot measured) (measuredReachability measured),
            measurable name,
            let (standing, detail) = standingFor name reached
          ],
        eraSamples = length judged,
        eraHandWritten = length (filter handWritten judged),
        eraFindings = sum (map (length . reportFindings) reports)
      }

detailOf :: Report -> [ObligationLine]
detailOf report = map (lineOf (unName (reportRule' report))) (reportCoverage report)

-- | One obligation as a row. The carrier is the rule whose samples settled it,
-- which is the rule itself except for an obligation measured where it is used.
lineOf :: Text -> Coverage -> ObligationLine
lineOf carrier entry =
  ObligationLine
    { lineCategory = renderCategory (obligationCategory (obligationKind obligation)),
      linePath = renderPath (obligationPath obligation),
      lineDemand = renderKind (obligationKind obligation),
      lineVerdict = verdict (coverageStatus entry),
      lineWitness = (\sample -> carrier <> "/" <> sample) <$> viaNonEmpty head (coverageWitnesses entry),
      lineNote = case coverageStatus entry of
        Waived _ why -> Just why
        _ -> Nothing
    }
  where
    obligation = coverageObligation entry
    verdict = \case
      Fulfilled -> "fulfilled"
      Waived kind _ -> waiverName kind
      Outstanding -> "outstanding"
      Untested cause -> "untested, " <> renderCause cause

standingOf :: Report -> Standing
standingOf = standingOfEntries . reportCoverage

standingOfEntries :: [Coverage] -> Standing
standingOfEntries entries =
  Measured (tally (== Fulfilled)) (tally (== Outstanding)) (tally waived) (tally untested)
  where
    tally predicate = length (filter (predicate . coverageStatus) entries)
    waived = \case
      Waived _ _ -> True
      _ -> False
    untested = \case
      Untested _ -> True
      _ -> False

lastSegment :: FilePath -> FilePath
lastSegment = toString . T.takeWhileEnd (/= '/') . T.dropWhileEnd (== '/') . toText

-- THE BASELINE

-- | Coverage lost since the baseline was written.
data Regression
  = -- | A rule fulfils fewer obligations than it did.
    FewerFulfilled !Text !Text !Int !Int
  | -- | A rule the baseline knows is no longer measured at all.
    RuleGone !Text !Text
  | -- | The derivation and the specification disagree about a sample. Never a
    -- coverage number, always a defect, so it fails the run on its own.
    Contradicted !Text !Int
  deriving (Eq, Show)

renderRegression :: Regression -> Text
renderRegression = \case
  FewerFulfilled era rule before after ->
    era
      <> "/"
      <> rule
      <> ": fulfils "
      <> show after
      <> " obligations where it fulfilled "
      <> show before
  RuleGone era rule -> era <> "/" <> rule <> ": no longer measured"
  Contradicted era found ->
    era <> ": " <> show found <> " samples the specification accepts but the derivation rejects"

-- | What a new measurement lost against a recorded one.
--
-- Only losses. A rule the baseline has never seen is new coverage, and a rule
-- that owes more than it did has had obligations derived for it, which is the
-- derivation getting sharper rather than the corpus getting worse.
regressionsAgainst :: Map Text (Map Text Int) -> [EraCoverage] -> [Regression]
regressionsAgainst baseline eras =
  [ regression
  | era <- eras,
    regression <- findings era <> gone era <> fewer era
  ]
  where
    recorded era = fromMaybe mempty (Map.lookup (eraName era) baseline)
    measuredNow era = Map.fromList [(ruleName rule, ruleFulfilled rule) | rule <- eraRules era]

    findings era = [Contradicted (eraName era) found | let found = eraFindings era, found > 0]

    gone era =
      [ RuleGone (eraName era) rule
      | (rule, _) <- Map.toList (recorded era),
        not (Map.member rule (measuredNow era))
      ]

    fewer era =
      [ FewerFulfilled (eraName era) rule before after
      | (rule, before) <- Map.toList (recorded era),
        Just after <- [Map.lookup rule (measuredNow era)],
        after < before
      ]

-- | The recorded coverage, or nothing where no baseline has been written yet.
readBaseline :: FilePath -> IO (Maybe (Map Text (Map Text Int)))
readBaseline path = do
  present <- doesFileExist path
  if not present
    then pure Nothing
    else do
      decoded <- eitherDecodeFileStrict' path
      case decoded >>= parseEither parseBaseline of
        Left message -> die $ "cannot read the coverage baseline '" <> path <> "': " <> message
        Right baseline -> pure (Just baseline)
  where
    parseBaseline = withObject "baseline" $ \top -> do
      eras <- top .: "eras"
      traverse (traverse (.: "fulfilled")) (asMap (asMap <$> eras))
    asMap = Map.fromList . map (first Key.toText) . KeyMap.toList

writeBaseline :: FilePath -> [EraCoverage] -> IO ()
writeBaseline path eras = do
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (encodePretty' baselineFormat (baselineValue eras))

-- | Rules in name order and one entry per line, so two baselines diff cleanly.
baselineFormat :: Config
baselineFormat =
  defConfig
    { confIndent = Spaces 2,
      confCompare = compare,
      confTrailingNewline = True
    }

baselineValue :: [EraCoverage] -> Value
baselineValue eras =
  object
    [ "eras"
        .= object
          [ Key.fromText (eraName era)
              .= object [Key.fromText (ruleName rule) .= ruleEntry rule | rule <- eraRules era]
          | era <- eras
          ]
    ]
  where
    ruleEntry rule =
      object
        [ "fulfilled" .= ruleFulfilled rule,
          "outstanding" .= ruleOutstanding rule,
          "untested" .= ruleUntested rule
        ]

-- THE MEASUREMENT ON DISK

-- | Everything a page is built from, written out beside it.
--
-- Measuring a corpus takes minutes and rendering it takes none, so the two are
-- kept apart: the page can be rebuilt from the last measurement while a new one
-- is still running, and anything else that wants these numbers can read them
-- without running the measurement itself.
writeCoverageData :: FilePath -> Maybe LedgerPin -> [EraCoverage] -> IO ()
writeCoverageData path pin eras = do
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (encodePretty' baselineFormat (object ["pin" .= pin, "eras" .= eras]))

readCoverageData :: FilePath -> IO (Maybe LedgerPin, [EraCoverage])
readCoverageData path = do
  decoded <- eitherDecodeFileStrict' path
  case decoded >>= parseEither parse of
    Left message -> die $ "cannot read the coverage data '" <> path <> "': " <> message
    Right result -> pure result
  where
    parse = withObject "coverage" $ \top -> (,) <$> top .: "pin" <*> top .: "eras"

-- THE PAGE

writeCoveragePage :: FilePath -> Maybe LedgerPin -> [EraCoverage] -> IO ()
writeCoveragePage path pin eras = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (coveragePage pin eras)

coveragePage :: Maybe LedgerPin -> [EraCoverage] -> Text
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
-- The first mention of the specification is a link, because a reader who wants
-- to know what an obligation was derived from should be one click away from the
-- source rather than hunting for it.
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
-- Every section is in the page and visible by default; the script hides the
-- ones not chosen. That way a reader with no scripting sees all of them rather
-- than an empty page, which is the failure that matters here.
-- | Picking a dataset, and narrowing what the table shows.
--
-- Both are one row because they answer the same question: which rules am I
-- looking at. The filters are checkboxes rather than a mode, since wanting to
-- see what is owed and what was never measured at once is the common case.
controls :: [EraCoverage] -> Text
controls eras =
  T.unlines
    [ "<div class=\"controls\">",
      selector eras,
      "<p class=\"pick\"><label class=\"pick-label\" for=\"filter\">Filter</label>",
      "<input id=\"filter\" type=\"search\" placeholder=\"rule name\" autocomplete=\"off\"></p>",
      "<p class=\"toggles\">",
      "<label><input type=\"checkbox\" id=\"only-owing\"> outstanding only</label>",
      "<span class=\"shown\"></span></p>",
      "</div>"
    ]

selector :: [EraCoverage] -> Text
selector eras = case eras of
  [only] -> "<p class=\"pick\"><span class=\"pick-label\">Dataset</span><b>" <> escape (eraCorpus only) <> "</b></p>"
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
        <> escape (eraName era)
        <> "\">"
        <> escape (eraCorpus era)
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
      "  }",
      "  filter.addEventListener('input', narrow);",
      "  onlyOwing.addEventListener('change', narrow);",
      "  if (pick) { pick.addEventListener('change', function () { show(pick.value); }); show(pick.value); }",
      "  else { narrow(); }",
      "})();",
      "</script>"
    ]

-- | The commit of the ledger the obligations were derived from.
--
-- Carried rather than assumed, because a link to a branch would drift: the
-- specification moves, and a page that claims to measure one version while
-- pointing at another is worse than a page with no link at all.
newtype LedgerPin = LedgerPin {ledgerCommit :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The pin as the repository records it.
--
-- The Dockerfile is where the commit is written down, so it is read rather
-- than copied: a second copy is a second thing to forget.
readLedgerPin :: FilePath -> IO (Maybe LedgerPin)
readLedgerPin path = do
  present <- doesFileExist path
  if not present
    then pure Nothing
    else do
      contents <- TIO.readFile path
      pure $
        viaNonEmpty
          head
          [ LedgerPin commit
          | line <- T.lines contents,
            Just rest <- [T.stripPrefix "ARG LEDGER_COMMIT=" (T.strip line)],
            let commit = T.takeWhile isHexDigit rest,
            T.length commit == 40
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
      "<dt><span class=\"swatch waived\"></span>verification by parent decoder</dt>",
      "<dd>Outstanding obligation covered via another decoder.</dd>",
      "<dt><span class=\"swatch waived\"></span>verification by the ledger</dt>",
      "<dd>The ledger refuses these bytes where the specification validator accepts them,",
      " so no sample can witness the obligation.</dd>",
      "<dt><span class=\"swatch waived\"></span>incorrect specification</dt>",
      "<dd>The specification asks for something no decoder enforces, and is judged wrong",
      " to ask it.</dd>",
      "<dt><span class=\"swatch untested\"></span>untested</dt>",
      "<dd>Could not be measured against any sample.</dd>",
      "</dl>"
    ]

eraSection :: Maybe LedgerPin -> EraCoverage -> [Text]
eraSection pin era =
  [ "<section data-era=\"" <> escape (eraName era) <> "\">",
    -- The dataset is named by the control above, so the section states only
    -- what the control cannot: which specification the obligations came from.
    "<p class=\"source\">" <> foldMap (specificationLink (eraName era)) pin <> "</p>",
    provenance era,
    summaryCards era,
    "<table class=\"rules\">",
    -- One head holding both rows: the totals belong beside the column names,
    -- and a stray row between a closed head and the body is folded into the
    -- body by the browser, where the filter would count it as a rule.
    "<thead><tr><th>rule</th><th class=\"n\">obligations</th><th class=\"n\">fulfilled</th>"
      <> "<th class=\"n\">outstanding</th><th class=\"n\">waived</th><th class=\"n\">untested</th>"
      <> "<th class=\"bar\"></th></tr>",
    totalsRow era,
    "</thead>",
    "<tbody>"
  ]
    <> concatMap (ruleRow (eraCorpus era)) (sortOn ordering (eraRules era))
    <> [ "</tbody>",
         "</table>",
         "</section>"
       ]
  where
    -- Worst first, and within a standing by name: a rule that owes samples is
    -- what a reader came for, a rule nothing reaches is the next thing to fix,
    -- and the ones already settled can stay at the bottom.
    ordering rule =
      ( case ruleStanding rule of
          Measured _ outstanding _ untested
            | outstanding + untested > 0 -> 0 :: Int
            | otherwise -> 3
          Unreached -> 1
          Reached -> 2,
        negate (ruleOutstanding rule + ruleUntested rule),
        ruleName rule
      )

-- | The numbers a reader wants before any table.
--
-- Rules first, because the specification is the denominator, then obligations,
-- then whether the derivation and the specification ever disagreed.
-- | How much of the corpus someone had to write.
--
-- Small on purpose: it is context for the numbers above it, not one of them.
provenance :: EraCoverage -> Text
provenance era =
  "<p class=\"provenance\">"
    <> show (eraSamples era)
    <> " samples ("
    <> show (eraHandWritten era)
    <> " manual = "
    <> show share
    <> "%)</p>"
  where
    share :: Int
    share
      | eraSamples era == 0 = 0
      | otherwise = (100 * eraHandWritten era) `div` eraSamples era

summaryCards :: EraCoverage -> Text
summaryCards era =
  T.unlines
    [ "<div class=\"cards\">",
      card (total ruleSettled) stated "obligations fulfilled",
      card fulfilledRules (length (eraRules era)) "rules fulfilled",
      "</div>"
    ]
  where
    -- Everything the specification asks for, not only what has samples: a rule
    -- with no corpus still owes what it states, and leaving it out of the
    -- denominator would make the dataset look finished while it is not.
    stated = sum (map ruleStated (eraRules era))
    total field = sum (map field (eraRules era))
    fulfilledRules =
      length
        [ rule
        | rule <- eraRules era,
          Measured _ outstanding _ untested <- [ruleStanding rule],
          outstanding + untested == 0
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

ruleRow :: Text -> RuleCoverage -> [Text]
ruleRow corpus rule =
  [ "<tr class=\""
      <> rowClass
      <> expandable
      <> "\"><td>"
      <> escape (ruleName rule)
      <> "</td>"
      <> number (ruleStated rule)
      <> body
      <> "</tr>"
  ]
    <> detailRow corpus rule
  where
    expandable
      | null (ruleDetail rule) = ""
      | otherwise = " expandable"

    body = case ruleStanding rule of
      Measured fulfilled outstanding waived untested ->
        number fulfilled
          <> number outstanding
          <> number waived
          <> number untested
          <> "<td class=\"bar\">"
          <> bar rule
          <> "</td>"
      -- Nothing to put in the counted columns, because nothing was measured.
      -- Saying so once, across them, is honest where a row of zeros would read
      -- as a rule that failed everything.
      Reached -> "<td class=\"says\" colspan=\"5\">exercised inside other rules, never measured</td>"
      Unreached -> "<td class=\"says\" colspan=\"5\">no corpus root reaches this rule</td>"
    rowClass = case ruleStanding rule of
      Measured _ outstanding _ untested
        | outstanding + untested == 0 -> "complete"
        | otherwise -> "owing"
      Reached -> "reached"
      Unreached -> "unreached"
    number value = "<td class=\"n\">" <> show value <> "</td>"

detailRow :: Text -> RuleCoverage -> [Text]
detailRow corpus rule
  | null (ruleDetail rule) = []
  | otherwise =
      [ "<tr class=\"detail\" hidden><td colspan=\"7\">"
          <> foldMap (\text -> "<pre class=\"cddl\">" <> escape (ruleName rule) <> " = " <> escape text <> "</pre>") (ruleText rule)
          <> "<table class=\"obligations\">"
          <> "<thead><tr><th class=\"verdict\">verdict</th><th class=\"category\">case</th>"
          <> "<th class=\"path\">where it is stated</th><th class=\"demand\">what it asks for</th>"
          <> "<th class=\"witness\">witness</th></tr></thead><tbody>"
          <> foldMap (obligationRow corpus) (ruleDetail rule)
          <> "</tbody></table></td></tr>"
      ]

obligationRow :: Text -> ObligationLine -> Text
obligationRow corpus line =
  "<tr class=\""
    <> verdictClass
    <> "\"><td class=\"verdict\">"
    <> escape (lineVerdict line)
    <> "</td><td class=\"category\">"
    <> escape (lineCategory line)
    <> "</td><td class=\"path\">"
    <> escape (linePath line)
    <> "</td><td class=\"demand\">"
    <> escape (lineDemand line)
    <> foldMap note (lineNote line)
    <> "</td><td class=\"witness\">"
    <> foldMap witnessLink (lineWitness line)
    <> "</td></tr>"
  where
    -- The page sits in a reports directory beside the corpora, so a witness is
    -- one level up and then the path the report already prints. Relative so the
    -- link works from a file opened off disk, not only from a server.
    -- The witness already names the rule whose samples settled it, which is not
    -- always the rule this row sits under.
    witnessLink sample =
      "<a href=\"../" <> escape corpus <> "/" <> escape sample <> "\">" <> escape sample <> "</a>"
    -- A waiver is only as good as the sentence behind it, so the sentence is on
    -- the row rather than a click away.
    note why = "<span class=\"why\">" <> escape why <> "</span>"
    verdictClass
      | lineVerdict line == "fulfilled" = "complete"
      | lineVerdict line `elem` map waiverName [minBound .. maxBound] = "waived"
      | lineVerdict line == "outstanding" = "owing"
      | otherwise = "reached"

-- | Three widths that always add to the full bar, so rules can be compared by
-- eye down the column without reading a single number.
bar :: RuleCoverage -> Text
bar rule
  | ruleStated rule == 0 = ""
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
    percent value = (100 * value) `div` ruleStated rule

totalsRow :: EraCoverage -> Text
totalsRow era =
  "<tr class=\"totals\"><th>total</th>"
    <> number (sum (map ruleStated (eraRules era)))
    <> number (total ruleFulfilled)
    <> number (total ruleOutstanding)
    <> number (total ruleWaived)
    <> number (total ruleUntested)
    <> "<th class=\"bar\"></th></tr>"
  where
    total field = sum (map field (eraRules era))
    number value = "<th class=\"n\">" <> show value <> "</th>"

escape :: Text -> Text
escape =
  T.replace ">" "&gt;"
    . T.replace "<" "&lt;"
    . T.replace "&" "&amp;"

-- | One stylesheet in the page, because a report that has to be served with a
-- second file beside it stops being something you can mail to someone.
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
      "table.rules > tbody > tr.reached > td:first-child::before { color: var(--untested); }",
      "table.rules > tbody > tr.unreached > td:first-child::before { color: var(--alarm); }",
      "tr.reached td:first-child, tr.unreached td:first-child { color: var(--dim); }",
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
