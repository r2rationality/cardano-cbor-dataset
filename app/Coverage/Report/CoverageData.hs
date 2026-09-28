{-# LANGUAGE DeriveAnyClass #-}

module Coverage.Report.CoverageData where

import Base.Json (readJson, writeJson)
import Base.Paths (abort, corpusCoverageFile, display, readText)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Coverage.Obligations.Divergence (Divergence)
import Coverage.Obligations.Obligation
import Coverage.Obligations.Obligations hiding (ruleObligations)
import Coverage.Obligations.SpecRule (HuddleSpec, renderRule)
import Coverage.Obligations.Trail (Absolute, Trail)
import Coverage.Report.SpecCoverage
import Data.Aeson
import Data.Char (isHexDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Generation.EraSpec
import Generation.SampleType (SampleCategory)
import Path
import Path.IO qualified as PathIO

-- | One era dataset measured against the huddle spec
data CoverageSummary = CoverageSummary
  { summaryEra :: Era,
    summaryRules :: [RuleSummary],
    summarySamples :: Int,
    summaryManual :: Int,
    -- | The places where the Haskell node and the specification disagree, and why
    summaryDivergences :: [Divergence],
    summaryUnmatchedDivergences :: [Divergence],
    summaryDivergenceMismatches :: [DivergenceMismatch]
  }
  deriving (Eq, Show)

instance ToJSON CoverageSummary where
  toJSON summary =
    object
      [ "era" .= summaryEra summary,
        "rules" .= summaryRules summary,
        "samples" .= summarySamples summary,
        "manual" .= summaryManual summary,
        "divergences" .= summaryDivergences summary,
        "unmatchedDivergences" .= summaryUnmatchedDivergences summary,
        "divergenceMismatches" .= summaryDivergenceMismatches summary
      ]

instance FromJSON CoverageSummary where
  parseJSON = withObject "coverage summary" $ \o ->
    CoverageSummary
      <$> o .: "era"
      <*> o .: "rules"
      <*> o .: "samples"
      <*> o .: "manual"
      <*> o .: "divergences"
      <*> o .: "unmatchedDivergences"
      <*> o .: "divergenceMismatches"

-- | One rule of the specification, and what the corpus knows about it.
data RuleSummary = RuleSummary
  { ruleName :: Name,
    ruleCounts :: CoverageCounts,
    ruleText :: Text,
    ruleObligations :: [RecordedObligation]
  }
  deriving (Eq, Show)

instance ToJSON RuleSummary where
  toJSON rule =
    object
      [ "name" .= ruleName rule,
        "coverage" .= ruleCounts rule,
        "description" .= ruleText rule,
        "obligations" .= ruleObligations rule
      ]

instance FromJSON RuleSummary where
  parseJSON = withObject "rule summary" $ \o ->
    RuleSummary
      <$> o .: "name"
      <*> o .: "coverage"
      <*> o .: "description"
      <*> o .: "obligations"

-- | One obligation as the page shows it and as the file keeps it: its category, where it is stated, what it asks for,
-- and its evidence. The path and the demand identify the obligation, so that a comparison matches each obligation with
-- the same one in another measurement.
data RecordedObligation = RecordedObligation
  { recordedCategory :: SampleCategory,
    recordedPath :: Trail Absolute,
    recordedDemand :: Demand,
    recordedEvidence :: Evidence
  }
  deriving (Eq, Show)

instance ToJSON RecordedObligation where
  toJSON obligation =
    object $
      [ "category" .= recordedCategory obligation,
        "path" .= recordedPath obligation,
        "demand" .= recordedDemand obligation
      ]
        <> evidencePairs (recordedEvidence obligation)

instance FromJSON RecordedObligation where
  parseJSON = withObject "obligation" $ \o ->
    RecordedObligation
      <$> o .: "category"
      <*> o .: "path"
      <*> o .: "demand"
      <*> parseEvidence o

-- | An obligation with its evidence, as the file keeps it.
recordObligation :: ObligationCoverage -> RecordedObligation
recordObligation (ObligationCoverage obligation evidence) =
  RecordedObligation
    { recordedCategory = sampleCategoryFor obligation,
      recordedPath = obligationPath obligation,
      recordedDemand = demandOf (obligationKind obligation),
      recordedEvidence = evidence
    }

-- | Summarize the specification coverage to save it to a file and create a HTML report from it.
summarizeCoverage :: Era -> HuddleSpec -> SpecCoverage -> CoverageSummary
summarizeCoverage era huddleSpec specCoverage =
  CoverageSummary
    { summaryEra = era,
      summaryRules = summarizeRules huddleSpec specCoverage,
      summarySamples = coverageSamplesNb specCoverage,
      summaryManual = manualSamplesNb specCoverage,
      summaryDivergences = declaredDivergences specCoverage,
      summaryUnmatchedDivergences = unmatchedDivergences specCoverage,
      summaryDivergenceMismatches = divergenceMismatches specCoverage
    }

summarizeRules :: HuddleSpec -> SpecCoverage -> [RuleSummary]
summarizeRules (CTreeRoot definitions) specCoverage =
  [ summarizeRule name (renderRule foldedRule definitions body) (ruleCoverageObligations report)
  | report <- sortOn ruleCoverageName (rules specCoverage),
    let name = ruleCoverageName report,
    Just body <- [Map.lookup name definitions]
  ]

-- | The summary of one rule, from its CDDL text and its measured obligations.
summarizeRule :: Name -> Text -> [ObligationCoverage] -> RuleSummary
summarizeRule name text obligations =
  RuleSummary
    { ruleName = name,
      ruleCounts = countsOfEntries obligations,
      ruleText = text,
      ruleObligations = map recordObligation obligations
    }

-- | How the corpus stands towards one measured rule of the specification: every obligation the rule states was put to
-- the samples one by one, and each count is the obligations with that verdict.
data CoverageCounts = CoverageCounts
  { countFulfilled :: Int,
    countOutstanding :: Int,
    countWaived :: Int,
    countUntested :: Int
  }
  deriving (Eq, Show)

instance ToJSON CoverageCounts where
  toJSON counts =
    object
      [ "fulfilled" .= countFulfilled counts,
        "outstanding" .= countOutstanding counts,
        "waived" .= countWaived counts,
        "untested" .= countUntested counts
      ]

instance FromJSON CoverageCounts where
  parseJSON = withObject "coverage counts" $ \o ->
    CoverageCounts
      <$> o .: "fulfilled"
      <*> o .: "outstanding"
      <*> o .: "waived"
      <*> o .: "untested"

-- | The obligations that still need samples: the outstanding ones and the untested ones.
countOwing :: CoverageCounts -> Int
countOwing counts = countOutstanding counts + countUntested counts

-- | The number of obligations of a rule.
ruleObligationsNb :: RuleSummary -> Int
ruleObligationsNb = length . ruleObligations

ruleFulfilled :: RuleSummary -> Int
ruleFulfilled = countFulfilled . ruleCounts

ruleOutstanding :: RuleSummary -> Int
ruleOutstanding = countOutstanding . ruleCounts

-- | Obligations the specification states, that the decoder for this rule does not enforce, with a recorded reason
-- naming where it is enforced instead.
ruleWaived :: RuleSummary -> Int
ruleWaived = countWaived . ruleCounts

ruleUntested :: RuleSummary -> Int
ruleUntested = countUntested . ruleCounts

-- | Obligations with nothing left to do: met by a sample, or waived because the decoder for this rule does not enforce
-- them and the reason beside the sample says so.
--
-- What the headline counts, and deliberately not what the baseline counts: a waiver is a decision taken rather than
-- coverage gained, so letting one into 'ruleFulfilled' would let a waiver hide a sample that stopped working.
ruleSettled :: RuleSummary -> Int
ruleSettled rule = ruleFulfilled rule + ruleWaived rule

countsOfEntries :: [ObligationCoverage] -> CoverageCounts
countsOfEntries entries =
  CoverageCounts (tally isFulfilled) (tally (== Sampled Unmet)) (tally waived) (tally untested)
  where
    tally predicate = length (filter (predicate . coverageEvidence) entries)
    waived = \case
      WaivedBy _ _ -> True
      _ -> False
    untested = (`elem` [Sampled NoCandidate, Sampled Undecided])

-- THE BASELINE

-- THE MEASUREMENT ON DISK

-- | Write the measurement of a corpus inside it, beside its configuration.
--
-- Measuring a corpus takes minutes and rendering it takes none, so the two are kept apart: the page is rebuilt from the
-- measurements the corpora carry, and the next generation compares its own measurement with this one, obligation by
-- obligation. Rules in name order and one entry per line, so two measurements diff cleanly.
writeCorpusCoverage :: Path Abs Dir -> CoverageSummary -> IO ()
writeCorpusCoverage corpusDir = writeJson (corpusDir </> corpusCoverageFile)

-- | The measurement a corpus carries, or nothing when it has none yet.
readCorpusCoverage :: Path Abs Dir -> IO (Maybe CoverageSummary)
readCorpusCoverage corpusDir = do
  let path = corpusDir </> corpusCoverageFile
  present <- PathIO.doesFileExist path
  if present then Just <$> readJson path else pure Nothing

-- | The measurement a corpus carries, which must be there.
requireCorpusCoverage :: Path Abs Dir -> IO CoverageSummary
requireCorpusCoverage corpusDir =
  readCorpusCoverage corpusDir
    >>= maybe (abort $ "no coverage measurement in '" <> display corpusDir <> "': generate the corpus first") pure

-- THE PAGE

-- | The commit of the ledger the obligations were derived from.
--
-- Carried rather than assumed, because a link to a branch would drift: the specification moves, and a page that claims
-- to measure one version while pointing at another is worse than a page with no link at all.
newtype LedgerPin = LedgerPin {ledgerCommit :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Where the ledger commit the obligations were derived from is written down.
--
-- One place, read rather than copied, so a page cannot claim a version the build did not use. Relative, so it is found
-- from the project directory.
ledgerPinFile :: Text
ledgerPinFile = "Dockerfile"

-- | The pin as the repository records it.
--
-- The Dockerfile is where the commit is written down, so it is read rather than copied: a second copy is a second thing
-- to forget.
readLedgerPin :: Path Abs File -> IO (Maybe LedgerPin)
readLedgerPin path = do
  present <- PathIO.doesFileExist path
  if not present
    then pure Nothing
    else do
      contents <- readText path
      pure $
        viaNonEmpty
          head
          [ LedgerPin commit
          | line <- T.lines contents,
            Just rest <- [T.stripPrefix "ARG LEDGER_COMMIT=" (T.strip line)],
            let commit = T.takeWhile isHexDigit rest,
            T.length commit == 40
          ]
