module Main where

import Corpus
  ( VerificationMode (..),
    generateDataset,
    verifyDataset,
  )
import CoverageReport
  ( EraCoverage,
    measureCoverage,
    readBaseline,
    readCoverageData,
    readLedgerPin,
    regressionsAgainst,
    renderRegression,
    writeBaseline,
    writeCoverageData,
    writeCoveragePage,
  )
import LedgerRules
  ( EraSpec,
    eraSpecName,
    lookupEra,
    ruleNames,
    supportedEraNames,
    supportedEras,
  )
import Options.Applicative
  ( Parser,
    ParserInfo,
    command,
    customExecParser,
    eitherReader,
    flag,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    prefs,
    progDesc,
    showDefault,
    showHelpOnEmpty,
    showHelpOnError,
    strArgument,
    strOption,
    switch,
    value,
  )
import Paths (listDirectoryChecked)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

data Command
  = Generate EraSpec (Maybe FilePath) [String] FilePath (Maybe FilePath) CoverageOutputs Bool
  | Verify EraSpec VerificationMode FilePath
  | ListEras
  | ListRules EraSpec
  | Report FilePath CoverageOutputs Bool Bool

-- | Where the two artefacts of a coverage run are written.
data CoverageOutputs = CoverageOutputs
  { coveragePagePath :: FilePath,
    coverageBaselinePath :: FilePath,
    -- | The measurement itself, written so the page can be rebuilt from it
    -- without measuring again.
    coverageDataPath :: FilePath
  }

eraOption :: Parser EraSpec
eraOption =
  option
    (eitherReader lookupEra)
    (long "era" <> metavar "ERA" <> help ("Ledger era: " <> intercalate ", " supportedEraNames))

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

readNatural :: String -> Either String Integer
readNatural "0" = Right 0
readNatural raw@(firstDigit : _)
  | firstDigit /= '0' && all isAsciiDigit raw =
      maybe (Left "expected a non-negative decimal integer without leading zeros") Right (readMaybe raw)
readNatural _ = Left "expected a non-negative decimal integer without leading zeros"

datasetArgument :: Parser FilePath
datasetArgument = strArgument $ metavar "DATASET_DIR"

commandInfo :: String -> Parser a -> ParserInfo a
commandInfo description parser = info parser $ progDesc description

-- | The era belongs to each mode rather than to @verify@ itself, so that
-- @verify MODE --era ERA DATASET_DIR@ reads in the order it is documented.
verifyModeParser :: VerificationMode -> Parser Command
verifyModeParser mode = (\era dataset -> Verify era mode dataset) <$> eraOption <*> datasetArgument

verifyParser :: Parser Command
verifyParser =
  hsubparser $
    command
      "deserialize"
      (commandInfo "Check decoder acceptance only" $ verifyModeParser DeserializeOnly)
      <> command
        "expected"
        ( commandInfo "Require normalized reserialization to equal each expected file" $
            verifyModeParser CheckExpectedOutput
        )

commandParser :: Parser Command
commandParser =
  hsubparser $
    command
      "generate"
      ( commandInfo "Generate a deterministic CBOR corpus" $
          ( Generate
              <$> eraOption
              <*> optional
                ( strOption
                    ( long "adopt-manual"
                        <> metavar "CORPUS_DIR"
                        <> help "Carry the hand written manual subtree over from an existing corpus"
                    )
                )
              <*> many
                ( strOption
                    ( long "only"
                        <> metavar "RULE"
                        <> help "Generate just this rule, repeatable; default is every root"
                    )
                )
              <*> strArgument (metavar "OUTPUT_DIR")
              <*> optional
                ( strOption
                    ( long "config"
                        <> metavar "FILE"
                        <> help "Seed and per-rule sample counts; defaults to the corpus's own corpus.json"
                    )
                )
              <*> coverageOutputs
              <*> flag
                True
                False
                ( long "no-coverage-check"
                    <> help "Skip the coverage page and the check against its baseline"
                )
          )
      )
      <> command
        "verify"
        (commandInfo "Verify a CBOR corpus" verifyParser)
      <> command
        "list-eras"
        (commandInfo "List supported ledger eras" $ pure ListEras)
      <> command
        "list-rules"
        (commandInfo "List rules supported for an era" $ ListRules <$> eraOption)
      <> command
        "report"
        ( commandInfo "Publish the specification coverage page and check it against the baseline" $
            Report
              <$> strArgument (metavar "DATASET_DIR")
              <*> coverageOutputs
              <*> switch
                ( long "update-baseline"
                    <> help "Record what is measured now as the baseline instead of checking against it"
                )
              <*> switch
                ( long "reuse"
                    <> help "Rebuild the page from the last measurement instead of measuring again"
                )
        )

coverageOutputs :: Parser CoverageOutputs
coverageOutputs =
  CoverageOutputs
    <$> strOption
      ( long "page"
          <> metavar "FILE"
          <> value "dataset/reports/coverage.html"
          <> showDefault
          <> help "Where to write the coverage page"
      )
    <*> strOption
      ( long "baseline"
          <> metavar "FILE"
          <> value "dataset/reports/coverage.json"
          <> showDefault
          <> help "The recorded coverage a run must not fall below"
      )
    <*> strOption
      ( long "data"
          <> metavar "FILE"
          <> value "dataset/reports/coverage-data.json"
          <> showDefault
          <> help "Where the measurement is written, and what --reuse reads back"
      )

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper)
    (fullDesc <> header "Cardano ledger CBOR corpus generator and verifier")

runCommand :: Command -> IO ()
runCommand (Generate era adoptFrom only outputDir configPath outputs checked) = do
  generateDataset era adoptFrom only outputDir configPath
  -- The page is written from what was just published, and a corpus that
  -- fulfils less than the one it replaces fails the run here rather than
  -- being noticed later by whoever reads the numbers.
  when checked $ runCommand (Report outputDir outputs False False)
runCommand (Verify era mode datasetDir) = verifyDataset era mode datasetDir
runCommand ListEras = mapM_ (putStrLn . eraSpecName) supportedEras
runCommand (ListRules era) = mapM_ putStrLn $ ruleNames era
runCommand (Report datasetDir outputs update reuse) = do
  (pin, eras) <-
    if reuse
      then readCoverageData (coverageDataPath outputs)
      else do
        corpora <- eraCorpora datasetDir
        when (null corpora) $ die $ "no era corpus found under '" <> datasetDir <> "'"
        measured <- forM corpora $ \(era, corpusDir) -> measureCoverage (toText era) corpusDir
        pinned <- readLedgerPin ledgerPinFile
        writeCoverageData (coverageDataPath outputs) pinned measured
        putStrLn $ "Measured into " <> coverageDataPath outputs
        pure (pinned, measured)
  writeCoveragePage (coveragePagePath outputs) pin eras
  putStrLn $ "Wrote " <> coveragePagePath outputs
  if update
    then do
      writeBaseline (coverageBaselinePath outputs) eras
      putStrLn $ "Recorded " <> coverageBaselinePath outputs
    else checkAgainstBaseline (coverageBaselinePath outputs) eras

-- | Where the ledger commit the obligations were derived from is written down.
--
-- One place, read rather than copied, so a page cannot claim a version the
-- build did not use.
ledgerPinFile :: FilePath
ledgerPinFile = "Dockerfile"

-- | The corpus directory of each era present.
--
-- A corpus is named for its era and nothing else: what seed and how many
-- samples produced it live in the configuration, where they can differ per
-- rule rather than being flattened into one name.
eraCorpora :: FilePath -> IO [(String, FilePath)]
eraCorpora datasetDir = do
  entries <- listDirectoryChecked datasetDir
  present <- filterM (\entry -> doesDirectoryExist (datasetDir </> entry)) entries
  pure
    [ (eraSpecName era, datasetDir </> eraSpecName era)
    | era <- supportedEras,
      eraSpecName era `elem` present
    ]

-- | Fail the run when a corpus fulfils less than it did.
--
-- Writing the baseline where none exists is not a decision anyone has to make:
-- there is nothing to regress against yet, and refusing would mean a first run
-- can never succeed.
checkAgainstBaseline :: FilePath -> [EraCoverage] -> IO ()
checkAgainstBaseline path eras = do
  recorded <- readBaseline path
  case recorded of
    Nothing -> do
      writeBaseline path eras
      putStrLn $ "Recorded " <> path <> " as the first baseline"
    Just baseline ->
      case regressionsAgainst baseline eras of
        [] -> putStrLn "Coverage holds against the baseline"
        losses -> do
          putTextLn "coverage lost against the baseline:"
          mapM_ (putTextLn . ("  " <>) . renderRegression) losses
          die "run with --update-baseline once the loss is understood and intended"

main :: IO ()
main =
  customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) parserInfo
    >>= runCommand
