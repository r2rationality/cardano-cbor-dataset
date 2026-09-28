{-# LANGUAGE QuasiQuotes #-}

module Main where

import Base.Paths (listDirectoryNames, resolveDirAgainst, resolveFileAgainst, (<//>))
import Coverage.Report.CoveragePage
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Traversable
import Generation.DatasetConfig (ConfigSource (..), corpusConfigSource)
import Generation.EraSpec
import Generation.Generate
import Generation.GenerationChecks
import Generation.Rule (RuleName (..))
import Generation.Verify
import Options.Applicative
import Path
import Path.IO (getCurrentDir)

-- | Top-level command for the generation and checks on the CBOR conformance datasets
data Command
  = Generate EraSpec [RuleName] (Maybe (Path Abs File)) GenerationChecks (Path Abs Dir)
  | Verify EraSpec (Path Abs Dir) (Path Abs File)
  | Report (Path Abs Dir) (Path Abs File)
  | ListEras
  | ListRules EraSpec

-- * COMMANDS

-- | The commands, with every path they take already made absolute against the working directory.
commandParser :: Path Abs Dir -> Parser Command
commandParser workingDir =
  hsubparser $
    generateCommand workingDir
      <> verifyCommand workingDir
      <> reportCommand workingDir
      <> listErasCommand
      <> listRulesCommand

generateCommand :: Path Abs Dir -> Mod CommandFields Command
generateCommand workingDir =
  command "generate" $
    commandInfo "Generate a CBOR corpus" $
      Generate
        <$> eraOption
        <*> onlyOption
        <*> configOption workingDir
        <*> generationChecks
        <*> argument (dirReader workingDir) (metavar "OUTPUT_DIR")

verifyCommand :: Path Abs Dir -> Mod CommandFields Command
verifyCommand workingDir =
  command "verify" $
    commandInfo "Verify a CBOR corpus: decoding, rejection and normalized reserialization of every sample" $
      Verify
        <$> eraArgument
        <*> datasetArgument workingDir
        <*> argument
          (fileReader workingDir)
          ( metavar "RESULTS_JSON"
              <> help "Write raw per-sample JSON results to this path"
          )

reportCommand :: Path Abs Dir -> Mod CommandFields Command
reportCommand workingDir =
  command "report" $
    commandInfo "Write the specification coverage page from the measurement each corpus carries" $
      Report
        <$> datasetArgument workingDir
        <*> option
          (fileReader workingDir)
          ( long "page"
              <> metavar "FILE"
              <> value (workingDir </> [relfile|dataset/reports/coverage.html|])
              <> showDefaultWith (const "dataset/reports/coverage.html")
              <> help "Where to write the coverage page"
          )

listErasCommand :: Mod CommandFields Command
listErasCommand =
  command "list-eras" $
    commandInfo "List supported ledger eras" $
      pure ListEras

listRulesCommand :: Mod CommandFields Command
listRulesCommand =
  command "list-rules" $
    commandInfo "List rules supported for an era" $
      ListRules <$> eraOption

-- * OPTIONS

-- | The rules to generate, all of them when the option is absent.
onlyOption :: Parser [RuleName]
onlyOption =
  many $
    strOption
      ( long "only"
          <> metavar "RULE"
          <> help "Generate just this rule, repeatable; default is every root"
      )

configOption :: Path Abs Dir -> Parser (Maybe (Path Abs File))
configOption workingDir =
  optional $
    option
      (fileReader workingDir)
      ( long "config"
          <> metavar "FILE"
          <> help "Seed and per-rule sample counts; default is the built-in configuration"
      )

generationChecks :: Parser GenerationChecks
generationChecks =
  GenerationChecks
    <$> flag
      True
      False
      ( long "no-verify"
          <> help "Do not verify the corpus before it replaces the previous one"
      )
    <*> flag
      True
      False
      ( long "no-coverage-check"
          <> help "Do not measure the coverage of the corpus"
      )
    <*> switch
      ( long "accept-coverage-loss"
          <> help "Replace the previous corpus even if the new one covers less of the specification"
      )

eraOption :: Parser EraSpec
eraOption = option eraReader (long "era" <> metavar "ERA" <> help eraHelp)

eraArgument :: Parser EraSpec
eraArgument = argument eraReader (metavar "ERA" <> help eraHelp)

-- | Return an EraSpec given its name
eraReader :: ReadM EraSpec
eraReader = eitherReader $ first toString . lookupEra . toText

eraHelp :: String
eraHelp = toString $ "Ledger era: " <> T.intercalate ", " (map eraSpecName eraSpecs)

readNatural :: String -> Either String Integer
readNatural "0" = Right 0
readNatural raw@(firstDigit : _)
  | firstDigit /= '0' && all isDigit raw =
      maybe (Left "expected a non-negative decimal integer without leading zeros") Right (readMaybe raw)
readNatural _ = Left "expected a non-negative decimal integer without leading zeros"

datasetArgument :: Path Abs Dir -> Parser (Path Abs Dir)
datasetArgument workingDir = argument (dirReader workingDir) (metavar "DATASET_DIR")

-- | Resolve a file path against the working directory.
fileReader :: Path Abs Dir -> ReadM (Path Abs File)
fileReader workingDir = eitherReader $ first toString . resolveFileAgainst workingDir . toText

-- | Resolve a directory path against the working directory.
dirReader :: Path Abs Dir -> ReadM (Path Abs Dir)
dirReader workingDir = eitherReader $ first toString . resolveDirAgainst workingDir . toText

-- * RUNNING COMMANDS

runCommand :: Command -> IO ()
runCommand (Generate era only configPath checks outputDir) = do
  configSource <- maybe (outputDir <//> eraSpecName era >>= corpusConfigSource) (pure . FromFile) configPath
  generateDataset era only configSource checks outputDir
runCommand (Verify era datasetDir resultsPath) = verifyDataset era datasetDir resultsPath
runCommand (Report datasetDir page) = writeCoveragePage page =<< eraCorpora datasetDir
runCommand ListEras = for_ eraSpecs (putTextLn . eraSpecName)
runCommand (ListRules era) = for_ (ruleNames era) (putTextLn . unRuleName)

-- | The corpus directory of each era.
eraCorpora :: Path Abs Dir -> IO [Path Abs Dir]
eraCorpora datasetDir = do
  present <- listDirectoryNames datasetDir
  for [era | era <- eraSpecs, eraSpecName era `elem` present] $ \era ->
    datasetDir <//> eraSpecName era

parserInfo :: Path Abs Dir -> ParserInfo Command
parserInfo workingDir =
  info
    (commandParser workingDir <**> helper)
    (fullDesc <> header "Cardano ledger CBOR corpus generator and verifier")

commandInfo :: String -> Parser a -> ParserInfo a
commandInfo description parser = info parser $ progDesc description

main :: IO ()
main = do
  workingDir <- getCurrentDir
  customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) (parserInfo workingDir)
    >>= runCommand
