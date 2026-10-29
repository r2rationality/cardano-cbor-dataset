module Generation.Generate where

import Base.Extra
import Base.Paths
import Base.Text
import Control.Monad (foldM)
import Control.Monad.Loops (iterateUntilM)
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text qualified as T
import Generation.Dataset
import Generation.DatasetConfig
import Generation.EraSpec
import Generation.GenerateCbor
import Generation.GenerationChecks
import Generation.GenerationRun
import Generation.GenerationTarget
import Generation.Normalize (definiteForms, definiteStrings)
import Generation.Report
import Generation.Rule as Rule
import Generation.SampleType
import Generation.Verify
import Path
import Path.IO qualified as PathIO

-- | Generate a dataset for a given era,
--    - optionally filtering by specific rules,
--    - with the configuration from the given source,
--    - and output to the specified root directory.
--
--  This function creates the relevant directories and calls the `generate-cbor` executable to generate samples on
--  stdout. Those samples are then saved in files within the appropriate directories under the staging area.
--
--  Finally the staging area is published to the destination directory if the run was successful.
generateDataset :: EraSpec -> [RuleName] -> ConfigSource -> GenerationChecks -> Path Abs Dir -> IO ()
generateDataset eraSpec onlyRules configSource checks outputDir = do
  -- the revisions are read first, so that a missing commit stops the run before the slow generation
  revisions <- if checkVerification checks then Just <$> readRevisions else pure Nothing
  createOutputDirectory outputDir
  destination <- outputDir <//> eraSpecName eraSpec

  -- Read the configuration
  config <- readConfigSource configSource
  let rules = getRulesToGenerate eraSpec onlyRules

  -- Generate samples, first in a staging directory, then published to the destination directory
  result <- publishDirectory destination $ \staging -> do
    -- write the configuration to the staging area
    writeConfigSource configSource (staging </> corpusConfigFile)

    -- generate samples for each rule and sampleType
    generatedNb <- foldMapM (\rule -> foldMapM (generateSamples staging eraSpec config rule) generatedSampleTypes) rules

    -- keep:
    --   - previously generated samples if we just generated a subset of the rules
    --   - manual samples
    whenDirExists destination $ keepPreviousSamples eraSpec rules destination staging

    -- create expected files for each rule in the staging area (for valid samples)
    for_ rules (createExpectedFiles staging)

    -- the new corpus replaces the previous one only when it passes the verification and coverage checks
    -- (unless they are deactivated)
    dataset <- loadDataset eraSpec staging

    -- verification is used to write the verification report for the Haskell node
    verification <- whenMonoid (checkVerification checks) $ checkDataset dataset

    -- check the dataset coverage
    when (checkCoverage checks) $ checkCoverageOf eraSpec checks destination staging

    pure GenerationResult {generated = generatedNb, verification}

  -- report results and run checks
  putTextLn $ "Generated " <> show (getSum (generated result)) <> "/" <> show (targetSamplesFor config rules) <> " samples in " <> display outputDir

  for_ revisions $ \revision -> do
    resultsPath <- outputDir </*//> "reports" :| [eraSpecName eraSpec, "haskell-latest.json"]
    writeVerificationResults resultsPath eraSpec revision (verification result)

data GenerationResult = GenerationResult
  { generated :: Sum Int,
    verification :: Verification
  }
  deriving (Eq, Show)

-- | Generate samples for one rule and category. Return the number of generated samples.
generateSamples :: Path Abs Dir -> EraSpec -> DatasetConfig -> GenerationRule -> SampleType -> IO (Sum Int)
generateSamples staging era config rule sampleType = do
  ruleDir <- staging <//> ruleNameText rule
  let target = GenerationTarget era rule sampleType ruleDir
  let run = newGenerationRun config rule
  createSampleDirectories ruleDir sampleType

  putTextLn $ "Generating " <> targetLabel target
  finalRun <- iterateUntilM (isRunFinished config) (runBatch config target) run
  finalizeRun config target finalRun

-- | Create the directories for a given sample type
createSampleDirectories :: Path Abs Dir -> SampleType -> IO ()
createSampleDirectories ruleDir sampleType = do
  for_ (sampleTargetDirectories sampleType) $
    PathIO.ensureDir <=< (ruleDir <//>)

-- | Generate one batch of samples with the `generate-cbor` command.
runBatch :: DatasetConfig -> GenerationTarget -> GenerationRun -> IO GenerationRun
runBatch config target run = do
  let requested = min (configBatchSize config) $ min (samplesNb run - accepted run) (maxAttempts run - attempts run)
  result <- generateCbor config target run requested
  run' <- foldM (writeSample target) run (batchLines result)

  let exhausted = if batchExhausted result then 1 else 0
  pure
    run'
      { attempts = attempts run + length (batchLines result) + exhausted,
        batch = batch run + 1,
        exhaustions = exhaustions run + exhausted
      }

-- | Keep previous samples: manual samples and previously generated samples not affected by this run
keepPreviousSamples :: EraSpec -> [GenerationRule] -> Path Abs Dir -> Path Abs Dir -> IO ()
keepPreviousSamples era generated previous staging = do
  ruleDirs <- listDirectoryNames previous
  for_ ruleDirs $ \rule -> do
    source <- previous <//> rule
    target <- staging <//> rule
    if rule `elem` map ruleNameText generated
      then
        -- keep the manual samples for that rule if we generated new samples
        copyManualSamples source target
      else
        -- keep all the samples for that rule if we did not generate new samples
        when (RuleName rule `elem` ruleNames era) $
          PathIO.copyDirRecur source target

-- | Copy the manual samples from the source directory to the target directory
copyManualSamples :: Path Abs Dir -> Path Abs Dir -> IO ()
copyManualSamples source target =
  for_ [ManualValid, ManualInvalid] $ \sampleType -> do
    from <- source <//> sampleDirectory sampleType
    whenDirExists from $ target <//> sampleDirectory sampleType >>= PathIO.copyDirRecur from

-- | Report how the run went, and return the number of generated samples
finalizeRun :: DatasetConfig -> GenerationTarget -> GenerationRun -> IO (Sum Int)
finalizeRun config target run = do
  let label = targetLabel target
  let messages =
        [generatedFor run label | accepted run < samplesNb run && not (shouldGiveUp config run)]
          <> [gaveUpZappingFor run label | shouldGiveUp config run]
          <> [exhaustedSearchFor run label | exhaustions run > 0]
          <> [droppedMutationsFor run label | dropped run > 0]
  for_ messages warn
  pure (Sum (accepted run))

-- | Write an expected file for every sample of one rule that must decode: the generated @valid@ samples and the hand
-- written @manual-valid@ ones.
createExpectedFiles :: Path Abs Dir -> GenerationRule -> IO ()
createExpectedFiles staging rule = do
  putTextLn $ "create expected files for " <> ruleNameText rule
  for_ [Valid, ManualValid] $ \sampleType -> do
    directory <- sampleTypeDirectory staging rule sampleType
    whenDirExists directory $ do
      stems <- mapMaybe inputStem <$> listFileNames directory
      for_ stems $ createExpectedFile rule directory

-- | Write one generated sample where the decoder says it belongs, and count it in the run.
writeSample :: GenerationTarget -> GenerationRun -> BS.ByteString -> IO GenerationRun
writeSample target run bytes
  | isSeen run bytes = pure run
  | otherwise = case targetSampleType target of
      Valid
        | decodes bytes -> output Valid bytes run
        | otherwise -> do
            run' <- output (Zap 0) bytes run
            -- Try the strings, then the containers, in definite-length form
            case find (rescues run') (rights [definiteStrings bytes, definiteForms bytes]) of
              Nothing -> pure run'
              Just rewritten -> output Valid rewritten run'
      zap@(Zap _)
        | decodes bytes -> pure (record (sha256Hex bytes) Dropped run)
        | otherwise -> output zap bytes run
      _ -> pure run
  where
    decodes = isRight . deserializeRule (targetRule target)
    isSeen current sample = Set.member (sha256Hex sample) (seen current)
    rescues current candidate = not (isSeen current candidate) && decodes candidate
    output destination sample current = do
      let digest = sha256Hex sample
      writeSampleFile target current destination digest sample
      pure (record digest (Written destination) current)

-- | Write a sample to disk at a given position within the sample type directory
writeSampleFile :: GenerationTarget -> GenerationRun -> SampleType -> Text -> BS.ByteString -> IO ()
writeSampleFile target run sampleType contentDigest content = do
  destinationDir <- targetRuleDir target <//> sampleDirectory sampleType
  let position = getPositionFor run sampleType
  let stem = T.justifyRight 5 '0' (show position) <> "-" <> T.take 16 contentDigest
  path <- destinationDir <///> sampleFileName stem InputFile
  writeBytes path content

-- | Write the reference encoding of one sample that must decode
createExpectedFile :: GenerationRule -> Path Abs Dir -> Text -> IO ()
createExpectedFile rule validDir stem = do
  samplePath <- validDir <///> sampleFileName stem InputFile
  readResult <- readBytes samplePath
  bytes <- throwEither (reserializeRule rule readResult)
  outputPath <- validDir <///> sampleFileName stem ExpectedFile
  writeBytes outputPath bytes
