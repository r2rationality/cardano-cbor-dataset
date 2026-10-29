module Generation.Verify where

import Base.Paths
import Coverage.Obligations.Divergence (acceptedSamples, divergentSamples)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Traversable
import Generation.Dataset
import Generation.DatasetConfig
import Generation.EraSpec
import Generation.Report
import Generation.Rule
import Generation.SampleType (SampleKey, renderSampleKey)
import Generation.VerificationFailure
import Path (Abs, Dir, File, Path)

-- | Verify a dataset for a given era and write the result of each sample to the given JSON file. Fail when the dataset
--   does not pass.
verifyDataset :: EraSpec -> Path Abs Dir -> Path Abs File -> IO ()
verifyDataset era datasetDir reportPath = do
  revisions <- readRevisions
  dataset <- loadDataset era datasetDir
  verification <- checkDataset dataset
  writeVerificationResults reportPath era revisions verification
  unless (verificationPassed verification) exitFailure

-- | Check every sample of a dataset. Every sample must be accepted or rejected as its directory says. Every accepted
--   sample must re-encode to its expected value
checkDataset :: Dataset -> IO Verification
checkDataset dataset = do
  let era = datasetEra dataset
  let root = datasetRoot dataset
  let files = datasetFiles dataset
  config <- readConfig dataset

  results <- for files $ \file -> do
    outcome <- verifyFile root file <&> applyDivergences config file
    printOutcome root file outcome
    pure Outcome {file = file, outcome = outcome}

  putTextLn $ "Checked " <> show (length files) <> " " <> show era <> " CBOR files"

  -- A divergence that points to no existing sample is reported
  let orphaned = missingNamedSamples dataset config
  unless (null orphaned) $
    putTextLn $
      "  corpus.json names absent samples: " <> T.intercalate ", " (map renderSampleKey orphaned)

  let passed = not (any (isJust . outcome) results) && null orphaned
  let verification = Verification results passed
  unless passed $
    abort "the corpus does not pass verification"

  pure verification

-- | Check a given sample.
--    - If the sample is supposed to be an invalid one, just check if it fails to decode
--    - If the sample is supposed to be a valid one, check if it decodes and if the re-encoding matches its reference
verifyFile :: DatasetRoot -> DatasetFile -> IO (Maybe VerificationFailure)
verifyFile root datasetFile = do
  let rule = datasetFileRule datasetFile
  content <- datasetFileContent root datasetFile
  if isInvalidFile datasetFile
    then
      deserializeSample rule content <&> \case
        Left _ -> Nothing
        Right _ -> Just (VerificationFailure DecodeSucceeded "decoded successfully")
    else do
      expected <- datasetFileExpectedContent root datasetFile
      reserializeSample rule content <&> \case
        Left reason -> Just reason
        Right actual ->
          if actual == expected
            then Nothing
            else Just (VerificationFailure EncodeFailed "re-encoded bytes do not match expected")

-- | The samples that the divergences of a dataset name, and that the dataset does not have
missingNamedSamples :: Dataset -> DatasetConfig -> [SampleKey]
missingNamedSamples dataset config = do
  let named = divergentSamples (configDivergences config)
  sort . toList $ named `Set.difference` fromList (map datasetFileKey (datasetFiles dataset))

-- | Apply the divergences of the corpus to the outcome of a sample.
--
-- A divergence can say that the node accepts a manual-invalid sample, which the specification refuses. Then the node
-- must decode that sample: a refusal fails the run, because the divergence no longer holds. Any other failure of the
-- sample is still reported.
applyDivergences :: DatasetConfig -> DatasetFile -> Maybe VerificationFailure -> Maybe VerificationFailure
applyDivergences config file outcome
  | datasetFileKey file `Set.member` acceptedSamples (configDivergences config) = case outcome of
      Just (VerificationFailure DecodeSucceeded _) -> Nothing
      Nothing -> Just (VerificationFailure DecodeFailed "a divergence says that the node accepts it, and the node refuses it")
      reason -> reason
  | otherwise = outcome

-- | Print the outcome of verifying a single sample.
printOutcome :: DatasetRoot -> DatasetFile -> Maybe VerificationFailure -> IO ()
printOutcome _ _ Nothing = pure ()
printOutcome root datasetFile (Just reason) = do
  samplePath <- datasetFilePath root datasetFile
  warn $ "FAIL " <> display samplePath <> " (" <> formatVerificationFailure reason <> ")"
