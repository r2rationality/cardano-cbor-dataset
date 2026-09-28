module Generation.Dataset where

import Base.Json (readJson)
import Base.Paths
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Control.Exception
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Traversable
import GHC.IO.Exception (userError)
import Generation.DatasetConfig
import Generation.EraSpec
import Generation.Rule
import Generation.SampleType
import Path

-- | The result of reading a dataset directory:
--     - 'datasetRoot' is the corpus the samples were read from.
--     - 'datasetFiles' contains all the sample files found.
data Dataset = Dataset
  { datasetEra :: Era,
    datasetRoot :: DatasetRoot,
    datasetFiles :: [DatasetFile]
  }

-- | The directory a corpus was read from, which every sample path is relative to
newtype DatasetRoot = DatasetRoot {datasetRootDir :: Path Abs Dir}

-- | A dataset file is a single sample file that checks a given rule
data DatasetFile = DatasetFile
  { datasetFileRule :: GenerationRule,
    -- | The type and the stem of the sample.
    datasetFileName :: SampleName
  }
  deriving (Eq, Show)

-- | The configuration the dataset carries.
readConfig :: Dataset -> IO DatasetConfig
readConfig dataset = do
  let DatasetRoot root = datasetRoot dataset
  readJson (root </> corpusConfigFile)

-- | Return true if the dataset file is valid according to its sample type.
isValidFile :: DatasetFile -> Bool
isValidFile = isValid . sampleNameType . datasetFileName

-- | Return true if the dataset file is invalid according to its sample type.
isInvalidFile :: DatasetFile -> Bool
isInvalidFile = isInvalid . sampleNameType . datasetFileName

-- | The key of a dataset file, which the results file and the waivers name it by.
datasetFileKey :: DatasetFile -> SampleKey
datasetFileKey file = SampleKey (Name (ruleNameText (datasetFileRule file))) (datasetFileName file)

-- | @\<rule\>\/\<sample type\>\/\<name\>@, the name a report gives this sample, with no suffix.
--
-- Relative to the corpus: it is what the results file keys by and what a waiver cites, so it is written the same way
-- wherever the corpus happens to sit.
fileStem :: DatasetFile -> IO (Path Rel File)
fileStem =
  parseRelFile
    . toString
    . renderSampleKey
    . datasetFileKey

-- | The directory holding every sample for one rule, in a corpus directory.
ruleDirectory :: Path Abs Dir -> GenerationRule -> IO (Path Abs Dir)
ruleDirectory corpusDir rule = corpusDir <//> ruleNameText rule

-- | The directory holding one rule's samples of one type, in a corpus directory
sampleTypeDirectory :: Path Abs Dir -> GenerationRule -> SampleType -> IO (Path Abs Dir)
sampleTypeDirectory corpusDir rule sampleType = corpusDir </*/> [ruleNameText rule, sampleDirectory sampleType]

-- | Return an absolute path to the sample file
datasetFilePath :: DatasetRoot -> DatasetFile -> IO (Path Abs File)
datasetFilePath root file = sampleFile root file InputFile

-- | Read the content of a sample file
datasetFileContent :: DatasetRoot -> DatasetFile -> IO BS.ByteString
datasetFileContent root datasetFile = do
  samplePath <- datasetFilePath root datasetFile
  readBytes samplePath

-- | Return the absolute path to the expected encoding of a sample
datasetFileExpected :: DatasetRoot -> DatasetFile -> IO (Path Abs File)
datasetFileExpected root file = sampleFile root file ExpectedFile

-- | Read the expected encoding of a sample. This function throws when the file is missing, meaning for example that we
--   tried to read an expected file for an invalid sample type. This would be a bug.
datasetFileExpectedContent :: DatasetRoot -> DatasetFile -> IO BS.ByteString
datasetFileExpectedContent root file = datasetFileExpected root file >>= readBytes

-- | Return the absolute path to one of the two files of a sample
sampleFile :: DatasetRoot -> DatasetFile -> SampleFileKind -> IO (Path Abs File)
sampleFile (DatasetRoot root) file kind = do
  directory <- sampleTypeDirectory root (datasetFileRule file) (sampleNameType (datasetFileName file))
  directory <///> sampleFileName (sampleNameStem (datasetFileName file)) kind

-- | Load a Dataset from a directory:
--    - Load the rules directories
--    - Check the actual rules against the rules expected by the era spec
loadDataset :: EraSpec -> Path Abs Dir -> IO Dataset
loadDataset eraSpec datasetDir = do
  requireRealDirectory "dataset directory" datasetDir
  let root = DatasetRoot datasetDir
  directoryNames <- listDirectoryNames datasetDir
  when (null directoryNames) $ abort $ "dataset contains no rule directories: " <> display datasetDir

  knownRules <-
    traverse (lookupRule eraSpec . RuleName) directoryNames
      & either (throwIO . userError . toString) pure
  found <- foldMapM (loadRuleSamples root) knownRules
  when (null found) $ abort $ "dataset contains no CBOR files: " <> display datasetDir
  pure $ Dataset (eraSpecEra eraSpec) root found

-- | Load all the samples for a given rule.
loadRuleSamples :: DatasetRoot -> GenerationRule -> IO [DatasetFile]
loadRuleSamples root rule = do
  rulePath <- ruleDirectory (datasetRootDir root) rule
  requireRealDirectory "dataset directory" rulePath
  sampleDirectories <- listDirectoryNames rulePath

  let actualSampleTypes = mapMaybe fromSampleDirectory sampleDirectories
  foldMapM (loadRuleSamplesForOneSampleType root rule) actualSampleTypes

-- | Load all the samples for a given sample type within a rule.
loadRuleSamplesForOneSampleType :: DatasetRoot -> GenerationRule -> SampleType -> IO [DatasetFile]
loadRuleSamplesForOneSampleType root rule sampleType = do
  samplePath <- sampleTypeDirectory (datasetRootDir root) rule sampleType
  requireRealDirectory "dataset directory" samplePath
  stems <- mapMaybe inputStem <$> listFileNames samplePath
  for stems $ \stem -> do
    let file = DatasetFile rule (SampleName sampleType stem)
    datasetFilePath root file >>= requireCBORFile
    pure file

-- | The two files a sample is made of, told apart by their suffix.
data SampleFileKind
  = -- | The data to be decoded
    InputFile
  | -- | The expected data after re-encoding
    ExpectedFile
  deriving (Eq, Show)

fileSuffix :: SampleFileKind -> Text
fileSuffix InputFile = ".input.cbor"
fileSuffix ExpectedFile = ".expected.cbor"

-- | The name of one of the files of a sample, from the stem of the sample.
sampleFileName :: Text -> SampleFileKind -> Text
sampleFileName stem kind = stem <> fileSuffix kind

-- | The input file of a sample, relative to its rule directory, for example @valid/00001-6a7584251bc715e6.input.cbor@.
renderSampleName :: SampleName -> Text
renderSampleName name = sampleDirectory (sampleNameType name) <> "/" <> sampleFileName (sampleNameStem name) InputFile

-- | Retrieve the stem of a sample file, if it has the input suffix. For example, given a file named
--   "example.input.cbor", this function will return Just "example".
inputStem :: Text -> Maybe Text
inputStem = T.stripSuffix (fileSuffix InputFile)

isManualSample :: SampleName -> Bool
isManualSample = isManual . sampleNameType
