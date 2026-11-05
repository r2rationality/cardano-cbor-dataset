module Coverage.Report.Samples where

import Base.Paths
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Coverage.Obligations.Cbor (Cbor, readCbor)
import Data.ByteString qualified as BS
import Data.Traversable (for)
import Generation.Dataset
import Generation.SampleType
import Path

-- | One sample of a rule, decoded.
data Sample = Sample
  { -- | The rule of the sample, and its name in the rule, for example coin/valid/00001-39ba9bf686418065
    sampleKey :: SampleKey,
    -- | The decoded sample, with how each head spells its length or its number
    sampleCbor :: Cbor
  }
  deriving (Eq, Show)

-- | The name of the sample in its rule.
sampleName :: Sample -> SampleName
sampleName = sampleKeyName . sampleKey

sampleType :: Sample -> SampleType
sampleType = sampleNameType . sampleName

-- | True for a sample that the node refuses.
isRejected :: Sample -> Bool
isRejected = (== Reject) . sampleCategory . sampleType

-- | Read and decode the sample in this file. A sample that is not exactly one CBOR value stops the run: every sample
-- of a corpus is CBOR, so such a file is a broken corpus, not a sample to skip.
decodeSample :: Path Abs File -> SampleKey -> IO Sample
decodeSample path key = do
  bytes <- readBytes path
  case readSample key bytes of
    Left message -> abort $ "the sample '" <> display path <> "' is not CBOR: " <> message
    Right sample -> pure sample

-- | The sample that these bytes hold.
readSample :: SampleKey -> BS.ByteString -> Either Text Sample
readSample key bytes = Sample key <$> readCbor bytes

-- | Load the samples of a rule from the rule directory. A directory whose name is not a sample type, such as the
-- configuration a corpus carries, holds no samples.
--
-- Manual samples count for exactly what they would if the generator had produced them: where the bytes came from
-- decides nothing.
loadSamples :: Path Abs Dir -> Name -> IO [Sample]
loadSamples datasetDir rule@(Name ruleName) = do
  ruleDir <- datasetDir <//> ruleName
  directories <- whenDirExists ruleDir $ listDirectoryNames ruleDir
  concat <$> for (mapMaybe fromSampleDirectory directories) (loadSampleType ruleDir rule)

-- | Load the samples of every rule of a dataset.
loadDatasetSamples :: Path Abs Dir -> IO [Sample]
loadDatasetSamples datasetDir = do
  rules <- map Name <$> listDirectoryNames datasetDir
  concat <$> for rules (loadSamples datasetDir)

-- | Load the samples of one sample type of a rule.
loadSampleType :: Path Abs Dir -> Name -> SampleType -> IO [Sample]
loadSampleType ruleDir rule kind = do
  directory <- ruleDir <//> sampleDirectory kind
  whenDirExists directory $ do
    stems <- mapMaybe inputStem <$> listFileNames directory
    for stems $ \stem -> do
      path <- directory <///> sampleFileName stem InputFile
      decodeSample path (SampleKey rule (SampleName kind stem))
