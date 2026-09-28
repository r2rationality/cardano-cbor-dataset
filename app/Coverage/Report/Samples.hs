module Coverage.Report.Samples where

import Base.Paths
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term (Term)
import Coverage.Obligations.CborTerm (decodeTaggedTerm)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Traversable (for)
import Generation.Dataset
import Generation.SampleType
import Path

-- | One sample of a rule, decoded.
data Sample = Sample
  { -- | The rule of the sample, and its name in the rule, for example coin/valid/00001-39ba9bf686418065
    sampleKey :: SampleKey,
    -- | The decoded sample
    sampleTerm :: Term
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
  case decodeCbor bytes of
    Left message -> abort $ "the sample '" <> display path <> "' is not CBOR: " <> message
    Right term -> pure Sample {sampleKey = key, sampleTerm = term}

-- | The one CBOR value these bytes hold.
decodeCbor :: BS.ByteString -> Either Text Term
decodeCbor bytes = case CBOR.deserialiseFromBytes decodeTaggedTerm (LBS.fromStrict bytes) of
  Left failure -> Left (show failure)
  Right (rest, term)
    | LBS.null rest -> Right term
    | otherwise -> Left "trailing bytes after a complete value"

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
