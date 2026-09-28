module Generation.DatasetConfig where

import Base.Json (readJson, withNoNullFields, withoutFieldPrefix, writeJson)
import Base.Paths
import Coverage.Obligations.Divergence
import Data.Aeson.Types
import Data.Default (Default (..))
import Data.Map qualified as Map
import Generation.Rule
import Generation.SampleType
import Path
import Path.IO qualified as PathIO

-- | How a dataset is generated
data DatasetConfig = DatasetConfig
  { -- | Seed for the random number generator used to create the dataset
    datasetSeed :: Integer,
    -- | Number of samples to generate for the dataset
    datasetSamples :: Int,
    -- | Override for the number of samples per rule
    datasetRules :: Maybe (Map Text Int),
    -- | How many fruitless batches to allow before giving up on zapping a rule.
    datasetZapGiveUpAfter :: Maybe Int,
    -- | The size of each batch of samples generated
    datasetBatchSize :: Maybe Int,
    -- | The places where the Haskell node and the specification disagree
    datasetDivergences :: Maybe [Divergence]
  }
  deriving (Eq, Show, Generic)

-- | An absent parameter is not written, and reads back as absent.
instance ToJSON DatasetConfig where
  toJSON = genericToJSON configOptions

instance FromJSON DatasetConfig where
  parseJSON = genericParseJSON configOptions

configOptions :: Options
configOptions = defaultOptions & withoutFieldPrefix "dataset" & withNoNullFields

-- | The parameters of a corpus generated with no configuration: a seed and a number of samples. Every other parameter
-- takes its default value.
instance Default DatasetConfig where
  def =
    DatasetConfig
      { datasetSeed = 123,
        datasetSamples = 100,
        datasetRules = Nothing,
        datasetZapGiveUpAfter = Nothing,
        datasetBatchSize = Nothing,
        datasetDivergences = Nothing
      }

-- | The number of samples for each rule that has its own number. None by default.
configRules :: DatasetConfig -> Map Text Int
configRules = fromMaybe mempty . datasetRules

-- | Fruitless batches allowed before a rule's zapping is abandoned. 3 by default.
configZapGiveUpAfter :: DatasetConfig -> Int
configZapGiveUpAfter = fromMaybe 3 . datasetZapGiveUpAfter

-- | The size of each batch of samples generated. 128 by default.
configBatchSize :: DatasetConfig -> Int
configBatchSize = fromMaybe 128 . datasetBatchSize

-- | The places where the Haskell node and the specification disagree. None by default.
configDivergences :: DatasetConfig -> [Divergence]
configDivergences = fromMaybe [] . datasetDivergences

-- | Return the target number of samples for a set of rules. This depends on default sample number for each rule and any
--   rule-specific overrides.
targetSamplesFor :: DatasetConfig -> [GenerationRule] -> Int
targetSamplesFor config rules =
  length generatedSampleTypes
    * sum (map (numberOfSamplesFor config) rules)

-- | Return the number of samples for a specific rule, considering any rule-specific overrides.
numberOfSamplesFor :: DatasetConfig -> GenerationRule -> Int
numberOfSamplesFor config rule =
  fromMaybe (datasetSamples config) (Map.lookup (ruleNameText rule) (configRules config))

-- | The configuration is either provided via a configuration file or we use a default file.
data ConfigSource
  = FromFile (Path Abs File)
  | FromDefaults

-- | Where the configuration of a run come from depending on the existence of a previous file or not.
corpusConfigSource :: Path Abs Dir -> IO ConfigSource
corpusConfigSource corpusDir = do
  let path = corpusDir </> corpusConfigFile
  present <- PathIO.doesFileExist path
  pure $ if present then FromFile path else FromDefaults

-- | The configuration a run uses. A file must exist and parse.
readConfigSource :: ConfigSource -> IO DatasetConfig
readConfigSource (FromFile path) = readJson path
readConfigSource FromDefaults = pure def

-- | Put the parameters in the corpus, so a regeneration starts where this one did. A file is copied rather than
-- re-encoded: what the corpus carries is then what was actually read, key order and all.
writeConfigSource :: ConfigSource -> Path Abs File -> IO ()
writeConfigSource (FromFile path) destination = PathIO.copyFile path destination
writeConfigSource FromDefaults destination = writeJson destination (def :: DatasetConfig)

-- | The configuration of the corpus in a directory.
readCorpusConfig :: Path Abs Dir -> IO DatasetConfig
readCorpusConfig corpusDir = readJson (corpusDir </> corpusConfigFile)
