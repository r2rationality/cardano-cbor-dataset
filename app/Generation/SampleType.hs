module Generation.SampleType where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Text qualified as T

-- | Type of sample. A sample produced by the generator or manually created then validated by a Haskell node decoder to
-- determine its final type
data SampleType = Valid | Zap Int | ManualValid | ManualInvalid
  deriving (Eq, Ord, Show)

data SampleCategory = Accept | Reject
  deriving (Eq, Ord, Show)

sampleCategory :: SampleType -> SampleCategory
sampleCategory Valid = Accept
sampleCategory (Zap _) = Reject
sampleCategory ManualValid = Accept
sampleCategory ManualInvalid = Reject

renderSampleCategory :: SampleCategory -> Text
renderSampleCategory = \case
  Reject -> "reject"
  Accept -> "accept"

instance ToJSON SampleCategory where
  toJSON = toJSON . renderSampleCategory

instance FromJSON SampleCategory where
  parseJSON = withText "category" $ \text ->
    maybe (fail ("unknown category: " <> toString text)) pure (find ((== text) . renderSampleCategory) [Accept, Reject])

-- | Directory of a sample, relative to its rule directory. Every sample directory is exactly one directory deep, so a
-- runner finds the whole dataset with a single recursive walk and reads the expectation off the directory name: a
-- sample under @valid@ or @manual-valid@ must decode, a sample under anything else must be rejected.
sampleDirectory :: SampleType -> Text
sampleDirectory Valid = "valid"
sampleDirectory (Zap level) = "invalid-zap-" <> show level
sampleDirectory ManualValid = "manual-valid"
sampleDirectory ManualInvalid = "manual-invalid"

-- | The @valid@ run fills severity zero as well, so both directories need to be created before a run starts
sampleTargetDirectories :: SampleType -> [Text]
sampleTargetDirectories sampleType =
  let destinations = if sampleType == Valid then [Valid, Zap 0] else [sampleType]
   in map sampleDirectory destinations

fromSampleDirectory :: Text -> Maybe SampleType
fromSampleDirectory "valid" = Just Valid
fromSampleDirectory "manual-valid" = Just ManualValid
fromSampleDirectory "manual-invalid" = Just ManualInvalid
fromSampleDirectory dir
  | Just suffix <- T.stripPrefix "invalid-zap-" dir =
      Zap <$> readMaybe (toString suffix)
  | otherwise =
      Nothing

-- | Every sample type a dataset can hold.
sampleTypes :: [SampleType]
sampleTypes = Valid : ManualValid : ManualInvalid : map Zap zapLevels

-- | The generated sample types
generatedSampleTypes :: [SampleType]
generatedSampleTypes = Valid : map Zap (filter (> 0) zapLevels)

-- | Severity zero is the non-mutated sample the decoder rejects; one to three are the mutations of increasing severity.
zapLevels :: [Int]
zapLevels = [0 .. 3]

-- | Sample types that are supposed to be decoded
isValid :: SampleType -> Bool
isValid Valid = True
isValid ManualValid = True
isValid _ = False

-- | Sample types that are not supposed to be decoded
isInvalid :: SampleType -> Bool
isInvalid = not . isValid

-- | Sample types that have been manually created
isManual :: SampleType -> Bool
isManual ManualValid = True
isManual ManualInvalid = True
isManual _ = False

-- | One sample of a rule: its type, which names its directory, and the name of its files without their suffix.
data SampleName = SampleName
  { sampleNameType :: SampleType,
    sampleNameStem :: Text
  }
  deriving (Eq, Ord, Show)

-- | One sample of a corpus: its rule, and its name in the rule.
data SampleKey = SampleKey
  { sampleKeyRule :: Name,
    sampleKeyName :: SampleName
  }
  deriving (Eq, Ord, Show)

-- | @\<rule\>\/\<sample type\>\/\<stem\>@, relative to the corpus, for example @certificates/manual-invalid/bare-empty@.
-- The results file and the waivers name a sample by this text.
renderSampleKey :: SampleKey -> Text
renderSampleKey (SampleKey (Name rule) (SampleName kind stem)) = T.intercalate "/" [rule, sampleDirectory kind, stem]

-- | Read a key written by 'renderSampleKey'.
parseSampleKey :: Text -> Either Text SampleKey
parseSampleKey text = case T.splitOn "/" text of
  [rule, directory, stem]
    | not (T.null rule),
      not (T.null stem),
      Just kind <- fromSampleDirectory directory ->
        Right (SampleKey (Name rule) (SampleName kind stem))
  _ -> Left ("not a sample: " <> text <> ", expected <rule>/<sample type>/<stem>")

instance ToJSON SampleKey where
  toJSON = toJSON . renderSampleKey

instance FromJSON SampleKey where
  parseJSON = withText "sample" (either (fail . toString) pure . parseSampleKey)
