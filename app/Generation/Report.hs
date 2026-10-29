module Generation.Report where

import Base.Json (writeJson)
import Base.Paths (abort)
import Cardano.Ledger.BaseTypes (getVersion)
import Data.Aeson (Value (Bool), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Char (isHexDigit)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Generation.Dataset
import Generation.EraSpec (EraSpec (..))
import Generation.SampleType (renderSampleKey)
import Generation.VerificationFailure
import Path
import Path.IO qualified as PathIO

-- | The result of verifying all the samples of a dataset
data Verification = Verification
  { -- | The list of outcomes for each sample in the dataset.
    verificationOutcomes :: [Outcome],
    -- | Whether all samples passed verification.
    verificationPassed :: Bool
  }
  deriving (Eq, Show)

instance Semigroup Verification where
  Verification o1 p1 <> Verification o2 p2 = Verification (o1 <> o2) (p1 && p2)

instance Monoid Verification where
  mempty = Verification [] True

-- | The outcome of verifying a single sample within a dataset.
data Outcome = Outcome
  { file :: DatasetFile,
    outcome :: Maybe VerificationFailure
  }
  deriving (Eq, Show)

-- | The revisions of the dataset and of the node code that produced a report.
data Revisions = Revisions
  { -- | The commit of this repository that holds the samples.
    datasetCommit :: Text,
    -- | The commit of the node code that decoded them.
    implementationCommit :: Text
  }
  deriving (Eq, Show)

-- | The revisions of a run, given by the environment variables:
--     - @CBOR_DATASET_COMMIT@
--     - @CBOR_IMPLEMENTATION_COMMIT@
readRevisions :: IO Revisions
readRevisions = Revisions <$> commitFrom "CBOR_DATASET_COMMIT" <*> commitFrom "CBOR_IMPLEMENTATION_COMMIT"
  where
    commitFrom name =
      lookupEnv name >>= \case
        Just value | isCommit (toText value) -> pure (toText value)
        _ -> abort $ toText name <> " must be set to a full commit hash (40 hexadecimal digits)"
    isCommit text = T.length text == 40 && T.all isHexDigit text

-- | The format of the report that 'writeVerificationResults' writes.
reportVersion :: Text
reportVersion = "2"

-- | A report is an object with the version of its format, the time and revisions of the run, and the outcome of
-- every sample in @results@. Keys of @results@ are corpus-relative POSIX paths including the .input.cbor suffix.
-- Include every sample, including failures; presentation belongs to the caller.
writeVerificationResults :: Path Abs File -> EraSpec -> Revisions -> Verification -> IO ()
writeVerificationResults jsonPath era revisions results = do
  PathIO.ensureDir (Path.parent jsonPath)
  now <- getPOSIXTime
  writeJson jsonPath . object $
    [ "version" .= reportVersion,
      "generated_at" .= (floor (now * 1000) :: Integer),
      "protocol_version" .= (show (getVersion (eraSpecProtocolVersion era) :: Integer) <> ".0" :: Text),
      "dataset_commit" .= datasetCommit revisions,
      "implementation_commit" .= implementationCommit revisions,
      "results"
        .= object
          [ Key.fromText (sampleFileName (renderSampleKey (datasetFileKey file)) InputFile) .= outcomeValue outcome
          | Outcome {file, outcome} <- verificationOutcomes results
          ]
    ]
  where
    outcomeValue Nothing = Bool True
    outcomeValue (Just reason) = toJSON $ formatVerificationFailure reason
