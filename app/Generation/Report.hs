module Generation.Report where

import Base.Json (writeJson)
import Data.Aeson (Value (Bool), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Generation.Dataset
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

-- | Keys are corpus-relative POSIX paths including the .input.cbor suffix. Include every sample, including failures;
-- presentation belongs to the caller.
writeVerificationResults :: Path Abs File -> Verification -> IO ()
writeVerificationResults jsonPath results = do
  PathIO.ensureDir (Path.parent jsonPath)
  writeJson jsonPath . object $
    [ Key.fromText (sampleFileName (renderSampleKey (datasetFileKey file)) InputFile) .= outcomeValue outcome
    | Outcome {file, outcome} <- verificationOutcomes results
    ]
  where
    outcomeValue Nothing = Bool True
    outcomeValue (Just reason) = toJSON $ formatVerificationFailure reason
