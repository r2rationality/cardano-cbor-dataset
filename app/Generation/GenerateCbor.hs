module Generation.GenerateCbor where

import Base.Text
import Control.Exception
import Data.ByteString qualified as BS
import Data.Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable
import GHC.IO.Exception
import Generation.DatasetConfig
import Generation.EraSpec
import Generation.GenerationRun
import Generation.GenerationTarget
import Generation.Rule
import Generation.SampleType
import Numeric (readHex)
import System.Process

-- | Call the generate-cbor executable to produce a batch of samples.
--
-- Return the number of output lines and a flag indicating whether the batch was exhausted.
generateCbor :: DatasetConfig -> GenerationTarget -> GenerationRun -> Int -> IO GenerateCborResult
generateCbor config target run requested = do
  let arguments = makeArguments config target run requested
  processResult <- try $ readProcessWithExitCode "generate-cbor" (toString <$> arguments) ""
  (status, stdoutText, stderrText) <-
    case processResult of
      Left (err :: IOException) -> die $ "cannot execute generate-cbor: " <> show err
      Right (status, stdoutText, stderrText) -> pure (status, toText stdoutText, toText stderrText)

  outputLines <- for (filter (not . T.all isSpace) $ T.lines stdoutText) decodeHex
  let exhausted = "failed to generate a sample after " `T.isInfixOf` stderrText
  case status of
    ExitSuccess -> pure $ GenerateCborResult outputLines False
    ExitFailure code
      | Zap _ <- targetSampleType target,
        exhausted -> do
          when (length outputLines >= requested) $ failGeneration target stderrText $ ": invalid partial output for a batch of " <> show requested
          pure $ GenerateCborResult outputLines True
      | otherwise -> failGeneration target stderrText $ " with exit " <> show code

data GenerateCborResult = GenerateCborResult
  { batchLines :: [BS.ByteString],
    batchExhausted :: Bool
  }
  deriving (Eq, Show)

-- | Construct the list of command-line arguments for the generate-cbor executable.
makeArguments :: DatasetConfig -> GenerationTarget -> GenerationRun -> Int -> [Text]
makeArguments config target run requested = do
  let rule = targetRule target
      sampleType = targetSampleType target

      ruleName = ruleNameText rule
  [ "--era",
    eraSpecName (targetEra target),
    "--seed",
    show $ seedFor config target run,
    "--count",
    show requested
    ]
    <> zapArguments sampleType
    <> [ruleName]

-- | Compute the seed for a given dataset configuration, generation target, and run.
seedFor :: DatasetConfig -> GenerationTarget -> GenerationRun -> Int
seedFor config target run =
  case readHex (toString (T.take 8 digest)) of
    [(value, "")] -> fromInteger $ value `mod` 1500000000
    _ -> error "internal error: SHA-256 digest is not hexadecimal"
  where
    digest = sha256Hex $ T.encodeUtf8 $ show (datasetSeed config) <> "|" <> targetRuleName target <> "|" <> sampleDirectory (targetSampleType target) <> "|" <> show (batch run)

-- | Construct the list of command-line arguments for the zap option, if applicable
zapArguments :: SampleType -> [Text]
zapArguments (Zap level) = ["--zap", show level]
zapArguments _ = []

failGeneration :: GenerationTarget -> Text -> Text -> IO a
failGeneration target stderrText message =
  die $
    toString $
      "generate-cbor failed for "
        <> targetRuleName target
        <> "/"
        <> targetDirectoryName target
        <> message
        <> if T.null stderrText then "" else "\n" <> stderrText
