-- | Raw per-sample results and the Markdown report derived from them.
module Report (
  Reason,
  FailureKind (..),
  formatReason,
  writeReport,
) where

import Data.Aeson (Value (Bool), object, toJSON, (.=))
import Data.Aeson.Encode.Pretty (
  Config (confCompare, confIndent, confTrailingNewline),
  Indent (Spaces),
  defConfig,
  encodePretty',
 )
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (IOMode (WriteMode), withBinaryFile)
import System.Process (CreateProcess (std_out), StdStream (UseHandle), proc, waitForProcess, withCreateProcess)

-- | The category and diagnostic of a sample failure.
data FailureKind
  = DecodeFailed
  | DecodeSucceeded
  | ReferenceMismatch
  | ByteExactMismatch
  | ReferenceUnreadable
  | SampleUnreadable
  | EncodeFailed
  | Unsupported

type Reason = (FailureKind, String)

formatReason :: Reason -> String
formatReason (kind, message) = category kind <> ": " <> message
  where
    category DecodeFailed = "decode"
    category DecodeSucceeded = "decode"
    category SampleUnreadable = "decode"
    category EncodeFailed = "encode"
    category ReferenceMismatch = "encode"
    category ByteExactMismatch = "encode"
    category ReferenceUnreadable = "encode"
    category Unsupported = "unsupported"

-- | Keys are corpus-relative POSIX paths including the .input.cbor suffix.
-- Write the raw results before rendering, including when samples failed.
writeReport :: FilePath -> FilePath -> FilePath -> [(FilePath, Either Reason ())] -> IO ()
writeReport dataset jsonPath markdownPath results = do
  createDirectoryIfMissing True $ takeDirectory jsonPath
  -- A failed render must not leave an older Markdown report beside new JSON.
  previous <- doesFileExist markdownPath
  if previous then removeFile markdownPath else pure ()
  BL.writeFile jsonPath . encodePretty' configuration . object $
    [ Key.fromString sample .= outcomeValue outcome
    | (sample, outcome) <- results
    ]
  scriptOverride <- lookupEnv "CBOR_REPORT_SCRIPT"
  let script = maybe "scripts/make-report.py" id scriptOverride
  status <- withBinaryFile markdownPath WriteMode $ \handle ->
    withCreateProcess (proc "python3" [script, dataset, jsonPath]) {std_out = UseHandle handle} $
      \_ _ _ process -> waitForProcess process
  case status of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      removeFile markdownPath
      ioError . userError $
        "make-report.py exited with " <> show code
  where
    outcomeValue (Right ()) = Bool True
    outcomeValue (Left reason) = toJSON $ formatReason reason
    configuration =
      defConfig
        { confIndent = Spaces 2
        , confCompare = compare
        , confTrailingNewline = True
        }
