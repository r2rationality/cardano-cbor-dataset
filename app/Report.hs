-- | Raw per-sample verification results.
module Report (
  Reason,
  FailureKind (..),
  formatReason,
  writeResults,
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
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

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
-- Include every sample, including failures; presentation belongs to the caller.
writeResults :: FilePath -> [(FilePath, Either Reason ())] -> IO ()
writeResults jsonPath results = do
  createDirectoryIfMissing True $ takeDirectory jsonPath
  BL.writeFile jsonPath . encodePretty' configuration . object $
    [ Key.fromString sample .= outcomeValue outcome
    | (sample, outcome) <- results
    ]
  where
    outcomeValue (Right ()) = Bool True
    outcomeValue (Left reason) = toJSON $ formatReason reason
    configuration =
      defConfig
        { confIndent = Spaces 2
        , confCompare = compare
        , confTrailingNewline = True
        }
