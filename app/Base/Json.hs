-- | JSON as this project writes it: one entry per line, keys in sorted order and a newline at the end, so two versions
-- of a file diff cleanly.
module Base.Json where

import Base.Paths (abort, display, writeLazyBytes)
import Data.Aeson (FromJSON, Options (..), ToJSON, eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), NumberFormat (Generic), defConfig, encodePretty')
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.List (stripPrefix)
import Path (File, Path, toFilePath)
import Path.IO qualified as PathIO

-- | A value encoded as this project writes JSON.
encodeJson :: (ToJSON a) => a -> LBS.ByteString
encodeJson = encodePretty' format
  where
    format =
      defConfig
        { confIndent = Spaces 2,
          confCompare = compare,
          confTrailingNewline = True,
          -- Plain decimals, so a number reads as it is written by hand.
          confNumFormat = Generic
        }

-- | Write a value to a file as this project writes JSON.
writeJson :: (ToJSON a) => Path b File -> a -> IO ()
writeJson path = writeLazyBytes path . encodeJson

-- | Read a value from a JSON file. Stop with a message that names the file if it is missing or does not decode.
readJson :: (FromJSON a) => Path b File -> IO a
readJson path = do
  present <- PathIO.doesFileExist path
  unless present $ abort $ "no file at '" <> display path <> "'"
  decoded <- eitherDecodeFileStrict' (toFilePath path)
  either (\message -> abort $ "cannot read '" <> display path <> "': " <> toText message) pure decoded

-- | JSON options for a record whose fields share a prefix: each key is a field name without the prefix, starting in
-- lower case. With the prefix @dataset@, @datasetBatchSize@ is written as @batchSize@.
withoutFieldPrefix :: String -> Options -> Options
withoutFieldPrefix prefix options =
  options {fieldLabelModifier = withoutPrefix . fieldLabelModifier options}
  where
    withoutPrefix label = maybe label lowerFirst (stripPrefix prefix label)
    lowerFirst = \case
      initial : rest -> toLower initial : rest
      [] -> []

-- | JSON options that leave out a field whose value is 'Nothing', rather than writing it as @null@.
withNoNullFields :: Options -> Options
withNoNullFields options = options {omitNothingFields = True}
