-- | Check the committed normalization vectors against the reference implementation.
--
-- The vectors are a specification artifact: their hex table was verified by hand, so this test asks whether the
-- implementation still agrees with the specification, not whether the vectors match whatever the implementation
-- currently does. A failure means one of the two has moved and the other has not, and the table in the vector
-- directory's README is the authority.
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Traversable
import Generation.Normalize (normalizeBytes)
import Path
import Path.IO qualified as PathIO
import Text.Printf (printf)

data Outcome = Passed | Failed Text

checkVector :: Path Abs Dir -> Text -> IO Outcome
checkVector directory name = do
  expectedPath <- (directory </>) <$> parseRelFile (toString (name <> expectedSuffix))
  hasOutput <- PathIO.doesFileExist expectedPath
  if not hasOutput
    then pure . Failed $ "no " <> name <> expectedSuffix <> " beside the input"
    else do
      inputPath <- (directory </>) <$> parseRelFile (toString (name <> inputSuffix))
      input <- BS.readFile (toFilePath inputPath)
      expected <- BS.readFile (toFilePath expectedPath)
      pure $ case normalizeBytes input of
        Left message -> Failed message
        Right actual
          | actual == expected -> Passed
          | otherwise -> Failed $ "got " <> hex actual <> ", want " <> hex expected

-- | The suffixes a vector is named by, which are the corpus's own: a sample and the bytes it must normalize to, told
-- apart by their names rather than by two directories.
inputSuffix :: Text
inputSuffix = ".input.cbor"

expectedSuffix :: Text
expectedSuffix = ".expected.cbor"

-- | Hex, so a mismatch can be read straight against the README's table without reaching for another tool.
hex :: BS.ByteString -> Text
hex = T.pack . concatMap (printf "%02x") . BS.unpack

-- | An output with no input is as much a problem as a mismatch: it means a case was half removed, and the run would
-- otherwise look clean while testing less than the table claims.
orphanedOutputs :: [Text] -> [Text]
orphanedOutputs entries =
  sort [name | entry <- entries, Just name <- [stripSuffix expectedSuffix entry], name `notElem` inputs]
  where
    inputs = [name | entry <- entries, Just name <- [stripSuffix inputSuffix entry]]

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix suffix name
  | T.isSuffixOf suffix name = Just $ T.take (T.length name - T.length suffix) name
  | otherwise = Nothing

main :: IO ()
main = do
  arguments <- getArgs
  directory <- PathIO.resolveDir' $ case arguments of
    (path : _) -> path
    [] -> "dataset/normalization"
  entries <- map (toText . toFilePath) . snd <$> PathIO.listDirRel directory
  let names = sort [name | entry <- entries, Just name <- [stripSuffix inputSuffix entry]]
      orphans = orphanedOutputs entries
  unless (null orphans) $
    die $
      "outputs with no input in '" <> toFilePath directory <> "': " <> show orphans
  if null names
    then die $ "no vectors in '" <> toFilePath directory <> "'"
    else do
      outcomes <- for names $ \name -> do
        outcome <- checkVector directory name
        case outcome of
          Passed -> putTextLn $ "ok    " <> name
          Failed message -> putTextLn $ "FAIL  " <> name <> " (" <> message <> ")"
        pure outcome
      let failed = length [() | Failed _ <- outcomes]
      printf "\n%d passed, %d failed\n" (length outcomes - failed) failed
      unless (failed == 0) exitFailure
