{-# LANGUAGE QuasiQuotes #-}

-- | Filesystem checks shared by the commands that read or publish trees of CBOR files. Every check refuses symbolic
-- links, so a corpus cannot smuggle reads or writes outside the directory it names.
--
-- Paths are @path@'s well-typed ones: a directory and a file are different types, and so are an absolute and a relative
-- path. Every check below demands an absolute path, which is what stops a path assembled from rule and sample names
-- being read against whatever directory the process happens to be in. Text crosses into a path here, at the one layer
-- that touches the filesystem.
module Base.Paths where

import Base.Extra
import Control.Exception (IOException, onException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.IO qualified as TIO
import Path
import Path.IO
import Path.IO qualified as PathIO
import System.FilePath qualified as FilePath
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files

-- | Exit with a message, which every check here builds out of 'Text'.
abort :: Text -> IO a
abort = die . toString

-- | Write a message to standard error, and continue.
warn :: Text -> IO ()
warn = TIO.hPutStrLn stderr

-- | How a path reads in a message.
display :: Path b t -> Text
display = toText . toFilePath

-- | Run an action on a directory that exists, or give an empty result when the directory is absent.
whenDirExists :: (Monoid a) => Path b Dir -> IO a -> IO a
whenDirExists directory action = do
  present <- PathIO.doesDirExist directory
  whenMonoid present action

-- | The bytes of a file.
readBytes :: Path b File -> IO BS.ByteString
readBytes = BS.readFile . toFilePath

-- | Write bytes to a file, replacing what it held.
writeBytes :: Path b File -> BS.ByteString -> IO ()
writeBytes = BS.writeFile . toFilePath

-- | Write lazy bytes, such as an encoded JSON value, to a file.
writeLazyBytes :: Path b File -> LBS.ByteString -> IO ()
writeLazyBytes = LBS.writeFile . toFilePath

-- | The text of a file.
readText :: Path b File -> IO Text
readText = TIO.readFile . toFilePath

-- | Write text to a file, replacing what it held.
writeText :: Path b File -> Text -> IO ()
writeText = TIO.writeFile . toFilePath

-- | A subdirectory named by text, such as a rule or a sample type.
infixl 3 <//>

(<//>) :: Path r Dir -> Text -> IO (Path r Dir)
base <//> segment = do
  rel <- relativeDir segment
  pure (base </> rel)

-- | A subdirectory named by text, such as a rule or a sample type.
(<///>) :: Path r Dir -> Text -> IO (Path r File)
base <///> segment = do
  rel <- relativeFile segment
  pure (base </> rel)

-- | Append a list of directories
infixl 4 </*/>

(</*/>) :: Path r Dir -> [Text] -> IO (Path r Dir)
base </*/> [] = pure base
base </*/> (s : rest) = base <//> s >>= (</*/> rest)

-- | Append a file at the end of a list of directories
infixl 4 </*//>

(</*//>) :: Path r Dir -> NonEmpty Text -> IO (Path r File)
base </*//> (segment :| []) = base <///> segment
base </*//> (segment :| [s1]) = base <//> segment >>= (<///> s1)
base </*//> (segment :| (s1 : s2 : ss)) = do
  d1 <- base <//> segment
  d2 <- d1 <//> s1
  d2 </*//> (s2 :| ss)

-- | One relative segment named by text, such as a rule or a sample type.
--
-- The names come from the era specification and from directory listings, so they are already the names of things that
-- exist; parsing can still fail, and throwing says so rather than inventing a path.
relativeDir :: Text -> IO (Path Rel Dir)
relativeDir = parseRelDir . toString

relativeFile :: Text -> IO (Path Rel File)
relativeFile = parseRelFile . toString

-- | A path the caller gave us, resolved against the working directory when it is relative. This is where a command line
-- argument becomes a path.
absoluteDir :: Text -> IO (Path Abs Dir)
absoluteDir = PathIO.resolveDir' . toString

absoluteFile :: Text -> IO (Path Abs File)
absoluteFile = PathIO.resolveFile' . toString

-- | A directory the caller gave us, resolved against a working directory without touching the filesystem, so a command
-- line parser can do it.
resolveDirAgainst :: Path Abs Dir -> Text -> Either Text (Path Abs Dir)
resolveDirAgainst workingDir raw =
  first (toText . displayException) $ parseAbsDir (withoutDotSegments workingDir raw <> "/")

-- | A file the caller gave us, resolved against a working directory without touching the filesystem, so a command line
-- parser can do it.
resolveFileAgainst :: Path Abs Dir -> Text -> Either Text (Path Abs File)
resolveFileAgainst workingDir raw
  | FilePath.hasTrailingPathSeparator (toString raw) = Left $ "expected a file, not a directory: " <> raw
  | otherwise = first (toText . displayException) $ parseAbsFile (withoutDotSegments workingDir raw)

-- | An absolute path with its @.@ and @..@ segments removed from the text.
--
-- @path@ refuses both kinds of segment, but a person types @../corpus@. The segments are removed as text, so @link/..@
-- is not the directory the link leads out of: every check in this module refuses a symbolic link anyway.
withoutDotSegments :: Path Abs Dir -> Text -> FilePath
withoutDotSegments workingDir raw =
  "/" <> intercalate "/" (reverse (foldl' step [] segments))
  where
    typed = toString raw
    full = if FilePath.isAbsolute typed then typed else toFilePath workingDir FilePath.</> typed
    segments = filter (not . null) (FilePath.splitDirectories full)
    step kept segment = case segment of
      "/" -> kept
      "." -> kept
      ".." -> drop 1 kept
      _ -> segment : kept

-- | The name of a directory, without the separator @path@ keeps on the end.
dirName :: Path Rel Dir -> Text
dirName = toText . FilePath.dropTrailingPathSeparator . toFilePath

-- | The name of a file, which is what a corpus calls a sample.
fileName :: Path Rel File -> Text
fileName = toText . toFilePath

-- | 'Nothing' when the path does not exist; any other failure to inspect it is fatal, since carrying on would treat an
-- unreadable path as an absent one.
--
-- This stats the link itself rather than following it, which is what lets every check below refuse a symbolic link
-- instead of silently reporting on whatever it points at.
pathStatus :: Path Abs t -> IO (Maybe FileStatus)
pathStatus path = do
  result <- try $ getSymbolicLinkStatus (toFilePath path)
  case result of
    Left (err :: IOException)
      | isDoesNotExistError err -> pure Nothing
      | otherwise -> abort $ "cannot inspect path '" <> display path <> "': " <> show err
    Right status -> pure $ Just status

createOutputDirectory :: Path Abs Dir -> IO ()
createOutputDirectory outputDir = do
  requireRealDirectory "output directory" outputDir
  permissions <- getPermissions outputDir
  unless (writable permissions) $ abort $ "output directory is not writable: " <> display outputDir
  when (Path.parent outputDir == outputDir) $ abort "output directory must not be the filesystem root"

-- | Exit unless the path is a directory and not a symbolic link. The description names the role the caller expects of
-- it, such as @\"dataset directory\"@, and is what the reader sees in the failure.
requireRealDirectory :: Text -> Path Abs Dir -> IO ()
requireRealDirectory description path = do
  status <- pathStatus path
  case status of
    Nothing -> abort $ "missing " <> description <> ": " <> display path
    Just entry
      | isSymbolicLink entry -> abort $ description <> " must not be a symbolic link: " <> display path
      | isDirectory entry -> pure ()
      | otherwise -> abort $ "expected " <> description <> ": " <> display path

-- | Exit unless the path is a regular @.cbor@ file and not a symbolic link.
requireCBORFile :: Path Abs File -> IO ()
requireCBORFile path = do
  status <- pathStatus path
  case status of
    Just entry
      | isSymbolicLink entry -> abort $ "dataset files must not be symbolic links: " <> display path
      | isRegularFile entry && fileExtension path == Just ".cbor" -> pure ()
    _ -> abort $ "expected a .cbor dataset file: " <> display path

-- | What a directory holds, named relative to it and in a stable order.
listEntries :: Path Abs Dir -> IO ([Path Rel Dir], [Path Rel File])
listEntries directory = do
  result <- try (PathIO.listDirRel directory)
  case result of
    Left (err :: IOException) ->
      abort $ "cannot list directory '" <> display directory <> "': " <> show err
    Right (dirs, files) -> pure (sort dirs, sort files)

-- | The names of the directories one directory holds.
listDirectoryNames :: Path Abs Dir -> IO [Text]
listDirectoryNames = fmap (map dirName . fst) . listEntries

-- | The names of the files one directory holds.
listFileNames :: Path Abs Dir -> IO [Text]
listFileNames = fmap (map fileName . snd) . listEntries

-- | Remove a staging tree, ignoring any failure to do so. This only runs while unwinding from an earlier error, and
-- that error is the one worth reporting.
cleanupDirectory :: Path Abs Dir -> IO ()
cleanupDirectory path = do
  _ <- try (PathIO.removeDirRecur path) :: IO (Either IOException ())
  pure ()

-- | Build the destination in a sibling staging tree and move it into place, so an interrupted or failed run never
-- leaves a half-written directory at the final path. The build can read a directory already at the destination: it is
-- replaced only once the build has succeeded, and put back if its replacement cannot be moved in.
publishDirectory :: Path Abs Dir -> (Path Abs Dir -> IO a) -> IO a
publishDirectory destination build = do
  present <- isJust <$> pathStatus destination
  when present $ requireRealDirectory "destination directory" destination
  -- Both siblings match the @.*.tmp-*@ pattern the repository ignores, so a run
  -- killed halfway leaves nothing that looks like part of the dataset.
  let sibling suffix = PathIO.createTempDir (Path.parent destination) ("." <> toString (dirName (Path.dirname destination)) <> suffix)
  staging <- sibling ".tmp-"
  let cleanup = cleanupDirectory staging
  result <- (setFileMode (toFilePath staging) 0o755 >> build staging) `onException` cleanup
  if not present
    then PathIO.renameDir staging destination `onException` cleanup
    else do
      -- A fresh name for the directory being replaced: created to reserve it,
      -- then removed so the rename does not land inside it.
      replaced <- sibling ".tmp-replaced-"
      PathIO.removeDir replaced
      PathIO.renameDir destination replaced `onException` cleanup
      PathIO.renameDir staging destination `onException` (PathIO.renameDir replaced destination >> cleanup)
      cleanupDirectory replaced
  pure result

-- | What a corpus keeps its generation parameters in.
--
-- Inside the corpus rather than beside it, because two eras need not be generated the same way. It carries the waivers
-- too: a waiver changes what a run asks of the corpus, so it belongs with everything else that does.
corpusConfigFile :: Path Rel File
corpusConfigFile = [relfile|corpus.json|]

-- | What a corpus keeps its coverage measurement in.
--
-- Inside the corpus, so a corpus and the measurement it is held to on its next generation are always replaced together.
corpusCoverageFile :: Path Rel File
corpusCoverageFile = [relfile|coverage.json|]
