{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corpus
  ( VerificationMode (..),
    generateDataset,
    CorpusConfig (..),
    readCorpusConfig,
    verifyDataset,
  )
where

import Cardano.Crypto.Hash.Class (Hash, hashToStringAsHex, hashWith)
import Cardano.Crypto.Hash.SHA256 (SHA256)
import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Types (parseEither, withObject, (.!=), (.:), (.:?))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (digitToInt, isHexDigit, isSpace)
import Data.List (isInfixOf, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import LedgerRules
  ( EraSpec,
    RuleCheck,
    deserializeRule,
    eraSpecName,
    eraSpecRules,
    lookupRule,
    reserializeRule,
    ruleCheckByteExact,
    ruleCheckName,
  )
import Normalize (definiteForms, definiteStrings, normalizeBytes)
import Numeric (readHex)
import Paths
  ( ledgerWaiversName,
    listDirectoryChecked,
    parentWaiversName,
    publishDirectory,
    requireCBORFile,
    requireRealDirectory,
    specDefectsName,
  )
import Report
  ( FailureKind (..),
    Reason,
    formatReason,
    writeResults,
  )
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getPermissions,
    writable,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO (hPutStrLn)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

-- VERIFICATION MODES
data VerificationMode
  = DeserializeOnly
  | -- | Check references and write raw results to the caller's output path.
    CheckExpectedOutput FilePath

verificationModeName :: VerificationMode -> String
verificationModeName DeserializeOnly = "deserialize"
verificationModeName (CheckExpectedOutput _) = "expected"

data Expectation = MustDecode | MustReject
  deriving (Eq)

data DatasetFile = DatasetFile
  { datasetFilePath :: !FilePath,
    datasetFileRule :: !(Either String RuleCheck),
    datasetFileCategory :: !DatasetCategory,
    -- | @\<rule\>\/\<category\>\/\<name\>@, the name a report gives this
    -- sample. It carries no suffix, so a consumer can append either of them.
    datasetFileSample :: !FilePath,
    -- | The reference this sample's re-encoding must reproduce. Only a @valid@
    -- sample has one, and every @valid@ sample does: generation puts a sample
    -- it cannot decode under @invalid@ instead.
    datasetFileExpectedPath :: !(Maybe FilePath)
  }

-- | Where a sample lives, which is also what it must do. Generation decides
-- this per sample rather than by where the generator was aimed: bytes that
-- satisfy the CDDL but that the decoder rejects are not valid, so they go under
-- @invalid@ at severity zero, beside the mutations of the same sample.
data DatasetCategory = Valid | Zap !Int | ManualValid | ManualInvalid | Waived
  deriving (Eq)

-- | Directory of a category, relative to its rule directory. Every category is
-- exactly one directory deep, so a runner finds the whole corpus with a single
-- recursive walk and reads the expectation off the directory name: a sample
-- under @valid@ must decode, a sample under anything else must be rejected.
categoryName :: DatasetCategory -> FilePath
categoryName Valid = validCategoryName
categoryName (Zap level) = invalidCategoryPrefix <> show level
categoryName ManualValid = manualValidCategoryName
categoryName ManualInvalid = manualInvalidCategoryName
categoryName Waived = waivedCategoryName

-- | The one category that must decode.
validCategoryName :: FilePath
validCategoryName = "valid"

-- | Shared by every category that must be rejected, completed by the mutation
-- severity.
invalidCategoryPrefix :: FilePath
invalidCategoryPrefix = "invalid-zap-"

-- | Samples written by hand rather than generated.
--
-- Generation cannot reach everything. A rule whose values are bare bytes or
-- unbounded numbers survives almost any byte the mutator flips, a branch
-- nothing invalid ever opens with is never exercised, and the generator has no
-- reason to pick the largest value a type admits. Those cases are written out
-- instead, in two directories mirroring the generated ones, with the references
-- for the valid samples beside them as everywhere else.
--
-- Nothing generates in here, so it is the one part of a corpus a regeneration
-- has to carry over rather than rebuild.
manualValidCategoryName :: FilePath
manualValidCategoryName = "manual-valid"

manualInvalidCategoryName :: FilePath
manualInvalidCategoryName = "manual-invalid"

-- | Samples the specification refuses, that the decoder takes because the
-- constraint is checked somewhere else.
--
-- A constraint the specification states on a rule is not always enforced by the
-- type that decodes it: @withdrawals@ forbids an empty map, and @Withdrawals@
-- decodes one happily, because the ledger checks for emptiness at the single
-- transaction body field that carries it. The obligation still holds for any
-- implementation; only the place it is checked has moved. Bytes like these
-- belong in the corpus -- they are what the deferral looks like -- but they fit
-- neither side of the usual split, so they are kept apart and each one carries
-- a written reason naming where the check does live.
--
-- Only for a constraint that really is checked. Where the specification asks
-- for something the ledger does not enforce anywhere, there is nothing to defer
-- to, and the entry belongs in the era's incorrect specification list instead.
--
-- Verification holds these to decoding, not to being refused, which is what
-- makes the deferral falsifiable: the day the ledger tightens, the sample stops
-- decoding and the run says so instead of the reason quietly going stale.
waivedCategoryName :: FilePath
waivedCategoryName = "verification-by-parent"

-- | Suffix of the written reason a waived sample carries, named after the
-- sample the way a reference encoding is.
reasonSuffix :: FilePath
reasonSuffix = ".reason.txt"

-- | Suffix of every sample, and so what a runner globs for.
inputSuffix :: FilePath
inputSuffix = ".input.cbor"

-- | Suffix of a reference encoding. It sits beside the sample it belongs to and
-- is named after it, so a runner derives the reference path from the sample
-- path rather than looking it up in a second directory.
expectedSuffix :: FilePath
expectedSuffix = ".expected.cbor"

-- | The name a sample and its reference share, or 'Nothing' for a file that is
-- not a sample.
inputStem :: FilePath -> Maybe FilePath
inputStem = stripSuffix inputSuffix

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix text = reverse <$> stripPrefix (reverse suffix) (reverse text)

-- | What a category contributes to the generator seed.
--
-- Deliberately not 'categoryName'. The seed decides which bytes a run produces,
-- so tying it to a directory name would mean that moving the files changes the
-- corpus a given seed regenerates.
categorySeedLabel :: DatasetCategory -> String
categorySeedLabel Valid = "valid"
categorySeedLabel (Zap level) = "invalid/zap-" <> show level
categorySeedLabel ManualValid = manualValidCategoryName
categorySeedLabel ManualInvalid = manualInvalidCategoryName
categorySeedLabel Waived = waivedCategoryName

categoryExpectation :: DatasetCategory -> Expectation
categoryExpectation Valid = MustDecode
categoryExpectation (Zap _) = MustReject
categoryExpectation ManualValid = MustDecode
categoryExpectation ManualInvalid = MustReject
-- The specification refuses these and the decoder does not, which is the whole
-- point of the category: holding them to decoding is what pins the divergence.
categoryExpectation Waived = MustDecode

-- | What a sample must do. Its category settles this on its own, which is the
-- point of putting the generator output the decoder rejects under @invalid@
-- rather than leaving it in @valid@ without a reference.
datasetFileExpectation :: DatasetFile -> Expectation
datasetFileExpectation = categoryExpectation . datasetFileCategory

-- | Severity zero is the unmutated sample the decoder rejects; one to three are
-- the mutations of increasing severity.
zapLevels :: [Int]
zapLevels = [0 .. 3]

-- | Every category a corpus can hold.
datasetCategories :: [DatasetCategory]
datasetCategories = Valid : ManualValid : ManualInvalid : Waived : map Zap zapLevels

-- | Categories a rule is not expected to have.
--
-- A rule owes generated samples at every severity, so a missing one is a gap
-- worth naming. Hand written samples exist only where generation could not
-- reach, so having none is the normal case.
optionalCategory :: DatasetCategory -> Bool
optionalCategory ManualValid = True
optionalCategory ManualInvalid = True
optionalCategory Waived = True
optionalCategory _ = False

-- | The categories generation is aimed at. Severity zero is not among them: it
-- is filled by the @valid@ run, from the samples the decoder rejects.
generatedCategories :: [DatasetCategory]
generatedCategories = Valid : map Zap (filter (> 0) zapLevels)

-- | Reject entries that name no category at all.
--
-- No directory is demanded, not even @valid@. Version control tracks no empty
-- directory, so a rule whose samples all land somewhere else arrives without
-- the one it emptied: a rule the decoder never accepts has no @valid@, and a
-- rule whose mutations it never rejects is missing a severity. Treating any of
-- those as a broken corpus stopped verification of every other rule, so an
-- absent directory counts as zero samples and is named in the run summary
-- instead. A corpus with nothing in it at all is still caught, by the check
-- that it holds at least one file.
requireKnownCategories :: FilePath -> [FilePath] -> IO ()
requireKnownCategories path actual = do
  let known = map categoryName datasetCategories
      unknown = sort $ filter (`notElem` known) actual
  unless (null unknown) $
    die $
      "unexpected entries in '" <> path <> "': " <> show unknown

-- | The sample names of one category directory.
--
-- A reference sits beside the sample it belongs to, so the directory holds the
-- two suffixes and nothing else. Anything else is a corpus assembled by hand or
-- by an older generator, and reporting it is more use than walking past it.
requireSampleNames :: DatasetCategory -> FilePath -> [FilePath] -> IO [FilePath]
requireSampleNames category path fileNames = do
  let recognized name =
        isJust (inputStem name)
          || isJust (stripSuffix expectedSuffix name)
          || isJust (stripSuffix reasonSuffix name)
      unknown = sort $ filter (not . recognized) fileNames
  unless (null unknown) $
    die $
      "unexpected entries in '" <> path <> "': " <> show unknown
  let stems = [stem | Just stem <- map inputStem fileNames]
  -- A waiver with no reason is a sample nobody can weigh, so the reason counts
  -- as part of the sample rather than as documentation beside it.
  when (category == Waived) $ do
    let unexplained = sort [stem | stem <- stems, (stem <> reasonSuffix) `notElem` fileNames]
    unless (null unexplained) $
      die $
        "samples with no " <> reasonSuffix <> " in '" <> path <> "': " <> show unexplained
  pure stems

-- | Every sample in the corpus, and the categories that are not there.
listDatasetFiles :: EraSpec -> FilePath -> IO ([DatasetFile], [FilePath])
listDatasetFiles era root = do
  -- Everything in a corpus is a rule directory bar the configuration that
  -- produced it, which travels with the corpus so a reader can see what made it.
  actualRules <- filter (`notElem` [corpusConfigName, specDefectsName, ledgerWaiversName, parentWaiversName]) <$> listDirectoryChecked root
  when (null actualRules) $ die $ "dataset contains no rule directories: " <> root
  nested <- forM actualRules $ \ruleName -> do
    let rule = lookupRule era ruleName
        rulePath = root </> ruleName
    requireRealDirectory "dataset directory" rulePath
    actualCategories <- listDirectoryChecked rulePath
    requireKnownCategories rulePath actualCategories
    categoryFiles <- forM datasetCategories $ \category ->
      if categoryName category `notElem` actualCategories
        then pure ([], [ruleName </> categoryName category | not (optionalCategory category)])
        else do
          let categoryPath = rulePath </> categoryName category
          requireRealDirectory "dataset directory" categoryPath
          stems <- listDirectoryChecked categoryPath >>= requireSampleNames category categoryPath
          found <- forM stems $ \stem -> do
            let path = categoryPath </> stem <> inputSuffix
            requireCBORFile path
            pure $
              DatasetFile
                { datasetFilePath = path,
                  datasetFileRule = rule,
                  datasetFileCategory = category,
                  datasetFileSample = ruleName </> categoryName category </> stem,
                  datasetFileExpectedPath =
                    case category of
                      -- A hand written valid sample owes a reference just as a
                      -- generated one does, and keeps it in the same place.
                      Valid -> Just $ categoryPath </> stem <> expectedSuffix
                      ManualValid -> Just $ categoryPath </> stem <> expectedSuffix
                      _ -> Nothing
                }
          pure (found, [])
    pure (concatMap fst categoryFiles, concatMap snd categoryFiles)
  let files = concatMap fst nested
  when (null files) $ die $ "dataset contains no CBOR files: " <> root
  pure (files, concatMap snd nested)

-- | One accepted sample decoded and re-encoded: the raw re-encoding, and the
-- reference encoding a conforming implementation has to reproduce.
--
-- Normalizing is what makes an ordinary reference portable. An implementation
-- reproduces it from its own decoder without having to reproduce this encoder's
-- choice of container form or integer head, neither of which the format fixes.
--
-- A rule whose bytes are hashed is the exception, and its reference is the raw
-- re-encoding. There the container form is fixed, by the hash, so normalizing
-- would throw away the very thing the reference is there to pin down.
expectedBytes :: RuleCheck -> BS.ByteString -> Either Reason (BS.ByteString, BS.ByteString)
expectedBytes rule bytes = do
  reserialized <- first (\message -> (DecodeFailed, message)) $ reserializeRule rule bytes
  reference <-
    if ruleCheckByteExact rule
      then pure reserialized
      else first (\message -> (EncodeFailed, message)) $ normalizeBytes reserialized
  pure (reserialized, reference)

checkExpectation :: Expectation -> Either Reason a -> Either Reason (Maybe a)
checkExpectation MustReject (Left (DecodeFailed, _)) = Right Nothing
checkExpectation MustReject (Right _) = Left (DecodeSucceeded, "succeeded when expected to fail")
checkExpectation _ (Left reason) = Left reason
checkExpectation MustDecode (Right value) = Right $ Just value

checkDatasetFile :: (RuleCheck -> BS.ByteString -> Either Reason a) -> DatasetFile -> IO (Either Reason (BS.ByteString, Maybe a))
checkDatasetFile operation datasetFile = do
  readResult <- try (BS.readFile $ datasetFilePath datasetFile) :: IO (Either IOException BS.ByteString)
  pure $ case readResult of
    Left err -> Left (SampleUnreadable, "cannot read file: " <> show err)
    Right bytes -> do
      rule <- first (\message -> (Unsupported, message)) $ datasetFileRule datasetFile
      checked <-
        checkExpectation
          (datasetFileExpectation datasetFile)
          (operation rule bytes)
      pure (bytes, checked)

decodeSample :: RuleCheck -> BS.ByteString -> Either Reason ()
decodeSample rule bytes =
  first (\message -> (DecodeFailed, message)) $ deserializeRule rule bytes

verifyFile :: VerificationMode -> DatasetFile -> IO (Either Reason ())
verifyFile DeserializeOnly datasetFile =
  fmap (fmap $ const ()) $ checkDatasetFile decodeSample datasetFile
-- \| For a sample that must be rejected the only question is whether the decoder
-- rejects it, so ask the decoder directly rather than routing through the
-- re-encode and normalize steps, whose own failures would read as a rejection.
verifyFile (CheckExpectedOutput _) datasetFile
  | isNothing (datasetFileExpectedPath datasetFile) =
      fmap (fmap $ const ()) $ checkDatasetFile decodeSample datasetFile
verifyFile (CheckExpectedOutput _) datasetFile = do
  checked <- checkDatasetFile expectedBytes datasetFile
  case checked of
    Left reason -> pure $ Left reason
    Right (_, Nothing) -> pure $ Right ()
    Right (original, Just (reserialized, bytes)) ->
      case datasetFileExpectedPath datasetFile of
        Nothing ->
          pure $ Left (ReferenceUnreadable, "sample was accepted but has no reference encoding")
        Just expectedPath -> do
          expectedResult <- try (BS.readFile expectedPath) :: IO (Either IOException BS.ByteString)
          pure $ case expectedResult of
            Left err ->
              Left
                ( ReferenceUnreadable,
                  "cannot read expected output '" <> expectedPath <> "': " <> show err
                )
            Right referenceBytes -> do
              -- The hashed types are checked first: agreeing with the reference
              -- after normalization says nothing about a hash, so reporting the
              -- weaker mismatch for one of these rules would understate it.
              byteExactCheck original reserialized
              if bytes == referenceBytes
                then Right ()
                else
                  Left
                    ( ReferenceMismatch,
                      "normalized reserialization differs from '" <> expectedPath <> "'"
                    )
  where
    byteExactCheck original reserialized
      | not $ either (const False) ruleCheckByteExact (datasetFileRule datasetFile) = Right ()
      | reserialized == original = Right ()
      | otherwise =
          Left
            ( ByteExactMismatch,
              "reserialization differs from the original bytes, which this rule hashes"
            )

loadDataset :: EraSpec -> FilePath -> IO ([DatasetFile], [FilePath])
loadDataset era datasetDir = do
  requireRealDirectory "dataset directory" datasetDir
  root <- canonicalizePath datasetDir
  listDatasetFiles era root

reportResult :: DatasetFile -> Either Reason a -> IO (Either Reason a)
reportResult datasetFile result = do
  case result of
    Left reason ->
      hPutStrLn stderr $ "FAIL " <> datasetFilePath datasetFile <> " (" <> formatReason reason <> ")"
    Right _ -> pure ()
  pure result

verifyDataset :: EraSpec -> VerificationMode -> FilePath -> IO ()
verifyDataset era mode datasetDir = do
  (files, absent) <- loadDataset era datasetDir
  results <- forM files $ \datasetFile -> do
    outcome <- verifyFile mode datasetFile >>= reportResult datasetFile
    pure (datasetFile, outcome)

  putStrLn $ "Checked " <> show (length files) <> " " <> eraSpecName era <> " CBOR files"
  putStrLn $ "  mode:             " <> verificationModeName mode
  -- A rule whose mutations the decoder never rejects is a hole in the suite, so
  -- name it rather than letting the run look complete.
  unless (null absent) $
    putStrLn $
      "  absent, no samples: " <> intercalate ", " (sort absent)

  -- Only the mode that checks references can publish conformance results;
  -- a deserialize run knows nothing about re-encoding.
  case mode of
    DeserializeOnly -> pure ()
    CheckExpectedOutput reportPath -> do
      let sampleResults =
            [ (intercalate "/" (splitDirectories $ datasetFileSample file) <> inputSuffix, outcome)
            | (file, outcome) <- results
            ]
      writeResult <- try $ writeResults reportPath sampleResults
      case writeResult of
        Left (err :: IOException) -> die $ "cannot write verification results: " <> show err
        Right () -> putStrLn $ "  results:          " <> reportPath

  -- Publish all outcomes before returning a failing verification status.
  when (any (isLeft . snd) results) exitFailure

-- | Write the normalized reference encoding of one generated @valid@ sample,
-- returning why it has none when it has none: either the generator emitted
-- bytes the ledger decoder rejects, or the re-encoding has no reproducible
-- normal form. Neither stops generation, since the sample itself is still a
-- legitimate decoder test case.
emitReference :: RuleCheck -> FilePath -> FilePath -> IO (Maybe String)
emitReference rule validDir stem = do
  readResult <- try (BS.readFile $ validDir </> stem <> inputSuffix) :: IO (Either IOException BS.ByteString)
  case fmap (expectedBytes rule) readResult of
    Left err -> pure $ Just $ "cannot read file: " <> show err
    Right (Left reason) -> pure $ Just $ formatReason reason
    Right (Right (_, bytes)) -> do
      let outputPath = validDir </> stem <> expectedSuffix
      writeResult <- try (BS.writeFile outputPath bytes) :: IO (Either IOException ())
      pure $ case writeResult of
        Left err -> Just $ "cannot write expected output '" <> outputPath <> "': " <> show err
        Right () -> Nothing

-- | The hand written samples of an existing corpus, as paths relative to it.
--
-- Generation builds a corpus from nothing and refuses to write over one, so
-- these would be lost on every regeneration unless they are read out first and
-- written back in. Only the rules being generated are read: a rule that is no
-- longer a corpus root has no directory to put its samples back into, and
-- silently dropping them is better than inventing one.
--
-- The references are read along with the samples rather than rebuilt: they are
-- what the valid ones are expected to normalize to, and recomputing them here
-- would make the corpus agree with the implementation by construction instead
-- of checking it.
readManualSamples :: [RuleCheck] -> FilePath -> IO [(FilePath, BS.ByteString)]
readManualSamples rules corpus = do
  requireRealDirectory "corpus to adopt from" corpus
  fmap concat $ forM rules $ \rule ->
    fmap concat $ forM [ManualValid, ManualInvalid, Waived] $ \category -> do
      let relativeDir = ruleCheckName rule </> categoryName category
          handWrittenDir = corpus </> relativeDir
      present <- doesDirectoryExist handWrittenDir
      if not present
        then pure []
        else do
          fileNames <- listDirectoryChecked handWrittenDir
          forM fileNames $ \fileName -> do
            let path = handWrittenDir </> fileName
            -- A reason is prose, and travels with the sample it explains.
            unless (isJust (stripSuffix reasonSuffix fileName)) $ requireCBORFile path
            bytes <- BS.readFile path
            pure (relativeDir </> fileName, bytes)

-- | Number of references written for one rule, and the samples that have none.
emitRuleReferences :: FilePath -> RuleCheck -> IO (Int, Int)
emitRuleReferences staging rule = do
  createDirectoryIfMissing True (categoryDir Valid)
  generated <- referencesIn Rebuild (categoryDir Valid)
  -- A hand written valid sample owes a reference just as a generated one does,
  -- and nothing else would ever write it: generation does not produce the
  -- sample, so it cannot produce what the sample normalizes to either.
  handWritten <- referencesIn KeepExisting (categoryDir ManualValid)
  pure (fst generated + fst handWritten, snd generated + snd handWritten)
  where
    categoryDir category = staging </> ruleCheckName rule </> categoryName category

    referencesIn existing directory = do
      present <- doesDirectoryExist directory
      if not present
        then pure (0, 0)
        else do
          fileNames <- listDirectoryChecked directory
          let referenced = [stem | Just stem <- map (stripSuffix expectedSuffix) fileNames]
              wanted = case existing of
                Rebuild -> [stem | Just stem <- map inputStem fileNames]
                KeepExisting ->
                  [stem | Just stem <- map inputStem fileNames, stem `notElem` referenced]
          failures <- forM wanted $ \stem -> do
            outcome <- emitReference rule directory stem
            case outcome of
              Just message ->
                hPutStrLn stderr $ "FAIL " <> (directory </> stem <> inputSuffix) <> " (" <> message <> ")"
              Nothing -> pure ()
            pure outcome
          pure (length [() | Nothing <- failures], length [() | Just _ <- failures])

-- | Whether a reference that is already there is rewritten.
--
-- A generated sample's reference is rewritten every run, since the sample is
-- rewritten too. A hand written one is kept: it is what the sample is expected
-- to normalize to, and silently recomputing it would make the corpus agree with
-- the implementation by construction rather than check it.
data ExistingReferences = Rebuild | KeepExisting

sha256Hex :: BS.ByteString -> String
sha256Hex bytes =
  hashToStringAsHex (hashWith id bytes :: Hash SHA256 BS.ByteString)

seedFor :: Integer -> String -> DatasetCategory -> Int -> Int
seedFor topSeed rule category batch =
  case readHex $ take 8 digest of
    [(value, "")] -> fromInteger $ value `mod` 1500000000
    _ -> error "internal error: SHA-256 digest is not hexadecimal"
  where
    digest = sha256Hex $ BSC.pack $ show topSeed <> "|" <> rule <> "|" <> categorySeedLabel category <> "|" <> show batch

data BatchResult = BatchResult
  { batchLines :: ![String],
    batchExhausted :: !Bool
  }

generateBatch :: EraSpec -> Integer -> RuleCheck -> DatasetCategory -> Int -> Int -> IO BatchResult
generateBatch era topSeed rule category batch requested = do
  let ruleName = ruleCheckName rule
      name = categoryName category
      zapArguments =
        case category of
          Valid -> []
          Zap level -> ["--zap", show level]
          ManualValid -> []
          ManualInvalid -> []
          Waived -> []
      arguments =
        [ "--era",
          eraSpecName era,
          "--seed",
          show $ seedFor topSeed ruleName category batch,
          "--count",
          show requested
        ]
          <> zapArguments
          <> [ruleName]
  processResult <-
    try (readProcessWithExitCode "generate-cbor" arguments "") ::
      IO (Either IOException (ExitCode, String, String))
  (status, stdoutText, stderrText) <-
    case processResult of
      Left err -> die $ "cannot execute generate-cbor: " <> show err
      Right result -> pure result

  let outputLines = filter (not . all isSpace) $ map toString $ lines (toText stdoutText)
      exhausted = "failed to generate a sample after " `isInfixOf` stderrText
      failGeneration message =
        die $
          "generate-cbor failed for "
            <> ruleName
            <> "/"
            <> name
            <> message
            <> if null stderrText then "" else "\n" <> stderrText
  case status of
    ExitSuccess -> pure $ BatchResult outputLines False
    ExitFailure code
      | Zap _ <- category,
        exhausted -> do
          when (length outputLines >= requested) $
            failGeneration $
              ": invalid partial output for a batch of " <> show requested
          pure $ BatchResult outputLines True
      | otherwise -> failGeneration $ " with exit " <> show code

decodeHex :: String -> Either String BS.ByteString
decodeHex encoded
  | null encoded || odd (length encoded) || any (not . isHexDigit) encoded =
      Left $ "invalid hexadecimal output '" <> encoded <> "'"
  | otherwise = Right $ BS.pack $ decodePairs encoded
  where
    decodePairs [] = []
    decodePairs (high : low : rest) =
      fromIntegral (digitToInt high * 16 + digitToInt low) : decodePairs rest
    decodePairs _ = error "internal error: odd hexadecimal length"

-- | Where one generated sample belongs, decided by the decoder rather than by
-- which generator run produced it.
--
-- A sample the CDDL generator produced but the decoder rejects is not valid, so
-- it goes under @invalid@ at severity zero. A mutation the decoder still accepts
-- tests nothing, since the corpus would demand a rejection that is correct not
-- to happen, so it is dropped: the generator mutates bytes without consulting
-- the decoder, and rules whose fields are all optional, such as
-- @protocol_param_update@, often survive a mutation as another legal value.
data Destination = Keep !DatasetCategory | Drop

destinationOf :: RuleCheck -> DatasetCategory -> BS.ByteString -> Destination
destinationOf rule category bytes =
  case category of
    Valid
      | rejected -> Keep $ Zap 0
      | otherwise -> Keep Valid
    Zap _
      | rejected -> Keep category
      | otherwise -> Drop
    -- Nothing generates into the hand written categories, so nothing is ever
    -- routed to them.
    ManualValid -> Drop
    ManualInvalid -> Drop
    Waived -> Drop
  where
    rejected = isLeft $ deserializeRule rule bytes

-- | Samples written to the category asked for, samples routed to severity zero
-- because the decoder rejected them, mutations dropped for being decodable, and
-- the digests seen so far. A dropped mutation still joins the digests, so an
-- identical one later in the run costs a lookup rather than another decode.
data Accumulated = Accumulated !Int !Int !Int !(Set.Set String)

addGeneratedLine :: FilePath -> RuleCheck -> DatasetCategory -> Accumulated -> String -> IO Accumulated
addGeneratedLine ruleDir rule category (Accumulated written rejected dropped seen) encoded = do
  let ruleName = ruleCheckName rule
  bytes <-
    case decodeHex encoded of
      Left message -> die $ message <> " for " <> ruleName <> "/" <> categoryName category
      Right value -> pure value
  let digest = sha256Hex bytes
      seenNow = Set.insert digest seen
      writeTo destination position = writeBytes bytes digest destination position
      writeBytes content contentDigest destination position = do
        let fileName = printf "%05d-%s%s" position (take 16 contentDigest) inputSuffix :: FilePath
            path = ruleDir </> categoryName destination </> fileName
        writeResult <- try (BS.writeFile path content) :: IO (Either IOException ())
        case writeResult of
          Left (err :: IOException) -> die $ "cannot write '" <> path <> "': " <> show err
          Right () -> pure ()

      -- The same sample with its strings written out in one piece, where that
      -- is what the decoder was objecting to.
      --
      -- The generator picks a string's form by coin toss and the ledger reads
      -- only the definite one, so a rule carrying several strings almost never
      -- produces a sample that decodes: a transaction body has dozens, and all
      -- hundred of its samples were refused. The refused sample is kept, since
      -- the specification accepts it and that disagreement is worth recording,
      -- and the rewritten one is kept beside it so the rule has something to
      -- decode at all. Only strings are rewritten, so a container keeps the
      -- form it was generated in.
      -- Strings first, so a sample keeps the container forms it was generated
      -- with wherever that is enough. Only where the decoder still refuses it
      -- are the containers written out too.
      alsoAsDefinite position =
        case [candidate | Right candidate <- [definiteStrings bytes, definiteForms bytes], accepted candidate] of
          [] -> pure Nothing
          rewritten : _ -> do
            writeBytes rewritten (sha256Hex rewritten) Valid position
            pure $ Just (sha256Hex rewritten)
        where
          accepted candidate =
            candidate /= bytes
              && not (Set.member (sha256Hex candidate) seenNow)
              && case destinationOf rule Valid candidate of
                Keep Valid -> True
                _ -> False
  if Set.member digest seen
    then pure $ Accumulated written rejected dropped seen
    else case destinationOf rule category bytes of
      Drop -> pure $ Accumulated written rejected (dropped + 1) seenNow
      Keep (Zap 0)
        | Valid <- category -> do
            writeTo (Zap 0) (rejected + 1)
            recovered <- alsoAsDefinite (written + 1)
            case recovered of
              Nothing -> pure $ Accumulated written (rejected + 1) dropped seenNow
              Just extra ->
                pure $ Accumulated (written + 1) (rejected + 1) dropped (Set.insert extra seenNow)
      Keep destination -> do
        writeTo destination (written + 1)
        pure $ Accumulated (written + 1) rejected dropped seenNow

-- | Samples written for one rule and category. The count includes the ones the
-- @valid@ run routed to severity zero, so a rule still receives the number of
-- generated samples that was asked for however few of them decode.
addCases :: FilePath -> EraSpec -> Integer -> RuleCheck -> DatasetCategory -> Int -> Int -> IO Int
addCases staging era topSeed rule category target giveUpAfter = do
  let ruleName = ruleCheckName rule
      name = categoryName category
      ruleDir = staging </> ruleName
      -- The @valid@ run fills severity zero as well, so both directories have to
      -- exist before it starts.
      destinations
        | Valid <- category = [Valid, Zap 0]
        | otherwise = [category]
      maxAttempts = 3 * target
      batchSize = 128
      -- Nothing came back and the zapper has said so this many times running.
      --
      -- An exhausted batch is not bad luck: generate-cbor spends its own retry
      -- budget before reporting one, and it reports one when the generator has
      -- too few decision points to zap at this severity. That is a fact about
      -- the rule's shape, so three such batches say as much as three hundred,
      -- and the three hundred are three hundred processes that each reload the
      -- specification. A rule that yields anything at all keeps the full budget:
      -- hard to zap is not the same as impossible.
      givenUp accepted exhaustions = accepted == 0 && exhaustions >= giveUpAfter

      finish accepted dropped attempts batches exhaustions = do
        when (givenUp accepted exhaustions) $
          hPutStrLn stderr $
            "gave up zapping "
              <> ruleName
              <> "/"
              <> name
              <> " after "
              <> show exhaustions
              <> " batches with too few decision points to zap"
        when (accepted < target && not (givenUp accepted exhaustions)) $
          hPutStrLn stderr $
            "generated "
              <> show accepted
              <> "/"
              <> show target
              <> " samples for "
              <> ruleName
              <> "/"
              <> name
              <> " after "
              <> show attempts
              <> " attempts in "
              <> show batches
              <> " batches"
        when (exhaustions > 0) $
          hPutStrLn stderr $
            "retried " <> ruleName <> "/" <> name <> " after " <> show exhaustions <> " generator search exhaustions"
        -- A rule that drops many mutations has a generator producing weak ones,
        -- which is worth knowing even when the budget still meets the target.
        when (dropped > 0) $
          hPutStrLn stderr $
            "dropped "
              <> show dropped
              <> " "
              <> ruleName
              <> "/"
              <> name
              <> " mutations the decoder accepts"
        pure accepted
      loop :: Int -> Int -> Int -> Int -> Int -> Int -> Set.Set String -> IO Int
      loop written rejected dropped attempts batch exhaustions seen
        | accepted >= target || attempts >= maxAttempts || givenUp accepted exhaustions =
            finish accepted dropped attempts batch exhaustions
        | otherwise = do
            let requested = min batchSize $ min (target - accepted) (maxAttempts - attempts)
            result <- generateBatch era topSeed rule category batch requested
            let actualLines = length $ batchLines result
                attempted = actualLines + if batchExhausted result then 1 else 0
            when (not (batchExhausted result) && actualLines /= requested) $
              die $
                "unexpected "
                  <> ruleName
                  <> "/"
                  <> name
                  <> " output: "
                  <> show actualLines
                  <> " lines, expected "
                  <> show requested
            Accumulated nextWritten nextRejected nextDropped nextSeen <-
              foldM
                (addGeneratedLine ruleDir rule category)
                (Accumulated written rejected dropped seen)
                (batchLines result)
            loop
              nextWritten
              nextRejected
              nextDropped
              (attempts + attempted)
              (batch + 1)
              (exhaustions + if batchExhausted result then 1 else 0)
              nextSeen
        where
          accepted = written + rejected
  forM_ destinations $ \destination ->
    createDirectoryIfMissing True $ ruleDir </> categoryName destination
  loop 0 0 0 0 0 0 Set.empty

-- | How a corpus is generated: one seed for the lot, and how many samples each
-- rule gets.
--
-- Kept in a file beside the dataset rather than in its directory name. A name
-- can carry one count, and rules do not all need the same one: a union of a
-- hundred and twenty nine alternatives needs far more samples than a pair of
-- bytes before every branch is seen.
data CorpusConfig = CorpusConfig
  { corpusSeed :: !Integer,
    corpusSamples :: !Int,
    -- | Rules that need more, or fewer, than the default.
    corpusRuleSamples :: !(Map String Int),
    -- | How many fruitless batches to allow before giving up on zapping a rule.
    --
    -- A budget rather than a list of rules to skip: naming rules would be a
    -- hand kept copy of something generation already works out, and a rule that
    -- became zappable after a ledger change would stay skipped with nothing
    -- saying so. Too low a budget costs samples that the run reports; it cannot
    -- quietly lose them.
    corpusZapGiveUp :: !Int
  }

samplesFor :: CorpusConfig -> RuleCheck -> Int
samplesFor config rule =
  fromMaybe (corpusSamples config) (Map.lookup (ruleCheckName rule) (corpusRuleSamples config))

-- | What a corpus keeps its generation parameters in.
--
-- Inside the corpus rather than beside it, because two eras need not be
-- generated the same way: a rule that exists in one and not the other, or that
-- needs more samples in one, has nowhere to be said in a shared file.
corpusConfigName :: FilePath
corpusConfigName = "corpus.json"

-- | Fruitless batches allowed before a rule's zapping is abandoned.
defaultZapGiveUp :: Int
defaultZapGiveUp = 3

readCorpusConfig :: FilePath -> IO CorpusConfig
readCorpusConfig path = do
  present <- doesFileExist path
  unless present $ die $ "no corpus configuration at '" <> path <> "'"
  decoded <- eitherDecodeFileStrict' path
  case decoded >>= parseEither parse of
    Left message -> die $ "cannot read '" <> path <> "': " <> message
    Right config -> pure config
  where
    parse = withObject "corpus" $ \top ->
      CorpusConfig
        <$> top .: "seed"
        <*> top .: "samples"
        <*> (Map.mapKeys Text.unpack <$> (top .:? "rules" .!= (mempty :: Map Text.Text Int)))
        <*> top .:? "zapGiveUpAfter" .!= defaultZapGiveUp

generateDataset :: EraSpec -> Maybe FilePath -> [String] -> FilePath -> Maybe FilePath -> IO ()
generateDataset era adoptFrom only requestedOutputRoot configOverride = do
  requireRealDirectory "output directory" requestedOutputRoot
  permissions <- getPermissions requestedOutputRoot
  unless (writable permissions) $ die $ "output directory is not writable: " <> requestedOutputRoot
  outputRoot <- canonicalizePath requestedOutputRoot
  when (outputRoot == "/") $ die "output directory must not be the filesystem root"

  -- The era alone: what seed and how many samples produced it is in the
  -- configuration, which the corpus carries rather than its name.
  let destination = outputRoot </> eraSpecName era
      configPath = fromMaybe (destination </> corpusConfigName) configOverride
  config <- readCorpusConfig configPath
  configBytes <- BS.readFile configPath
  let topSeed = corpusSeed config
      rules = case only of
        [] -> eraSpecRules era
        names -> filter ((`elem` names) . ruleCheckName) (eraSpecRules era)
  -- Naming a rule that is not a root would otherwise generate nothing and look
  -- like it worked, so it is refused rather than ignored.
  forM_ only $ \name ->
    unless (name `elem` map ruleCheckName (eraSpecRules era)) $
      die $
        "not a rule of " <> eraSpecName era <> ": " <> name
  adopted <- traverse (readManualSamples rules) adoptFrom
  (generated, referenced, unreferenced) <- publishDirectory destination $ \staging -> do
    -- The corpus leaves with the parameters it was made from, so a reader can
    -- see what produced it and a regeneration starts from the same place.
    BS.writeFile (staging </> corpusConfigName) configBytes
    forM_ [specDefectsName, ledgerWaiversName, parentWaiversName] $ \carried -> do
      let carriedPath = destination </> carried
      present <- doesFileExist carriedPath
      when present $ BS.readFile carriedPath >>= BS.writeFile (staging </> carried)
    forM_ (fromMaybe [] adopted) $ \(relativePath, bytes) -> do
      let path = staging </> relativePath
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path bytes
    counts <- forM rules $ \rule ->
      forM generatedCategories $ \category -> do
        putStrLn $ "Generating " <> ruleCheckName rule <> "/" <> categoryName category
        addCases staging era topSeed rule category (samplesFor config rule) (corpusZapGiveUp config)
    references <- forM rules $ \rule -> do
      putStrLn $ "Emitting references for " <> ruleCheckName rule
      emitRuleReferences staging rule
    pure (sum $ concat counts, sum $ map fst references, sum $ map snd references)

  let target = length generatedCategories * sum (map (samplesFor config) rules)
  putStrLn $ "Generated " <> show generated <> "/" <> show target <> " samples in " <> destination
  putStrLn $ "  hand written adopted:    " <> show (length (fromMaybe [] adopted))
  putStrLn $ "  references emitted:      " <> show referenced
  putStrLn $ "  valid samples without one: " <> show unreferenced
