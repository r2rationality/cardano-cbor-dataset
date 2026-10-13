-- | Which obligations a corpus actually fulfils.
--
-- Two independent judgements are made about every sample and then compared. The
-- specification validator says whether the bytes satisfy the rule; a structural
-- walk says which particular constraint they break. Neither alone is enough:
-- the validator does not say what failed, and the walk does not know what the
-- specification thinks.
--
-- Where the two disagree, the disagreement is the output. A sample the
-- validator accepts cannot break a derived constraint, so a walk that says it
-- does is a defect in the derivation, not evidence of coverage.
module ObligationCoverage
  ( Detail (..),
    reportDataset,
    reportRule,
    Measurement (..),
    measureDataset,
    unexaminedCount,
    unreachedCount,
    specificationRuleCount,
    specificationRules,
    coverageOf,
    loadSamples,
    Report (..),
    Coverage (..),
    SpecDefect (..),
    Status (..),
    Waiver (..),
    coverageGiven,
    waiverName,
    Cause (..),
    renderCause,
    Finding (..),
    Sample (..),
    Source (..),
    Validity (..),
    Judged (..),
    judge,
  )
where

import CborTerm
  ( arrayEntries,
    decodeTaggedTerm,
    entryCount,
    followPath,
    isIndefinite,
    leadingUnsigned,
    mapHasDuplicateKey,
    mapHasForeignKey,
    mapLacks,
    matchesLiteral,
    matchesPrimitive,
    repeatsAnEntry,
    shapeFits,
    termInteger,
    termTag,
    withRepeatedEntry,
  )
import Codec.CBOR.Cuddle.CBOR.Validator (ValidateCBORError (..), validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (isValid)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Codec.CBOR.Cuddle.IndexMappable (mapIndex)
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term (Term (..), decodeTerm, encodeTerm)
import Codec.CBOR.Write qualified as CBOR
import Data.Aeson (FromJSON (..), Value (Object), eitherDecodeFileStrict', withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (isSuffixOf, partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Obligations
  ( Category (..),
    Obligation (..),
    ObligationKind (..),
    Shape (..),
    foldedRule,
    obligationCategory,
    obligationsFor,
    renderKind,
    renderObligation,
    renderPath,
  )
import Paths (corpusConfigName, listDirectoryChecked, requireRealDirectory)
import Reachability (Reachability, eraReachability, eraRoot, fromRoots)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName, (</>))

-- COMMANDS

-- | How much of each rule's detail to print before the summary.
data Detail
  = -- | Only the obligations the corpus still owes. These are the ones that
    -- ask for work, and an untested one does not: nothing was measured there,
    -- so there is nothing to go and generate yet.
    OutstandingOnly
  | -- | Every obligation, including the ones already met and the ones this
    -- check cannot decide.
    Everything
  deriving (Eq, Show)

-- | Report obligation coverage for every rule directory in a corpus.
--
-- The corpus decides which rules are examined, not the specification: a rule
-- with no directory has no samples, so there is nothing to measure and
-- reporting it as uncovered would be reporting a measurement never made.
reportDataset :: Detail -> String -> FilePath -> IO ()
reportDataset detail era corpusDir = do
  measured <- measureDataset (Just detail) era corpusDir
  mapM_ putTextLn (renderSummary (measuredRoot measured) corpusDir (measuredRules measured))
  putTextLn ""
  mapM_
    putTextLn
    ( renderUnexamined
        (measuredRoot measured)
        (measuredReachability measured)
        (measuredRuleNames measured)
    )

-- | What a corpus fulfils, as data rather than as lines.
--
-- The printed report and anything built later read the same measurement, so a
-- page and a terminal run cannot disagree about a number.
data Measurement = Measurement
  { measuredRoot :: !(CTreeRoot MonoReferenced),
    measuredReachability :: !Reachability,
    measuredRuleNames :: ![Text],
    measuredRules :: ![Report]
  }

-- | Measure every rule directory in a corpus.
--
-- A detail prints each rule as it goes; 'Nothing' keeps quiet, which is what a
-- caller assembling a page wants.
measureDataset :: Maybe Detail -> String -> FilePath -> IO Measurement
measureDataset detail era corpusDir = do
  root <- either die pure (eraRoot era)
  requireRealDirectory "corpus directory" corpusDir
  entries <- listDirectoryChecked corpusDir
  ruleDirs <- filterM (\entry -> doesDirectoryExist (corpusDir </> entry)) entries
  defects <- readSpecDefects corpusDir
  reports <- forM ruleDirs $ \ruleDir ->
    case detail of
      Nothing -> measureRule defects root corpusDir ruleDir
      Just shown -> do
        report <- reportRule defects shown root corpusDir ruleDir
        putTextLn ""
        pure report
  reachability <- either die pure (eraReachability era ruleDirs)
  pure
    Measurement
      { measuredRoot = root,
        measuredReachability = reachability,
        measuredRuleNames = map toText ruleDirs,
        measuredRules = reports
      }

-- | Report one rule, and return it so the summary can be built from the same
-- numbers the detail was printed from.
reportRule :: [(Waiver, SpecDefect)] -> Detail -> CTreeRoot MonoReferenced -> FilePath -> FilePath -> IO Report
reportRule defects detail root corpusDir ruleDir = do
  report <- measureRule defects root corpusDir ruleDir
  mapM_ putTextLn (renderReport detail report)
  pure report

-- | One rule's coverage, with nothing printed.
measureRule :: [(Waiver, SpecDefect)] -> CTreeRoot MonoReferenced -> FilePath -> FilePath -> IO Report
measureRule defects root corpusDir ruleDir = do
  samples <- loadSamples (corpusDir </> ruleDir)
  let name = Name (toText ruleDir)
      mine = [entry | entry@(_, defect) <- defects, defectRule defect == toText ruleDir]
  pure (coverageGiven mine root name (obligationsFor root name) samples)

-- | The incorrect specification entries an era records, or none when it records
-- no file at all.
readSpecDefects :: FilePath -> IO [(Waiver, SpecDefect)]
readSpecDefects corpusDir = do
  -- Beside the seed and the sample counts: a waiver changes what a run asks of
  -- the corpus, so it belongs with everything else that does.
  let path = corpusDir </> corpusConfigName
  present <- doesFileExist path
  if not present
    then pure []
    else
      waiverEntries
        <$> (either (die . (("cannot read '" <> path <> "': ") <>)) pure =<< eitherDecodeFileStrict' path)

-- REPORT

data Report = Report
  { reportRule' :: !Name,
    reportJudged :: ![Judged],
    reportCoverage :: ![Coverage],
    reportFindings :: ![Finding]
  }

data Coverage = Coverage
  { coverageObligation :: !Obligation,
    coverageStatus :: !Status,
    coverageWitnesses :: ![Text]
  }

data Status
  = Fulfilled
  | -- | Nothing is owed here, for a reason that is written down rather than
    -- measured. Which reason decides what the entry means.
    Waived !Waiver !Text
  | -- | Measured against every candidate sample, and none of them met it. A
    -- debt the corpus still owes, and the only status that names work to do.
    Outstanding
  | -- | No verdict was reached, so this is not a gap but a blind spot. Never
    -- report it as one: saying "not covered" about something never examined
    -- claims more than the run knows.
    Untested !Cause
  deriving (Eq, Show)

-- | Why an obligation is settled without a sample showing it enforced.
data Waiver
  = -- | The decoder for this rule does not enforce the constraint, and the
    -- decoder of whatever carries it does. The obligation holds for any
    -- implementation: only the place it is checked has moved, and a sample
    -- under the carried category shows what this rule alone lets through.
    VerifiedByParent
  | -- | The ledger enforces the constraint right here and refuses the bytes,
    -- but the specification validator accepts them. So no sample can witness
    -- the obligation: the matcher only offers a rejected sample to a reject
    -- obligation, and this one the specification calls valid.
    --
    -- Not a doubt about the obligation. The CDDL states it, the ledger agrees
    -- and says so in its own words, and only the tooling that reads the CDDL
    -- falls short.
    VerifiedByLedger
  | -- | The specification asks for something the ledger does not enforce
    -- anywhere, and is judged wrong to ask. No sample goes with it: bytes the
    -- ledger is right to accept are not a divergence to keep, so the entry is a
    -- statement about the specification and lives in the era's list.
    IncorrectSpecification
  deriving (Eq, Show, Bounded, Enum)

-- | One entry of an era's incorrect specification list: which obligation it
-- speaks for, and why the specification is wrong to state it.
--
-- The obligation is named the way the report prints it, so an entry can be read
-- and written without the derivation at hand. An entry matching nothing is a
-- finding rather than a no-op: a specification that has since been fixed should
-- make the run say so.
data SpecDefect = SpecDefect
  { defectRule :: !Text,
    defectPath :: !Text,
    defectDemand :: !Text,
    defectReason :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SpecDefect where
  parseJSON = withObject "waiver" $ \o ->
    SpecDefect <$> o .: "rule" <*> o .: "path" <*> o .: "demand" <*> o .: "reason"

-- | A waiver as the corpus configuration writes it: an obligation named the way
-- the page prints it, the kind of waiver, and why.
newtype Waivers = Waivers {waiverEntries :: [(Waiver, SpecDefect)]}

instance FromJSON Waivers where
  parseJSON = withObject "corpus configuration" $ \o -> do
    declared <- o .:? "waivers" .!= []
    Waivers <$> traverse entry declared
    where
      entry = withObject "waiver" $ \o -> (,) <$> (o .: "kind" >>= kindNamed) <*> parseJSON (Object o)
      kindNamed :: Text -> Parser Waiver
      kindNamed = \case
        "verified-by-parent" -> pure VerifiedByParent
        "verified-by-ledger" -> pure VerifiedByLedger
        "incorrect-specification" -> pure IncorrectSpecification
        other -> fail ("unknown waiver kind: " <> toString other)

-- | Why no verdict was reached, which decides what would change the answer.
data Cause
  = -- | Nothing to try it against. More samples of the right kind would settle
    -- it, and the matcher is not the limit.
    NoSamples
  | -- | The subject never resolved in any sample. More samples will not help:
    -- an alternative that cannot be told from its sibling, a key a sample may
    -- leave out, an entry with no fixed position.
    NoPath
  | -- | A custom validator states the constraint and the tree does not
    -- describe what it checks, so nothing here can say what breaking it looks
    -- like.
    NoDescription
  deriving (Eq, Show)

-- | Something the run found that is not about coverage.
data Finding
  = -- | The specification rejects a sample the corpus calls valid: the ledger
    -- decoder accepts bytes the specification forbids. A real divergence, and
    -- the most valuable thing this command can turn up.
    ValidSampleRejectedBySpec !Text
  | -- | The specification accepts a sample, yet the structural walk claims it
    -- breaks a derived constraint. Both cannot be right, and the walk is the
    -- one to doubt: this is a defect in the derivation.
    DerivationContradicted !Text !Obligation
  | -- | An entry of the incorrect specification list names an obligation this
    -- rule does not state. Either the specification was fixed and the entry
    -- outlived it, or the entry never matched: both are worth stopping for,
    -- which is what keeps the list from gathering claims nobody checks.
    SpecDefectUnmatched !Text !Text

-- | Coverage with nothing declared about the specification, which is what the
-- tests and anything measuring one rule in isolation want.
coverageOf :: CTreeRoot MonoReferenced -> Name -> [Obligation] -> [Sample] -> Report
coverageOf = coverageGiven []

coverageGiven :: [(Waiver, SpecDefect)] -> CTreeRoot MonoReferenced -> Name -> [Obligation] -> [Sample] -> Report
coverageGiven defects root rule obligations samples =
  Report
    { reportRule' = rule,
      reportJudged = judged,
      reportCoverage = map classify obligations,
      reportFindings = misplaced <> contradictions <> unmatched
    }
  where
    judged = map (judge root rule) samples
    withTerm validity =
      [ (judgedName entry, judgedSource entry, term)
      | entry <- judged,
        judgedValidity entry == validity,
        Just term <- [judgedTerm entry]
      ]
    accepted = withTerm SpecValid
    rejected = withTerm SpecInvalid
    -- The specification's verdict decides which samples can fulfil what, not
    -- the directory. A zap-0 sample sits under `invalid` and is specification
    -- valid, so it can no more break a derived constraint than a `valid` one.
    candidates obligation = case obligationCategory (obligationKind obligation) of
      Accept -> accepted
      Reject -> rejected
    -- What a custom validator turns out to enforce, found by experiment.
    --
    -- The tree cannot express uniqueness, so the derivation records only that
    -- some validator is there. Repeating an entry of a sample the rule accepts
    -- and asking the rule again settles what it is for: a rule that now refuses
    -- the sample enforces uniqueness, and a sample carrying a repeat is what
    -- fulfils the obligation. A rule of the same shape with no validator
    -- accepts both and nothing is claimed, which is what tells @nonempty_set@
    -- from @nonempty_list@.
    --
    -- One accepted sample is enough: the answer is a property of the rule, not
    -- of the sample, and running it once keeps the probe off the hot path.
    enforcesUniqueness = any refusesARepeat (take 1 [term | (_, _, term) <- accepted])

    refusesARepeat term = case withRepeatedEntry term of
      Nothing -> False
      Just repeated -> validAs root rule term && not (validAs root rule repeated)

    -- An obligation the specification should not be stating is settled before
    -- any sample is looked at: there is nothing for a corpus to show.
    declaredWrong obligation =
      viaNonEmpty head [(kind, defectReason defect) | (kind, defect) <- defects, matches defect obligation]
    unmatched =
      [ SpecDefectUnmatched (defectPath defect) (defectDemand defect)
      | (_, defect) <- defects,
        not (any (matches defect) obligations)
      ]
    matches defect obligation =
      defectPath defect == renderPath (obligationPath obligation)
        && defectDemand defect == renderKind (obligationKind obligation)

    classify obligation =
      let verdicts =
            [ ((name, source), satisfies root obligation term)
            | (name, source, term) <- candidates obligation
            ]
          -- One witness is the evidence, and asking for more is what made this
          -- expensive: counting them walked every sample of every fulfilled
          -- obligation, when the first hit already settles it. The list is
          -- lazy, so taking one leaves the rest of the scan undone.
          hits = take 1 [name | ((name, source), Just True) <- verdicts, notWaived source]
          -- Looked at only when nothing enforced holds: a rule the decoder does
          -- enforce is covered outright, and the waiver is what is left when it
          -- does not.
          waivers = take 1 [(name, why) | ((name, FromWaived why), Just True) <- verdicts]
          decidable = any (isJust . snd) verdicts
          status
            | Just (kind, why) <- declaredWrong obligation = Waived kind why
            | not (null hits) = Fulfilled
            | ((_, why) : _) <- waivers = Waived VerifiedByParent why
            -- Describable only once the probe has said what the validator is
            -- for. Until then there is no constraint to hold a sample to.
            | ValidatorFails <- obligationKind obligation,
              not enforcesUniqueness =
                Untested NoDescription
            | null verdicts = Untested NoSamples
            | decidable = Outstanding
            | otherwise = Untested NoPath
       in Coverage obligation status (hits <> map fst waivers)
    misplaced =
      [ ValidSampleRejectedBySpec (judgedName entry)
      | entry <- judged,
        judgedSource entry == FromValid,
        judgedValidity entry == SpecInvalid
      ]
    contradictions =
      [ DerivationContradicted name obligation
      | obligation <- obligations,
        Reject <- [obligationCategory (obligationKind obligation)],
        isNothing (declaredWrong obligation),
        (name, _, term) <- accepted,
        satisfies root obligation term == Just True
      ]

-- SAMPLES

-- | Which directory a sample came from. This is where the corpus put it, not
-- what the specification thinks of it, and the two differ on purpose:
-- @invalid\/zap-0@ holds samples the specification accepts and the ledger
-- decoder rejects.
data Source
  = FromValid
  | FromInvalid
  | -- | From the waived category: the specification refuses these bytes and the
    -- decoder takes them, with the recorded reason carried along so a verdict
    -- can state it.
    FromWaived !Text
  deriving (Eq, Show)

-- | How a waiver reads on a report line.
waiverName :: Waiver -> Text
waiverName = \case
  VerifiedByParent -> "verification by parent decoder"
  VerifiedByLedger -> "verification by the ledger"
  IncorrectSpecification -> "incorrect specification"

-- | Whether a sample stands on the decoder refusing it rather than on a waiver.
notWaived :: Source -> Bool
notWaived = \case
  FromWaived _ -> False
  _ -> True

-- | What a category directory says about its samples, before the per sample
-- reason a waiver carries is read.
data SampleKind = KindValid | KindInvalid | KindWaived
  deriving (Eq, Show)

data Sample = Sample
  { sampleName :: !Text,
    sampleSource :: !Source,
    sampleBytes :: !BS.ByteString
  }

-- | What the specification makes of a sample.
data Validity
  = SpecValid
  | SpecInvalid
  | -- | The bytes never reached the rule: not CBOR, trailing bytes, or no such
    -- rule. Such a sample is rejected by everything and so fulfils no
    -- particular obligation.
    Undecodable !Text
  deriving (Eq, Show)

data Judged = Judged
  { judgedName :: !Text,
    judgedSource :: !Source,
    judgedValidity :: !Validity,
    judgedTerm :: !(Maybe Term)
  }

-- | Every sample of one rule.
--
-- A corpus names its categories in one directory level and marks samples with
-- a suffix, so both questions a sample raises are answered by its path: what it
-- must do comes from the directory, and whether it is a sample at all comes
-- from the suffix. Reference encodings are skipped: those are re-encodings this
-- project produced, so crediting coverage to them would credit it to our own
-- encoder.
loadSamples :: FilePath -> IO [Sample]
loadSamples ruleDir = do
  present <- doesDirectoryExist ruleDir
  categories <- if present then listDirectoryChecked ruleDir else pure []
  fmap concat $ forM categories $ \category ->
    case sourceOfCategory category of
      Nothing -> pure []
      Just source -> loadCategory source ruleDir category

-- | What a category directory says its samples must do, or 'Nothing' for an
-- entry that holds no samples at all, such as the configuration a corpus
-- carries.
--
-- Hand written samples count for exactly what they would if the generator had
-- produced them: where the bytes came from decides nothing.
sourceOfCategory :: FilePath -> Maybe SampleKind
sourceOfCategory category
  | category == "valid" = Just KindValid
  | category == "manual-valid" = Just KindValid
  | category == "manual-invalid" = Just KindInvalid
  | category == "verification-by-parent" = Just KindWaived
  | "invalid-zap-" `isPrefixOf` category = Just KindInvalid
  | otherwise = Nothing

loadCategory :: SampleKind -> FilePath -> FilePath -> IO [Sample]
loadCategory kind ruleDir category = do
  let directory = ruleDir </> category
  present <- doesDirectoryExist directory
  if not present
    then pure []
    else do
      entries <- listDirectoryChecked directory
      forM [entry | entry <- entries, ".input.cbor" `isSuffixOf` entry] $ \entry -> do
        bytes <- BS.readFile (directory </> entry)
        source <- case kind of
          KindValid -> pure FromValid
          KindInvalid -> pure FromInvalid
          KindWaived -> FromWaived <$> readReason directory entry
        pure
          Sample
            { sampleName = toText (category </> takeFileName entry),
              sampleSource = source,
              sampleBytes = bytes
            }

-- | The written reason beside a waived sample.
--
-- Required rather than optional: a waiver nobody wrote down is a sample that
-- looks like a gap and reads like an oversight, so its absence stops the run
-- the way a missing reference encoding does.
readReason :: FilePath -> FilePath -> IO Text
readReason directory entry = do
  let path = directory </> dropSuffix entry <> ".reason.txt"
  present <- doesFileExist path
  unless present $ die $ "waived sample with no reason: " <> path
  T.strip . decodeUtf8 <$> BS.readFile path
  where
    dropSuffix = reverse . drop (length (".input.cbor" :: String)) . reverse

-- | Validate once per sample and keep the decoded term beside the verdict.
-- Both are needed for every obligation, so doing this up front is what keeps
-- the cost one validation per sample rather than one per (sample, obligation).
judge :: CTreeRoot MonoReferenced -> Name -> Sample -> Judged
judge root rule sample =
  Judged
    { judgedName = sampleName sample,
      judgedSource = sampleSource sample,
      judgedValidity = validity,
      judgedTerm = term
    }
  where
    bytes = sampleBytes sample
    term = case CBOR.deserialiseFromBytes decodeTaggedTerm (LBS.fromStrict bytes) of
      Right (_, decoded) -> Just decoded
      Left _ -> Nothing
    validity = case validateCBOR bytes rule (mapIndex root) of
      Left (DecodingFailed _) -> Undecodable "not decodable as CBOR"
      Left (LeftoverBytes _) -> Undecodable "trailing bytes after a complete value"
      Left (RuleDoesNotExist _) -> Undecodable "the specification has no such rule"
      Right evidenced
        | isValid evidenced -> SpecValid
        | otherwise -> SpecInvalid

-- MATCHING

-- | Does this term satisfy this obligation? For a 'Reject' obligation that
-- means breaking the constraint; for an 'Accept' obligation, exhibiting the
-- permitted form. 'Nothing' is "cannot decide", never "no".
satisfies :: CTreeRoot MonoReferenced -> Obligation -> Term -> Maybe Bool
satisfies specification obligation root = do
  subject <- followPath (obligationSubject obligation) root
  case obligationKind obligation of
    TooFew lower -> (\count -> fromIntegral count < lower) <$> entryCount subject
    TooMany upper -> (\count -> fromIntegral count > upper) <$> entryCount subject
    AcceptArity size -> (== size) <$> entryCount subject
    -- Only where the term still carries a tag. The decoder folds some of them
    -- away, a bignum under tag 2 or 3 arriving as a plain integer, so an
    -- untagged term is not evidence that the tag was absent from the bytes.
    AcceptTag wanted -> case subject of
      TTagged actual _ -> Just (actual == wanted)
      _ -> Nothing
    WrongTag wanted -> case subject of
      TTagged actual _ -> Just (actual /= wanted)
      _ -> Nothing
    -- The subject of a required key is the map, not the entry, so carrying the
    -- key is the opposite of the map lacking it.
    AcceptRequiredKey key -> not <$> mapLacks key subject
    MissingRequiredKey key -> mapLacks key subject
    -- The referenced rule says what a valid value there is; asking it is the
    -- only way to check a site whose rule has no samples of its own.
    -- Reaching the entry is all that is asked: what it holds is folded in and
    -- states its own obligations beneath this one.
    AcceptPresent -> Just True
    AcceptReference name -> Just (validAs specification name subject)
    WrongReference name -> Just (not (validAs specification name subject))
    AcceptType primitive -> matchesPrimitive primitive subject
    WrongType primitive -> not <$> matchesPrimitive primitive subject
    AcceptLiteral value -> matchesLiteral value subject
    WrongLiteral value -> not <$> matchesLiteral value subject
    AcceptDefinite -> not <$> isIndefinite subject
    AcceptIndefinite -> isIndefinite subject
    -- Resolving the path already required the term to fit this alternative, so
    -- reaching here is the proof that the branch was taken.
    AcceptBranch -> Just True
    -- Only answerable where the shapes tell the alternatives apart. A branch
    -- that is a bare reference has no shape of its own, and claiming a sample
    -- matches none of them on that basis would be a gap nothing could fill.
    -- Tagged, but not one of the tags: the near miss a bare integer would not
    -- reach. Untagged is not a violation of this one, it is a different case.
    NoKeyMatches keys -> mapHasForeignKey keys subject
    DuplicateKey -> mapHasDuplicateKey subject
    NoTagMatches tags -> Just (maybe False (`notElem` tags) (termTag subject))
    NoDiscriminantMatches values ->
      Just (maybe False (`notElem` values) (leadingUnsigned subject))
    NoBranchMatches shapes
      | ShapeOther `elem` shapes -> Nothing
      | otherwise -> Just (not (any (`shapeFits` subject) shapes))
    AcceptElements shapes -> Just (elementsFit shapes subject)
    WrongCombination shapes -> Just (elementsFit shapes subject)
    -- Only about arrays of the width the combinations describe. A value of
    -- another width, or no array at all, is refused by the branch shapes
    -- already, and crediting this one for it would hide the near miss it is
    -- here to catch.
    NoElementsMatch combinations
      | Just width <- length <$> viaNonEmpty head combinations,
        Just entries <- arrayEntries subject,
        length entries == width ->
          Just (not (any (`elementsFit` subject) combinations))
      | otherwise -> Nothing
    -- Reaching the node in a sample the validator accepted is the proof, and is
    -- also the seed the probe mutates.
    ValidatorSucceeds -> Just True
    -- Only reached once the probe has shown the validator enforces uniqueness,
    -- so a repeated entry is what breaking it looks like.
    ValidatorFails -> repeatsAnEntry subject
    AcceptSize low high -> within low high <$> termSize subject
    ViolateSizeBelow low -> (< low) <$> termSize subject
    ViolateSizeAbove high -> (> high) <$> termSize subject
    ViolateLowBound low -> (< low) <$> termInteger subject
    ViolateHighBound high -> (> high) <$> termInteger subject
    AcceptBoundary value -> (== value) <$> termInteger subject
    InsideRange low high -> (\value -> value > low && value < high) <$> termInteger subject
    BelowRange low -> (< low) <$> termInteger subject
    AboveRange high -> (> high) <$> termInteger subject
    AcceptCbor -> decodesAsCbor subject
    ViolateCbor -> not <$> decodesAsCbor subject
    _ -> Nothing
  where
    within low high size = size >= low && size <= high

    elementsFit shapes term = case arrayEntries term of
      Just entries | length entries == length shapes -> and (zipWith shapeFits shapes entries)
      _ -> False

-- | Whether a sub-term is a valid value of the rule named at this site.
--
-- cuddle exposes no way to run a rule against a term, so the term goes back
-- through the public entry point: encode it and validate those bytes. The
-- re-encoding may choose a different length form from the original, which is
-- harmless for a reference check and would not be for one about the form.
validAs :: CTreeRoot MonoReferenced -> Name -> Term -> Bool
validAs specification name term =
  case validateCBOR (CBOR.toStrictByteString (encodeTerm term)) name (mapIndex specification) of
    Right evidenced -> isValid evidenced
    Left _ -> False

-- | The length a @.size@ control measures: bytes of a byte string, characters
-- of a text string. A number has no length, and @uint .size n@ is a ceiling on
-- the value rather than a length, so it is measured by 'termInteger' instead.
termSize :: Term -> Maybe Integer
termSize = \case
  TBytes bytes -> Just (fromIntegral (BS.length bytes))
  TBytesI bytes -> Just (fromIntegral (LBS.length bytes))
  TString text -> Just (fromIntegral (T.length text))
  TStringI text -> Just (fromIntegral (TL.length text))
  _ -> Nothing

-- | Whether a byte string's contents are themselves CBOR, which is what
-- @.cbor@ asks. Only decodability is checked here; whether the contents match
-- the named type would need that rule, which the path does not carry.
decodesAsCbor :: Term -> Maybe Bool
decodesAsCbor = \case
  TBytes bytes -> Just (decodes (LBS.fromStrict bytes))
  TBytesI bytes -> Just (decodes bytes)
  _ -> Nothing
  where
    decodes lazy = case CBOR.deserialiseFromBytes decodeTerm lazy of
      Right (rest, _) -> LBS.null rest
      Left _ -> False

isUntested :: Status -> Bool
isUntested = \case
  Untested _ -> True
  _ -> False

outstandingStatus :: Status -> Bool
outstandingStatus = (== Outstanding)

-- | What would change an untested answer, in the fewest words that distinguish
-- the two piles: one is fixed by generating samples, the other is not.
renderCause :: Cause -> Text
renderCause = \case
  NoSamples -> "no samples"
  NoPath -> "no path"
  NoDescription -> "opaque validator"

-- RENDERING

renderReport :: Detail -> Report -> [Text]
renderReport detail report =
  [ unName (reportRule' report) <> ": " <> counts,
    "  samples: " <> sampleSummary
  ]
    <> delegated
    <> map coverageLine shown
    <> findingLines
  where
    entries = reportCoverage report
    -- A rule that is nothing but a choice between references states no
    -- constraint of its own. That is not a gap: the referenced rules state
    -- them, and each is enumerated in its own right.
    delegated
      | null entries = ["  no constraints of its own; the rules it references state them"]
      | otherwise = []
    shown = case detail of
      Everything -> sortOn (rank . coverageStatus) entries
      OutstandingOnly -> filter (outstandingStatus . coverageStatus) entries
    counts =
      show (countWhere (== Fulfilled))
        <> " fulfilled, "
        <> show (countWhere (== Outstanding))
        <> " outstanding, "
        <> show (countWhere isUntested)
        <> " untested, of "
        <> show (length entries)
    countWhere predicate = length (filter (predicate . coverageStatus) entries)
    judged = reportJudged report
    judgedWhere predicate = show (length (filter predicate judged))
    sampleSummary =
      judgedWhere (\entry -> judgedValidity entry == SpecValid)
        <> " specification-valid, "
        <> judgedWhere (\entry -> judgedValidity entry == SpecInvalid)
        <> " specification-invalid, "
        <> judgedWhere (\entry -> case judgedValidity entry of Undecodable _ -> True; _ -> False)
        <> " undecodable, of "
        <> show (length judged)
    rank = \case
      Outstanding -> (0 :: Int)
      Untested _ -> 1
      Waived _ _ -> 2
      Fulfilled -> 3
    coverageLine entry =
      "  "
        <> T.justifyLeft 13 ' ' (label (coverageStatus entry))
        <> renderObligation (coverageObligation entry)
        <> witnesses (coverageWitnesses entry)
        <> because (coverageStatus entry)
    label = \case
      Fulfilled -> "fulfilled"
      Outstanding -> "OUTSTANDING"
      Waived kind _ -> waiverName kind
      Untested _ -> "untested"
    -- Only on an untested line, where what would change the answer is not
    -- otherwise readable from the row.
    because = \case
      Untested cause -> "   (" <> renderCause cause <> ")"
      _ -> ""
    witnesses = \case
      [] -> ""
      names -> "   e.g. " <> fromMaybe "" (viaNonEmpty head names)
    findingLines = case reportFindings report of
      [] -> []
      found -> "  findings:" : map (("    " <>) . renderFinding) found
    renderFinding = \case
      ValidSampleRejectedBySpec name ->
        name <> ": the specification rejects a sample the corpus calls valid"
      DerivationContradicted name obligation ->
        name
          <> ": the specification accepts this, but the derivation claims it breaks "
          <> renderObligation obligation
      SpecDefectUnmatched path demand ->
        toText corpusConfigName
          <> " waives an obligation this rule does not state: "
          <> path
          <> " "
          <> demand

-- | The per-rule counts, worst first, and the totals underneath.
--
-- Only rules with a corpus directory appear here. The specification has many
-- more, and what becomes of theirs is the next section's business.
renderSummary :: CTreeRoot MonoReferenced -> FilePath -> [Report] -> [Text]
renderSummary (CTreeRoot rules) corpusDir reports =
  [ "summary for " <> toText corpusDir,
    "",
    "  corpus rules: "
      <> show (length reports)
      <> " of "
      <> show (Map.size rules)
      <> " specification rules have a directory",
    "",
    heading
  ]
    <> map row (sortOn (negate . outstandingOf) reports)
    <> [ T.replicate (T.length heading) "-",
         columns
           blank
           "total"
           (show (sum (map fulfilledOf reports)))
           (show (sum (map outstandingCount reports)))
           (show (sum (map untestedOf reports)))
           (show (sum (map totalOf reports)))
       ]
  where
    heading = columns blank "rule" "fulfilled" "outstanding" "untested" "total"
    row report =
      columns
        (mark report)
        (unName (reportRule' report))
        (show (fulfilledOf report))
        (show (outstandingCount report))
        (show (untestedOf report))
        (show (totalOf report))
    -- Two spaces where an emoji would be, so the columns line up either way.
    blank = "  "
    -- A rule passes only when nothing is left over. An untested obligation is
    -- not a gap, but it is not coverage either, so it holds the mark back.
    -- A rule that states nothing of its own is neither: it delegates every
    -- constraint to the rules it references.
    mark report
      | totalOf report == 0 = "\10067"
      | outstandingOf report == 0 = "\9989"
      | otherwise = "\10060"
    columns markText name fulfilled outstanding untested total =
      "  "
        <> markText
        <> " "
        <> T.justifyLeft 30 ' ' name
        <> T.justifyRight 11 ' ' fulfilled
        <> T.justifyRight 12 ' ' outstanding
        <> T.justifyRight 9 ' ' untested
        <> T.justifyRight 8 ' ' total
    countWhere predicate report = length (filter (predicate . coverageStatus) (reportCoverage report))
    fulfilledOf = countWhere (== Fulfilled)
    outstandingCount = countWhere (== Outstanding)
    untestedOf = countWhere isUntested
    totalOf = length . reportCoverage
    outstandingOf report = outstandingCount report + untestedOf report

-- | The obligations no rule directory can speak to.
--
-- A rule with no directory has no samples, so its obligations are not covered,
-- not uncovered, but unexamined, and leaving them out would make the corpus
-- look far more complete than it is. Whether a rule is reachable from a corpus
-- root decides what to do about it: a reachable one has its values buried
-- inside other rules' samples and needs navigation to get at, while an
-- unreachable one needs a corpus root of its own, or a reason for not having
-- one.
-- | How many rules the specification defines, which is the denominator every
-- corpus count is read against.
specificationRuleCount :: CTreeRoot MonoReferenced -> Int
specificationRuleCount (CTreeRoot rules) = Map.size rules

-- | How many rules state obligations nothing measures, and how many that is.
--
-- The same count the printed section opens with, so a page and a terminal run
-- cannot disagree about how much of the specification is out of reach.
unexaminedCount :: CTreeRoot MonoReferenced -> [Text] -> (Int, Int)
unexaminedCount root@(CTreeRoot rules) corpusRules = (length unexamined, sum (map snd unexamined))
  where
    corpus = Set.fromList (map Name corpusRules)
    unexamined =
      [ (name, stated)
      | name <- Map.keys rules,
        not (Set.member name corpus),
        let stated = length (obligationsFor root name),
        stated > 0
      ]

-- | Every rule the specification states obligations for: its name, how many it
-- states, and whether any corpus root reaches it.
--
-- The specification is the denominator. A rule with no corpus directory is not
-- absent from the dataset, it is measured through whatever contains it, and
-- saying so is the difference between a page that reports a gap and one that
-- reports an unknown.
specificationRules :: CTreeRoot MonoReferenced -> Reachability -> [(Text, Int, Bool)]
specificationRules root@(CTreeRoot rules) reachability =
  [ (unName name, stated, Set.member name reached)
  | (name, definition) <- Map.toList rules,
    name /= syntheticRoot,
    not (splicesEntries definition),
    -- Folded into the field that carries it, so it is no more a rule of its own
    -- than a group is, and a row here would ask for samples nothing decodes.
    not (foldedRule name),
    let stated = length (obligationsFor root name),
    stated > 0
  ]
  where
    reached = fromRoots reachability
    -- Not a rule of the specification at all: cuddle emits it to anchor the
    -- roots it was built from, so its obligations are about the anchor rather
    -- than about anything a decoder will ever see.
    syntheticRoot = Name "huddle_root_defs"
    -- A group has no term of its own: it splices its entries into whatever
    -- contains it, and its obligations are already counted there. The generator
    -- refuses one as a root for the same reason, so a page that listed them
    -- would be counting the same constraints twice and asking for samples that
    -- cannot exist.
    splicesEntries = \case
      CTree.Group _ -> True
      _ -> False

-- | The rules nothing in the corpus touches, directly or otherwise, and the
-- obligations they state.
--
-- A rule with no directory of its own may still have its values buried inside
-- another rule's samples, which is coverage of a kind even though nothing
-- measures its obligations one by one. A rule no corpus root reaches has not
-- been exercised at all, and that is the number worth putting at the top of a
-- page.
unreachedCount :: CTreeRoot MonoReferenced -> Reachability -> [Text] -> (Int, Int)
unreachedCount root@(CTreeRoot rules) reachability corpusRules =
  (length unreached, sum (map snd unreached))
  where
    corpus = Set.fromList (map Name corpusRules)
    reached = fromRoots reachability
    unreached =
      [ (name, stated)
      | name <- Map.keys rules,
        not (Set.member name corpus),
        not (Set.member name reached),
        let stated = length (obligationsFor root name),
        stated > 0
      ]

renderUnexamined :: CTreeRoot MonoReferenced -> Reachability -> [Text] -> [Text]
renderUnexamined root@(CTreeRoot rules) reachability corpusRules =
  [ "specification rules with no corpus directory: "
      <> show (length unexamined)
      <> " rules stating "
      <> show (count unexamined)
      <> " obligations that are never examined",
    "",
    "  reached only inside other rules  "
      <> justify (count reached)
      <> " obligations on "
      <> justify (length reached)
      <> " rules",
    "  unreachable from any corpus root "
      <> justify (count unreached)
      <> " obligations on "
      <> justify (length unreached)
      <> " rules"
  ]
    <> section "unreachable" unreached
    <> section "largest" (take 10 (sortOn (negate . length . snd) reached))
  where
    corpus = Set.fromList (map Name corpusRules)
    unexamined =
      [ (name, obligations)
      | name <- Map.keys rules,
        not (Set.member name corpus),
        let obligations = obligationsFor root name,
        not (null obligations)
      ]
    (reached, unreached) =
      partition (\(name, _) -> Set.member name (fromRoots reachability)) unexamined
    count = sum . map (length . snd)
    justify = T.justifyRight 5 ' ' . show
    section _ [] = []
    section title entries = "" : ("  " <> title <> ":") : map row entries
    row (name, obligations) = "    " <> justify (length obligations) <> "  " <> unName name
