module Coverage.Report.TermChecker where

import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Custom.Core (RuleTerm (..))
import Codec.CBOR.Cuddle.CDDL.Custom.Validator (CustomValidatorResult (..), runValidator)
import Codec.CBOR.Cuddle.IndexMappable (mapIndex)
import Coverage.Obligations.Cbor
import Coverage.Obligations.CborTerm
import Coverage.Obligations.Obligation
import Coverage.Obligations.Obligations (ruleObligations)
import Coverage.Obligations.Shape
import Coverage.Obligations.SpecRule (HuddleSpec)
import Coverage.Obligations.Trail (stepsBelowPrefix)
import Data.ByteString qualified as BS
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Generation.SampleType (SampleCategory (..))

-- | What checking a term needs: the specification, and the obligations of each rule.
data TermChecker = TermChecker
  { checkerSpecification :: HuddleSpec,
    -- | The obligations of a rule are derived the first time that a reference to the rule is checked, then kept.
    checkerObligations :: Map Name [Obligation]
  }

-- | A term checker for this specification.
newTermChecker :: HuddleSpec -> TermChecker
newTermChecker specification@(CTreeRoot rules) =
  TermChecker specification (LazyMap.map (ruleObligations specification) rules)

-- | The obligations of a rule, none for a name that is not a rule.
obligationsOf :: TermChecker -> Name -> [Obligation]
obligationsOf checker name = Map.findWithDefault [] name (checkerObligations checker)

-- | Return True if the term meets all obligations of the rule: it meets none of the obligations to reject.
meetsObligations :: TermChecker -> Name -> Cbor -> Bool
meetsObligations checker name term = not (any breaks obligations)
  where
    obligations = obligationsOf checker name
    breaks obligation =
      sampleCategoryFor obligation == Reject && answerFor checker obligations obligation term == Just True

-- | Whether a sample meets an obligation.
--   If the obligation is about a validator failing, the sample meets it only if it fails as a whole and not due to any entry alone.
--   Otherwise check if the sample term satisfies the obligation.
answerFor :: TermChecker -> [Obligation] -> Obligation -> Cbor -> Maybe Bool
answerFor checker obligations obligation term = case obligationKind obligation of
  ValidatorFails -> (&& not breaksSomethingElse) <$> failsOnlyAsAWhole (checkerSpecification checker) obligation term
  _ -> satisfies checker obligation term
  where
    breaksSomethingElse =
      or
        [ satisfies checker other term == Just True
        | other <- obligations,
          other /= obligation,
          sampleCategoryFor other == Reject,
          isJust (stepsBelowPrefix (obligationPath obligation) (obligationPath other))
        ]

-- | Does this term satisfy this obligation?
--
--  For a 'Reject' obligation that means breaking the constraint.
--  For an 'Accept' obligation, exhibiting the permitted form.
--  Return Nothing if the answer is "cannot decide".
satisfies :: TermChecker -> Obligation -> Cbor -> Maybe Bool
satisfies checker obligation root =
  atAnyOccurrence (satisfiedAt checker obligation) (followPath (obligationSubject obligation) root)

-- | The answer for a path that reaches several occurrences of a repeated entry: met when one occurrence meets the
-- obligation, unmet when one occurrence does not and none does, and undecided otherwise.
atAnyOccurrence :: (Cbor -> Maybe Bool) -> [Cbor] -> Maybe Bool
atAnyOccurrence answer subjects
  | Just True `elem` answers = Just True
  | Just False `elem` answers = Just False
  | otherwise = Nothing
  where
    answers = map answer subjects

-- | Whether the value that the path of the obligation reaches satisfies the obligation.
satisfiedAt :: TermChecker -> Obligation -> Cbor -> Maybe Bool
satisfiedAt checker obligation subject =
  case obligationKind obligation of
    TooFew lower -> (\count -> fromIntegral count < lower) <$> entryCount subject
    TooMany upper -> (\count -> fromIntegral count > upper) <$> entryCount subject
    AcceptArity size -> (== size) <$> entryCount subject
    -- Only where the term still carries a tag. The decoder folds some of them
    -- away, a bignum under tag 2 or 3 arriving as a plain integer, so an
    -- untagged term is not evidence that the tag was absent from the bytes.
    AcceptTag wanted -> isTagged subject wanted
    WrongTag wanted -> isNotTagged subject wanted
    MissingTag _ -> Just (isNothing (termTag subject))
    -- The subject of a required key is the map, not the entry, so carrying the
    -- key is the opposite of the map lacking it.
    AcceptRequiredKey key -> not <$> mapLacks key subject
    MissingRequiredKey key -> mapLacks key subject
    -- The referenced rule says what a valid value there is; asking it is the
    -- only way to check a site whose rule has no samples of its own.
    -- Reaching the entry is all that is asked: what it holds is folded in and
    -- states its own obligations beneath this one.
    AcceptPresent -> Just True
    AcceptReference name -> Just (meetsObligations checker name subject)
    WrongReference name -> Just (not (meetsObligations checker name subject))
    AcceptType primitive -> matchesPrimitive primitive subject
    WrongType primitive -> not <$> matchesPrimitive primitive subject
    AcceptLiteral value -> matchesLiteral value subject
    WrongLiteral value -> not <$> matchesLiteral value subject
    AcceptDefinite -> not <$> isIndefinite subject
    AcceptIndefinite -> isIndefinite subject
    AcceptWideHead -> hasWideHead subject
    AcceptWidestHead -> hasWidestHead subject
    -- Resolving the path already required the term to fit this alternative, so
    -- reaching here is the proof that the branch was taken.
    AcceptBranch -> Just True
    -- Only answerable where the shapes tell the alternatives apart. A branch
    -- that is a bare reference has no shape of its own, and claiming a sample
    -- matches none of them on that basis would be a gap nothing could fill.
    -- Tagged, but not one of the tags: the specific error a bare integer would not
    -- reach. Untagged is not a violation of this one, it is a different case.
    NoKeyMatches keys -> mapHasForeignKey keys subject
    DuplicateKey -> mapHasDuplicateKey subject
    NoTagMatches tags -> Just (maybe False (`notElem` tags) (termTag subject))
    InsideTagGap low high -> Just (maybe False (\tag -> tag > low && tag < high) (termTag subject))
    NoReferenceMatches shape names -> Just (shapeFits shape subject && not (any (\name -> meetsObligations checker name subject) names))
    NoDiscriminantMatches values ->
      Just (maybe False (`notElem` values) (leadingUnsigned subject))
    NoBranchMatches shapes
      | ShapeOther `elem` shapes -> Nothing
      | otherwise -> Just (not (any (`shapeFits` subject) shapes))
    AcceptElements shapes -> Just (elementsFit shapes subject)
    WrongCombination shapes -> Just (elementsFit shapes subject)
    -- Only about arrays of the width the combinations describe. A value of
    -- another width, or no array at all, is refused by the branch shapes
    -- already, and crediting this one for it would hide the specific error it is
    -- here to catch.
    NoElementsMatch combinations
      | Just width <- length <$> viaNonEmpty head combinations,
        Just entries <- arrayEntries subject,
        length entries == width ->
          Just (not (any (`elementsFit` subject) combinations))
      | otherwise -> Nothing
    -- The custom validator itself is run on the value, so the check holds for
    -- any validator, whatever it checks.
    ValidatorSucceeds -> passesValidator (checkerSpecification checker) obligation subject
    ValidatorFails -> not <$> passesValidator (checkerSpecification checker) obligation subject
    AcceptSize low high -> within low high <$> termSize subject
    ViolateSizeBelow low -> (< low) <$> termSize subject
    ViolateSizeAbove high -> (> high) <$> termSize subject
    ViolateHighBound high -> (> high) <$> termInteger subject
    AcceptBoundary value -> (== value) <$> termInteger subject
    InsideRange low high -> (\value -> value > low && value < high) <$> termInteger subject
    InsideGap low high -> (\value -> value > low && value < high) <$> termInteger subject
    BelowRange low -> (< low) <$> termInteger subject
    AboveRange high -> (> high) <$> termInteger subject
    AcceptCbor -> isJust <$> embeddedContents subject
    ViolateCbor -> isNothing <$> embeddedContents subject
    TrailingAfterCbor -> (\bytes -> either (const False) ((< BS.length bytes) . snd) (readCborPrefix bytes)) <$> embeddedBytes subject
    WrongEmbedded name -> maybe False (not . meetsObligations checker name) <$> embeddedContents subject

-- | Check if the elements of an array term fit the given shapes.
-- Return 'False' if the term is not an array or the lengths do not match.
elementsFit :: [Shape] -> Cbor -> Bool
elementsFit shapes term = case arrayEntries term of
  Just entries | length entries == length shapes -> and (zipWith shapeFits shapes entries)
  _ -> False

-- | The length a @.size@ control measures: the bytes of a byte string, and the bytes of the UTF-8 encoding of a text
-- string, not its characters. A number has no length, and @uint .size n@ is a ceiling on the value rather than a
-- length, so it is measured by 'termInteger' instead.
termSize :: Cbor -> Maybe Integer
termSize = \case
  CBytes _ bytes -> Just (fromIntegral (BS.length bytes))
  CText _ text -> Just (fromIntegral (BS.length (TE.encodeUtf8 text)))
  _ -> Nothing

-- | The bytes of a byte string, and 'Nothing' for a term that is not one.
embeddedBytes :: Cbor -> Maybe BS.ByteString
embeddedBytes = \case
  CBytes _ bytes -> Just bytes
  _ -> Nothing

-- | The value that a byte string holds, which is what @.cbor@ asks for: 'Just' 'Nothing' when its contents do not
-- decode as one CBOR value, and 'Nothing' for a term that is not a byte string.
embeddedContents :: Cbor -> Maybe (Maybe Cbor)
embeddedContents = \case
  CBytes _ bytes -> Just (rightToMaybe (readCbor bytes))
  _ -> Nothing

-- | Check if a size is within the given bounds (inclusive).
within :: Integer -> Integer -> Integer -> Bool
within low high size = size >= low && size <= high

isTagged :: Cbor -> Word64 -> Maybe Bool
isTagged subject wanted = case subject of
  CTag _ actual _ -> Just (actual == wanted)
  _ -> Nothing

isNotTagged :: Cbor -> Word64 -> Maybe Bool
isNotTagged subject wanted = case subject of
  CTag _ actual _ -> Just (actual /= wanted)
  _ -> Nothing

-- | Whether the custom validator of the obligation fails on the value at its subject because of a rule about the value
-- as a whole, or 'Nothing' when the value or the validator is missing.
--
-- A validator also checks the entries under it, so it fails on a set with one malformed entry too. Each entry alone
-- must pass it: an entry alone cannot break a rule about how entries relate, such as uniqueness, so a failure there
-- comes from the entry itself.
failsOnlyAsAWhole :: HuddleSpec -> Obligation -> Cbor -> Maybe Bool
failsOnlyAsAWhole specification obligation root =
  atAnyOccurrence failsAt (followPath (obligationSubject obligation) root)
  where
    failsAt subject = do
      passes <- passesValidator specification obligation subject
      pure (not passes && all ((== Just True) . passesValidator specification obligation) (eachEntryAlone subject))

-- | Whether a value passes the custom validator that the obligation carries, or 'Nothing' when it carries none.
passesValidator :: HuddleSpec -> Obligation -> Cbor -> Maybe Bool
passesValidator specification obligation term = do
  CustomValidator validator <- obligationValidator obligation
  pure $ case runValidator (validator (SingleTerm (toTerm term))) (mapIndex specification) of
    CustomValidatorSuccess -> True
    CustomValidatorFailure _ -> False

-- | The same container with only one of its entries, for each entry. Empty for a value that is not a container.
eachEntryAlone :: Cbor -> [Cbor]
eachEntryAlone = \case
  CArray len entries -> [CArray len [entry] | entry <- entries]
  CMap len pairs -> [CMap len [pair] | pair <- pairs]
  CTag width tag inner -> CTag width tag <$> eachEntryAlone inner
  _ -> []
