{-# LANGUAGE AllowAmbiguousTypes #-}

module Generation.Rule where

import Cardano.Ledger.Binary (EncCBOR, serialize')
import Cardano.Ledger.Binary.Decoding
import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Generation.Normalize (normalizeBytes)
import Generation.VerificationFailure
import Text.Show qualified

-- | What a rule is called: the name the specification gives it, the name of its directory in a corpus, and the name a
-- report or a waiver refers to it by.
--
-- Named rather than left as text because a rule name is combined with path segments, sample names and era names all
-- over this project, and nothing but the type tells them apart.
newtype RuleName = RuleName {unRuleName :: Text}
  deriving (Eq, Ord, Show, IsString)

-- | A rule that the generator produces samples for: its name, and how the Haskell node decodes it and encodes it.
data GenerationRule = GenerationRule
  { ruleName :: RuleName,
    deserializeRule :: BS.ByteString -> Either Text (),
    -- | The reference of a sample this rule accepts: what a conforming implementation must produce when it decodes the
    -- sample and encodes it again. Generation writes it beside the sample and verification compares it with that file,
    -- so the two cannot disagree on how it is made.
    reserializeRule :: BS.ByteString -> Either VerificationFailure BS.ByteString
  }

instance Eq GenerationRule where
  (==) = (==) `on` ruleName

instance Ord GenerationRule where
  compare = comparing ruleName

instance Show GenerationRule where
  show = toString . ruleNameText

ruleNameText :: GenerationRule -> Text
ruleNameText = unRuleName . ruleName

-- | A rule whose re-encoding is normalized, so encoding choices that do not change the value do not count.
mkRule :: forall a. (DecCBOR a, EncCBOR a) => Version -> RuleName -> GenerationRule
mkRule version name = makeRule name (decodeAs @a version) (serialize' version) normalized

-- | A rule whose re-encoding must give back the input bytes: the Haskell node keeps the bytes it decoded, because a
-- hash or a signature is taken over them.
mkByteExactRule :: forall a. (DecCBOR a, EncCBOR a) => Version -> RuleName -> GenerationRule
mkByteExactRule version name = makeRule name (decodeAs @a version) (serialize' version) exact

-- | A rule whose type is read through an annotator.
--
-- A header keeps the bytes it decoded so its hash can be taken later, and the ledger expresses that with @DecCBOR
-- (Annotator a)@ rather than a plain instance
mkAnnotatorRule :: forall a. (DecCBOR (Annotator a), EncCBOR a) => Version -> RuleName -> GenerationRule
mkAnnotatorRule version name = makeRule name (decodeThroughAnnotator @a version name) (serialize' version) normalized

-- | A rule read through an annotator whose re-encoding must give back the input bytes.
mkByteExactAnnotatorRule :: forall a. (DecCBOR (Annotator a), EncCBOR a) => Version -> RuleName -> GenerationRule
mkByteExactAnnotatorRule version name = makeRule name (decodeThroughAnnotator @a version name) (serialize' version) exact

-- | Turns the input bytes and their re-encoding into the reference.
type Reference = BS.ByteString -> BS.ByteString -> Either VerificationFailure BS.ByteString

-- | The normal form of the re-encoding.
normalized :: Reference
normalized _ reencoded = first (VerificationFailure EncodeFailed) $ normalizeBytes reencoded

-- | The re-encoding itself, which must be the input bytes.
exact :: Reference
exact input reencoded
  | reencoded == input = Right reencoded
  | otherwise = Left (VerificationFailure ByteExactMismatch "the re-encoding differs from the input bytes")

makeRule :: RuleName -> (BS.ByteString -> Either Text a) -> (a -> BS.ByteString) -> Reference -> GenerationRule
makeRule name decode encode reference =
  GenerationRule
    { ruleName = name,
      deserializeRule = void . decode,
      reserializeRule = \input -> do
        value <- first (VerificationFailure DecodeFailed) $ decode input
        reference input (encode value)
    }

-- | Decode a sample with a rule, and return whether the rule accepts it.
--
-- A ledger decoder can raise an exception on input that it does not expect, such as the Conway block decoder on an
-- unsorted list of invalid transactions. The node then refuses the block, so an exception is a refusal too.
deserializeSample :: GenerationRule -> BS.ByteString -> IO (Either Text ())
deserializeSample rule = catchDecoderException id . deserializeRule rule

-- | The reference of a sample, as 'reserializeRule' gives it, with an exception of the decoder as a failure to decode.
reserializeSample :: GenerationRule -> BS.ByteString -> IO (Either VerificationFailure BS.ByteString)
reserializeSample rule = catchDecoderException (VerificationFailure DecodeFailed) . reserializeRule rule

-- | Evaluate the result of a decoder, and turn an exception that it raises into a failure.
catchDecoderException :: (Text -> e) -> Either e a -> IO (Either e a)
catchDecoderException failure result =
  Exception.try (Exception.evaluate result >>= traverse Exception.evaluate) >>= \case
    Right evaluated -> pure evaluated
    Left exception
      | Just (_ :: Exception.SomeAsyncException) <- Exception.fromException exception -> Exception.throwIO exception
      | otherwise -> pure (Left (failure ("the decoder raised an exception: " <> show exception)))

decodeAs :: forall a. (DecCBOR a) => Version -> BS.ByteString -> Either Text a
decodeAs version bytes = first (T.pack . show) $ decodeFull' version bytes

decodeThroughAnnotator :: forall a. (DecCBOR (Annotator a)) => Version -> RuleName -> BS.ByteString -> Either Text a
decodeThroughAnnotator version name bytes =
  first show $ decodeFullAnnotator version (unRuleName name) decCBOR (BS.fromStrict bytes)
