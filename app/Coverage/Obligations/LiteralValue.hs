module Coverage.Obligations.LiteralValue where

import Codec.CBOR.Cuddle.CDDL (ValueVariant (..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Char (digitToInt, isDigit, isHexDigit)
import Text.ParserCombinators.ReadP hiding (many, optional)

-- | A literal value the specification names.
--
-- Kept as a value rather than as its rendering so it can be compared against a decoded term exactly: the uint @13@ and
-- the text @\"13\"@ are different literals and must not answer for each other.
data LiteralValue
  = LitUInt Word64
  | LitNInt Word64
  | LitBignum Integer
  | LitText Text
  | LitBytes ByteString
  | LitBool Bool
  | LitFloat Double
  deriving (Eq, Show, Ord)

-- | The same tagged form as the generic instances of the other kinds, written out because JSON has no byte string:
-- the bytes of 'LitBytes' are a list of numbers.
instance ToJSON LiteralValue where
  toJSON = \case
    LitUInt number -> tagged "LitUInt" number
    LitNInt number -> tagged "LitNInt" number
    LitBignum number -> tagged "LitBignum" number
    LitText text -> tagged "LitText" text
    LitBytes bytes -> tagged "LitBytes" (BS.unpack bytes)
    LitBool value -> tagged "LitBool" value
    LitFloat number -> tagged "LitFloat" number
    where
      tagged :: (ToJSON a) => Text -> a -> Value
      tagged tag contents = object ["tag" .= tag, "contents" .= contents]

instance FromJSON LiteralValue where
  parseJSON = withObject "literal value" $ \o ->
    (o .: "tag" :: Parser Text) >>= \case
      "LitUInt" -> LitUInt <$> o .: "contents"
      "LitNInt" -> LitNInt <$> o .: "contents"
      "LitBignum" -> LitBignum <$> o .: "contents"
      "LitText" -> LitText <$> o .: "contents"
      "LitBytes" -> LitBytes . BS.pack <$> o .: "contents"
      "LitBool" -> LitBool <$> o .: "contents"
      "LitFloat" -> LitFloat <$> o .: "contents"
      other -> fail ("unknown literal value: " <> toString other)

renderLiteral :: LiteralValue -> Text
renderLiteral = \case
  LitUInt number -> show number
  LitNInt number -> "-" <> show number
  LitBignum number -> show number
  LitText text -> show text
  LitBytes bytes -> "bytes of " <> show (BS.length bytes)
  LitBool value -> show value
  LitFloat value -> show value

-- | A literal as a path writes it, which 'literalP' reads back exactly. It is close to CDDL: @14@, @-1@, @"text"@,
-- @h'0a1b'@, @true@, @1.5@, and @12n@ for a bignum, which CDDL has no literal for.
writeLiteral :: LiteralValue -> Text
writeLiteral = \case
  LitUInt number -> show number
  LitNInt number -> "-" <> show number
  LitBignum number -> show number <> "n"
  LitText text -> show text
  LitBytes bytes -> "h'" <> decodeUtf8 (toLazyByteString (byteStringHex bytes)) <> "'"
  LitBool True -> "true"
  LitBool False -> "false"
  LitFloat number -> show number

-- | Read a literal written by 'writeLiteral'.
literalP :: ReadP LiteralValue
literalP = textP +++ bytesP +++ boolP +++ numberP
  where
    textP = LitText . toText <$> readS_to_P (reads :: ReadS String)
    bytesP = LitBytes . BS.pack <$> (string "h'" *> many byteP <* char '\'')
    byteP = (\high low -> fromIntegral (digitToInt high * 16 + digitToInt low)) <$> satisfy isHexDigit <*> satisfy isHexDigit
    boolP = (LitBool True <$ string "true") +++ (LitBool False <$ string "false")
    numberP = do
      sign <- option "" (string "-")
      whole <- munch1 isDigit
      fraction <- option "" ((:) <$> char '.' <*> munch1 isDigit)
      exponent <- option "" exponentP
      big <- option False (True <$ char 'n')
      case (big, fraction <> exponent) of
        (True, "") -> pure (LitBignum (readNumber (sign <> whole)))
        (False, "") | sign == "-" -> pure (LitNInt (readNumber whole))
        (False, "") -> pure (LitUInt (readNumber whole))
        (False, decimals) -> pure (LitFloat (readNumber (sign <> whole <> decimals)))
        _ -> pfail
    exponentP = do
      marker <- char 'e'
      sign <- option "" (string "-")
      digits <- munch1 isDigit
      pure (marker : sign <> digits)
    readNumber :: (Read a) => String -> a
    readNumber = fromMaybe (error "a number that does not read as one") . readMaybe

fromVariant :: ValueVariant -> LiteralValue
fromVariant = \case
  VUInt number -> LitUInt number
  VNInt number -> LitNInt number
  VBignum number -> LitBignum number
  VText text -> LitText text
  VBytes bytes -> LitBytes bytes
  VBool value -> LitBool value
  VFloat16 value -> LitFloat (realToFrac value)
  VFloat32 value -> LitFloat (realToFrac value)
  VFloat64 value -> LitFloat value

-- | The value of an unsigned integer literal.
uintValue :: LiteralValue -> Maybe Integer
uintValue = \case
  LitUInt number -> Just (toInteger number)
  _ -> Nothing
