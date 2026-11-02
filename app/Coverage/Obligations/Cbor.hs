module Coverage.Obligations.Cbor where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term (Term (..), decodeTerm)
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL

-- | A CBOR value as it was written.
--
--  We can't reuse cborg's 'Term' because a 'Term' keeps the number that a head carries and drops how the head spells it,
--  so @82@ and @98 02@ read as the same array of two.
--  However, whether a sample spells a length or a number in a wider head than it needs is something the coverage has to
-- be able to ask, so a sample is read into this type instead.
data Cbor
  = -- | An integer of major type 0 or 1, whatever its size, and how its head is spelled
    --   Note: a bignum is different, it is tag 2 or 3 over a byte string
    CInteger Width Integer
  | CBytes Length BS.ByteString
  | CText Length T.Text
  | CArray Length [Cbor]
  | -- | The entries in the order they were written, a repeated key included.
    CMap Length [(Cbor, Cbor)]
  | -- | A tag, with how its number is spelled.
    CTag Width Word64 Cbor
  | CBool Bool
  | CNull
  | -- | A float, @undefined@ or any other simple value. Nothing here asks about them, so they stay as cborg reads them.
    COpaque Term
  deriving (Eq, Show)

-- | How a head spells the number that it carries.
data Width
  = -- | The shortest head that holds the number: @82@ for 2, @18 18@ for 24.
    Minimal
  | -- | A longer head than the number needs, but not the 8-byte one: @98 02@ for 2. It is the same number, and a decoder
    -- has to read it.
    Wide
  | -- | The 8-byte head for a number that fits in fewer bytes: @9b 00 00 00 00 00 00 00 02@ for 2. It is the only spelling
    -- that cborg reports as another token type, @TypeListLen64@ and its kin, so a decoder that looks at the token type can
    -- treat it differently from every other.
    Widest
  deriving (Eq, Ord, Show)

-- | How a string, an array or a map states its length.
data Length
  = -- | The length is in the head, spelled in this width.
    Definite Width
  | -- | The length is not stated and a break ends the value.
    Indefinite
  deriving (Eq, Ord, Show)

-- | The one CBOR value these bytes hold.
readCbor :: BS.ByteString -> Either Text Cbor
readCbor input = do
  (value, end) <- item 0
  if end == BS.length input then Right value else Left "trailing bytes after a complete value"
  where
    item :: Int -> Either Text (Cbor, Int)
    item position = do
      initial <- byteAt position
      let major = initial `shiftR` 5
          info = initial .&. 31
      (argument, after) <- headOf info (position + 1)
      case (major, argument) of
        (0, Just number) -> Right (CInteger (widthOf info number) (toInteger number), after)
        (1, Just number) -> Right (CInteger (widthOf info number) (-1 - toInteger number), after)
        (2, Just size) -> do
          (payload, end) <- slice after size
          Right (CBytes (Definite (widthOf info size)) payload, end)
        (3, Just size) -> do
          (payload, end) <- slice after size
          decoded <- utf8 payload
          Right (CText (Definite (widthOf info size)) decoded, end)
        (2, Nothing) -> (\(payload, end) -> (CBytes Indefinite payload, end)) <$> chunks 2 after
        (3, Nothing) -> do
          (payload, end) <- chunks 3 after
          decoded <- utf8 payload
          Right (CText Indefinite decoded, end)
        (4, Just size) -> (\(entries, end) -> (CArray (Definite (widthOf info size)) entries, end)) <$> items size after
        (4, Nothing) -> (\(entries, end) -> (CArray Indefinite entries, end)) <$> itemsUntilBreak after
        (5, Just size) -> (\(entries, end) -> (CMap (Definite (widthOf info size)) (pairs entries), end)) <$> items (2 * size) after
        (5, Nothing) -> (\(entries, end) -> (CMap Indefinite (pairs entries), end)) <$> itemsUntilBreak after
        (6, Just number) -> (\(inner, end) -> (CTag (widthOf info number) number inner, end)) <$> item after
        (7, Just _) -> simple position info after
        _ -> Left ("a break where a value starts, at byte " <> show position)

    -- A simple value or a float is the head and the payload that it names. cborg reads it from those bytes alone.
    simple position info after = case info of
      20 -> Right (CBool False, after)
      21 -> Right (CBool True, after)
      22 -> Right (CNull, after)
      _ -> case CBOR.deserialiseFromBytes decodeTerm (LBS.fromStrict (BS.take (after - position) (BS.drop position input))) of
        Right (_, term) -> Right (COpaque term, after)
        Left failure -> Left (show failure)

    items :: Word64 -> Int -> Either Text ([Cbor], Int)
    items 0 position = Right ([], position)
    items count position = do
      (entry, next) <- item position
      (rest, end) <- items (count - 1) next
      Right (entry : rest, end)

    itemsUntilBreak :: Int -> Either Text ([Cbor], Int)
    itemsUntilBreak position = do
      initial <- byteAt position
      if initial == 0xff
        then Right ([], position + 1)
        else do
          (entry, next) <- item position
          (rest, end) <- itemsUntilBreak next
          Right (entry : rest, end)

    pairs (key : value : rest) = (key, value) : pairs rest
    pairs _ = []

    -- The pieces of an indefinite string, each a definite string of the same major type, up to the break.
    chunks :: Word8 -> Int -> Either Text (BS.ByteString, Int)
    chunks major position = do
      initial <- byteAt position
      if initial == 0xff
        then Right (BS.empty, position + 1)
        else do
          (argument, after) <- headOf (initial .&. 31) (position + 1)
          case argument of
            Just size | initial `shiftR` 5 == major -> do
              (piece, next) <- slice after size
              (rest, end) <- chunks major next
              Right (piece <> rest, end)
            _ -> Left ("a piece of an indefinite string that is not a definite string, at byte " <> show position)

    slice :: Int -> Word64 -> Either Text (BS.ByteString, Int)
    slice from size
      | size <= fromIntegral (BS.length input - from) = Right (BS.take (fromIntegral size) (BS.drop from input), from + fromIntegral size)
      | otherwise = Left ("a string that runs past the end, at byte " <> show from)

    utf8 = first (const "text that is not UTF-8") . decodeUtf8'

    byteAt position = maybeToRight ("the value ends early, at byte " <> show position) (input BS.!? position)

    -- The number that a head carries and the position after the head. Nothing for the indefinite length and the break.
    headOf :: Word8 -> Int -> Either Text (Maybe Word64, Int)
    headOf info after
      | info < 24 = Right (Just (fromIntegral info), after)
      | info == 31 = Right (Nothing, after)
      | info <= 27 =
          let width = 2 ^ (fromIntegral info - 24 :: Int)
           in if after + width <= BS.length input
                then Right (Just (BS.foldl' (\number byte -> number * 256 + fromIntegral byte) 0 (BS.take width (BS.drop after input))), after + width)
                else Left ("a head that runs past the end, at byte " <> show after)
      | otherwise = Left ("a head that no CBOR value starts with, at byte " <> show (after - 1))

-- | Whether the head, with this additional information, spells the number wider than it needs.
widthOf :: Word8 -> Word64 -> Width
widthOf info number
  | info == 27 && number < 4294967296 = Widest
  | info == 24 && number < 24 = Wide
  | info == 25 && number < 256 = Wide
  | info == 26 && number < 65536 = Wide
  | otherwise = Minimal

-- | The value as cborg reads it, which is what the custom validators and the CDDL validator take. A tag stays a tag:
-- a bignum is not folded into an integer.
toTerm :: Cbor -> Term
toTerm = \case
  CInteger _ number
    | number >= toInteger (minBound :: Int) && number <= toInteger (maxBound :: Int) -> TInt (fromInteger number)
    | otherwise -> TInteger number
  CBytes (Definite _) payload -> TBytes payload
  CBytes Indefinite payload -> TBytesI (LBS.fromStrict payload)
  CText (Definite _) payload -> TString payload
  CText Indefinite payload -> TStringI (TL.fromStrict payload)
  CArray (Definite _) entries -> TList (map toTerm entries)
  CArray Indefinite entries -> TListI (map toTerm entries)
  CMap (Definite _) entries -> TMap (map entry entries)
  CMap Indefinite entries -> TMapI (map entry entries)
  CTag _ number inner -> TTagged number (toTerm inner)
  CBool value -> TBool value
  CNull -> TNull
  COpaque term -> term
  where
    entry (key, value) = (toTerm key, toTerm value)

-- * Building a value

-- | A non-negative integer with the shortest head.
uint :: Integer -> Cbor
uint = CInteger Minimal

-- | A definite array with the shortest head.
array :: [Cbor] -> Cbor
array = CArray (Definite Minimal)

indefiniteArray :: [Cbor] -> Cbor
indefiniteArray = CArray Indefinite

-- | A definite array whose head is wider than its length needs.
wideArray :: [Cbor] -> Cbor
wideArray = CArray (Definite Wide)

-- | A definite map with the shortest head.
mapOf :: [(Cbor, Cbor)] -> Cbor
mapOf = CMap (Definite Minimal)

indefiniteMap :: [(Cbor, Cbor)] -> Cbor
indefiniteMap = CMap Indefinite

-- | A definite byte string with the shortest head.
byteString :: BS.ByteString -> Cbor
byteString = CBytes (Definite Minimal)

-- | A definite text string with the shortest head.
textString :: T.Text -> Cbor
textString = CText (Definite Minimal)

-- | A tag with the shortest head.
tagged :: Word64 -> Cbor -> Cbor
tagged = CTag Minimal
