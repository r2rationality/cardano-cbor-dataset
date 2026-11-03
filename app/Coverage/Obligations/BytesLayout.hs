module Coverage.Obligations.BytesLayout where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Data.Bits (shiftL, testBit, (.&.))
import Data.ByteString qualified as BS

-- | What a decoder expects inside the bytes of a rule that the specification states as plain @bytes@.
--
-- The specification says @address = bytes@ and stops there, so no obligation can follow from it. The ledger decodes a
-- pointer address field by field, and refuses a number that is too large or a byte that follows the last field.
data BytesLayout = BytesLayout
  { -- | How the obligations name this layout.
    layoutName :: Text,
    -- | The rule whose bytes follow this layout.
    layoutRule :: Name,
    -- | The first bytes that start this layout.
    layoutHeaders :: [Word8],
    layoutFields :: [Field]
  }
  deriving (Eq, Show)

-- | A part of the bytes, after the header.
data Field
  = -- | Bytes that the decoder takes as they are, such as a hash.
    Fixed Int
  | -- | A number written in groups of 7 bits, each group but the last with its high bit set. It has a name, and the
    -- number of bits that the decoder holds it in.
    Number Text Int
  deriving (Eq, Show)

-- | The layouts of the bytes of a rule.
layoutsOf :: Name -> [BytesLayout]
layoutsOf name = [layout | layout <- knownLayouts, layoutRule layout == name]

knownLayouts :: [BytesLayout]
knownLayouts = [pointerAddressLayout]

-- | A pointer address, as @decodeAddrStateLenientT@ reads it from protocol version 9 on: the header, the hash of the
-- payment credential, then a slot in 32 bits, a transaction index in 16 bits and a certificate index in 16 bits. The
-- whole input must be used.
pointerAddressLayout :: BytesLayout
pointerAddressLayout =
  BytesLayout
    { layoutName = "pointer address",
      layoutRule = Name "address",
      -- Pointer in the high nibble (4 for a key payment, 5 for a script), the network in the lowest bit.
      layoutHeaders = [0x40, 0x41, 0x50, 0x51],
      layoutFields = [Fixed 28, Number "slot" 32, Number "transaction index" 16, Number "certificate index" 16]
    }

-- | What the bytes hold, read field by field.
data Reading = Reading
  { -- | Each number with its name, its value, the number of bytes that it takes, and the number of bits it is held in.
    readingNumbers :: [(Text, Integer, Int, Int)],
    -- | What follows the last field.
    readingRest :: BS.ByteString
  }
  deriving (Eq, Show)

-- | Read the bytes as the layout says. 'Nothing' when they do not start like the layout, or end inside a field.
-- A number is read whatever its size, so that a number that is too large can be told from bytes that are not this
-- layout at all.
readLayout :: BytesLayout -> BS.ByteString -> Maybe Reading
readLayout layout bytes = do
  (header, afterHeader) <- BS.uncons bytes
  guard (header `elem` layoutHeaders layout)
  go [] afterHeader (layoutFields layout)
  where
    go numbers rest [] = Just (Reading (reverse numbers) rest)
    go numbers rest (Fixed size : fields)
      | BS.length rest >= size = go numbers (BS.drop size rest) fields
      | otherwise = Nothing
    go numbers rest (Number name bits : fields) = do
      let (groups, afterGroups) = BS.span (`testBit` 7) rest
      (final, remaining) <- BS.uncons afterGroups
      let used = BS.snoc groups final
          value = BS.foldl' (\total byte -> (total `shiftL` 7) + toInteger (byte .&. 0x7f)) 0 used
      go ((name, value, BS.length used, bits) : numbers) remaining fields

-- | The largest value that a number of this many bits holds.
fieldMaximum :: Int -> Integer
fieldMaximum bits = (1 `shiftL` bits) - 1

-- | Whether a number is within what the decoder holds: its value fits, and it takes no more groups than the bits need.
fieldFits :: (Text, Integer, Int, Int) -> Bool
fieldFits (_, value, size, bits) = value <= fieldMaximum bits && size <= (bits + 6) `div` 7

-- | Whether the decoder reads all of the bytes as this layout.
fitsLayout :: Reading -> Bool
fitsLayout reading = all fieldFits (readingNumbers reading) && BS.null (readingRest reading)
