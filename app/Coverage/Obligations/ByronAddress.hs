-- | What the ledger expects inside the bytes of a Byron address.
--
-- The bytes hold CBOR, and that CBOR holds more CBOR: an array of the payload under tag 24 and a CRC, where the
-- payload is an array of a root, a map of attributes and a type, and two of the attributes hold CBOR in their own bytes.
-- The specification says only @address = bytes@. This module reads the bytes the way @Cardano.Chain.Common.Address@
-- does, and reports what is wrong with them, so that an obligation can ask for a value with exactly one fault.
module Coverage.Obligations.ByronAddress
  ( Fault (..),
    Feature (..),
    ByronReading (..),
    readByron,
    byronName,
    crc32,
    byronAcceptances,
    byronViolations,
  )
where

import Coverage.Obligations.Cbor
import Data.Bits (complement, shiftR, (.&.))
import Data.ByteString qualified as BS

-- | How the obligations name this layout.
byronName :: Text
byronName = "Byron address"

-- | A way that the bytes are wrong. The ledger refuses each one.
data Fault
  = CrcDoesNotMatch
  | TypeNotAllowed
  | KeysNotAscending
  | PathNotBytes
  | MagicNotUnsigned
  | BytesAfterPayload
  | PayloadNotThree
  | RootNotTwentyEightBytes
  | AttributesIndefinite
  | PayloadArrayIndefinite
  | StringIndefinite
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Something that a well formed address can have, and the ledger reads.
data Feature
  = HasPath
  | HasMagic
  | HasUnknownAttribute
  | IsRedemption
  | PayloadHeadWide
  deriving (Eq, Ord, Show, Enum, Bounded)

data ByronReading = ByronReading
  { readingFaults :: [Fault],
    readingFeatures :: [Feature]
  }
  deriving (Eq, Show)

-- | The obligations that ask for a well formed address, each with what it has.
byronAcceptances :: [(Text, ByronReading -> Bool)]
byronAcceptances =
  [ ("is well formed", null . readingFaults),
    ("has the attribute 1, with one CBOR byte string inside", has HasPath),
    ("has the attribute 2, with one CBOR unsigned integer inside", has HasMagic),
    ("has an attribute that no rule knows, with any bytes inside", has HasUnknownAttribute),
    ("has the type 2", has IsRedemption),
    ("has a payload array with a head wider than it needs", has PayloadHeadWide)
  ]
  where
    has feature reading = null (readingFaults reading) && feature `elem` readingFeatures reading

-- | The obligations that ask for an address with exactly one fault.
byronViolations :: [(Text, Fault)]
byronViolations =
  [ ("has a CRC that does not match the payload", CrcDoesNotMatch),
    ("has a type that is not 0 or 2", TypeNotAllowed),
    ("has attribute keys that are not in strictly ascending order", KeysNotAscending),
    ("has the attribute 1 with contents that are not one CBOR byte string", PathNotBytes),
    ("has the attribute 2 with contents that are not one CBOR unsigned integer", MagicNotUnsigned),
    ("has bytes after the payload", BytesAfterPayload),
    ("has a payload that is not an array of 3", PayloadNotThree),
    ("has a root that is not 28 bytes", RootNotTwentyEightBytes),
    ("has attributes in an indefinite map", AttributesIndefinite),
    ("has a payload in an indefinite array", PayloadArrayIndefinite),
    ("has an indefinite byte string", StringIndefinite)
  ]

-- | Read the bytes as a Byron address. 'Nothing' when they are not one at all: not an array of a tagged payload and a
-- number.
readByron :: BS.ByteString -> Maybe ByronReading
readByron bytes = do
  CArray (Definite _) [CTag _ 24 (CBytes payloadLength payload), CInteger _ crc] <- rightToMaybe (readCbor bytes)
  let outerFaults =
        [CrcDoesNotMatch | toInteger (crc32 payload) /= crc]
          <> [StringIndefinite | payloadLength == Indefinite]
  (inner, end) <- rightToMaybe (readCborPrefix payload)
  let afterPayload = [BytesAfterPayload | end < BS.length payload]
      (faults, features) = readPayload inner
  pure (ByronReading (outerFaults <> afterPayload <> faults) features)

-- | The faults and features of the payload array.
readPayload :: Cbor -> ([Fault], [Feature])
readPayload = \case
  CArray len [root, attributes, kind] ->
    let (attributeFaults, attributeFeatures) = readAttributes attributes
     in ( [PayloadArrayIndefinite | len == Indefinite]
            <> [RootNotTwentyEightBytes | not (isRoot root)]
            <> attributeFaults
            <> [TypeNotAllowed | not (isType kind)],
          attributeFeatures
            <> [PayloadHeadWide | len == Definite Wide]
            <> [IsRedemption | kind == CInteger Minimal 2]
        )
  _ -> ([PayloadNotThree], [])
  where
    isRoot = \case
      CBytes _ root -> BS.length root == 28
      _ -> False
    isType = \case
      CInteger _ kind -> kind == 0 || kind == 2
      _ -> False

-- | The faults and features of the attribute map.
readAttributes :: Cbor -> ([Fault], [Feature])
readAttributes = \case
  CMap len pairs ->
    let keys = [key | (CInteger _ key, _) <- pairs]
     in ( [AttributesIndefinite | len == Indefinite]
            <> [KeysNotAscending | not (strictlyAscending keys)]
            <> concatMap attributeFaults pairs,
          concatMap attributeFeatures pairs
        )
  _ -> ([AttributesIndefinite], [])
  where
    strictlyAscending keys = and (zipWith (<) keys (drop 1 keys))
    attributeFaults = \case
      (CInteger _ key, CBytes len inner) ->
        [StringIndefinite | len == Indefinite]
          <> case (key, readCbor inner) of
            (1, result) | not (isBytes result) -> [PathNotBytes]
            (2, result) | not (isUnsigned result) -> [MagicNotUnsigned]
            _ -> []
      _ -> [StringIndefinite]
    attributeFeatures = \case
      (CInteger _ 1, _) -> [HasPath]
      (CInteger _ 2, _) -> [HasMagic]
      (CInteger _ _, _) -> [HasUnknownAttribute]
      _ -> []
    isBytes = \case
      Right (CBytes _ _) -> True
      _ -> False
    isUnsigned = \case
      Right (CInteger _ number) -> number >= 0
      _ -> False

-- | The CRC-32 of the bytes, as the ledger checks it (the one that zlib calls crc32).
crc32 :: BS.ByteString -> Word32
crc32 = complement . BS.foldl' step 0xffffffff
  where
    step crc byte = foldl' (\value _ -> shift value) (crc `xor` fromIntegral byte) [1 .. 8 :: Int]
    shift value = if value .&. 1 == 1 then (value `shiftR` 1) `xor` 0xedb88320 else value `shiftR` 1
