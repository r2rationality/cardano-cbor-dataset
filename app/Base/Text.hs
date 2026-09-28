-- | Bytes written as hexadecimal text, and read back from it.
module Base.Text where

import Cardano.Crypto.Hash (Hash, SHA256, hashToTextAsHex, hashWith)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isHexDigit)
import Data.Text qualified as T

-- | The SHA-256 digest of some bytes, in hexadecimal.
sha256Hex :: BS.ByteString -> Text
sha256Hex bytes = hashToTextAsHex (hashWith id bytes :: Hash SHA256 BS.ByteString)

-- | The bytes a hexadecimal text stands for, failing on anything else.
decodeHex :: Text -> IO BS.ByteString
decodeHex encoded
  | T.null encoded || odd (T.length encoded) || not (T.all isHexDigit encoded) =
      fail $ "invalid hexadecimal output '" <> T.unpack encoded <> "'"
  | otherwise = pure $ BS.pack $ decodePairs (T.unpack encoded)
  where
    decodePairs [] = []
    decodePairs (high : low : rest) =
      fromIntegral (digitToInt high * 16 + digitToInt low) : decodePairs rest
    decodePairs _ = error "internal error: odd hexadecimal length"
