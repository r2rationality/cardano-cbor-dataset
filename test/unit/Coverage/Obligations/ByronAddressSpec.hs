module Coverage.Obligations.ByronAddressSpec where

import Coverage.Obligations.ByronAddress
import Data.ByteString qualified as BS
import Test.Hspec

spec :: Spec
spec = describe "The layout of a Byron address" $ do
  it "computes the CRC-32 that the address carries" $
    crc32 "123456789" `shouldBe` 0xcbf43926

  it "finds nothing wrong with a plain address, and nothing to say it has anything" $
    readByron (address payload) `shouldBe` Just (ByronReading [] [])

  it "finds the attributes that the ledger reads" $ do
    readByron (address (payloadOf [0xa1, 0x01, 0x42, 0x41, 0x01] 0x00)) `shouldBe` Just (ByronReading [] [HasPath])
    readByron (address (payloadOf [0xa1, 0x03, 0x41, 0xff] 0x02)) `shouldBe` Just (ByronReading [] [HasUnknownAttribute, IsRedemption])

  it "names the one fault of an address that is wrong in one way" $ do
    faults (address payload `withCrc` 1) `shouldBe` Just [CrcDoesNotMatch]
    faults (address (payloadOf [0xa0] 0x01)) `shouldBe` Just [TypeNotAllowed]
    faults (address (payloadOf [0xa1, 0x01, 0x41, 0xff] 0x00)) `shouldBe` Just [PathNotBytes]
    faults (address (payloadOf [0xa1, 0x02, 0x42, 0x01, 0x00] 0x00)) `shouldBe` Just [MagicNotUnsigned]
    faults (address (payloadOf [0xa2, 0x02, 0x41, 0x01, 0x01, 0x42, 0x41, 0x01] 0x00)) `shouldBe` Just [KeysNotAscending]
    faults (address (payload <> BS.pack [0x00])) `shouldBe` Just [BytesAfterPayload]

  it "does not read bytes that are not a Byron address" $ do
    readByron (BS.pack [0x61, 0x00]) `shouldBe` Nothing
    readByron (BS.pack [0x82, 0x01, 0x02]) `shouldBe` Nothing
  where
    faults = fmap readingFaults . readByron
    root = BS.pack (0x58 : 0x1c : replicate 28 0x11)
    payloadOf attributes kind = BS.concat [BS.pack [0x83], root, BS.pack attributes, BS.pack [kind]]
    payload = payloadOf [0xa0] 0x00
    address body = BS.concat [BS.pack [0x82, 0xd8, 0x18, 0x58, fromIntegral (BS.length body)], body, BS.pack (0x1a : crcBytes (crc32 body))]
    crcBytes crc = [fromIntegral (crc `div` (256 ^ n) `mod` 256) | n <- [3, 2, 1, 0 :: Int]]
    withCrc bytes delta = BS.take (BS.length bytes - 1) bytes <> BS.pack [BS.last bytes + delta]
