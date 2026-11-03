module Coverage.Obligations.BytesLayoutSpec where

import Coverage.Obligations.BytesLayout
import Data.ByteString qualified as BS
import Test.Hspec

spec :: Spec
spec = describe "The layout of a pointer address" $ do
  it "reads a pointer address with every number at its largest value" $
    fits (pointer [0x8f, 0xff, 0xff, 0xff, 0x7f] [0x83, 0xff, 0x7f] [0x83, 0xff, 0x7f] []) `shouldBe` Just True

  it "reads the numbers whatever their size, and says which one does not fit" $ do
    fitting (pointer [0x90, 0x80, 0x80, 0x80, 0x00] [0x01] [0x01] []) `shouldBe` Just [False, True, True]
    fitting (pointer [0x01] [0x84, 0x80, 0x00] [0x01] []) `shouldBe` Just [True, False, True]
    fitting (pointer [0x01] [0x01] [0x84, 0x80, 0x00] []) `shouldBe` Just [True, True, False]

  it "refuses a number that takes more groups than its bits need, even when its value is small" $
    fitting (pointer [0x01] [0x80, 0x80, 0x80, 0x01] [0x01] []) `shouldBe` Just [True, False, True]

  it "keeps the bytes after the last field apart from the fields" $ do
    fits (pointer [0x01] [0x02] [0x03] [0x00]) `shouldBe` Just False
    fmap (BS.unpack . readingRest) (reading (pointer [0x01] [0x02] [0x03] [0x00])) `shouldBe` Just [0x00]

  it "does not read bytes that do not start like a pointer address, or that end inside a field" $ do
    reading (BS.pack (0x61 : replicate 28 0x22)) `shouldBe` Nothing
    reading (BS.pack (0x41 : replicate 28 0x22 <> [0x01, 0x02])) `shouldBe` Nothing
    reading (BS.pack (0x41 : replicate 28 0x22 <> [0x01, 0x82])) `shouldBe` Nothing
  where
    layout = pointerAddressLayout
    reading = readLayout layout
    fits = fmap fitsLayout . reading
    fitting = fmap (map numberFits . readingNumbers) . reading
    pointer slot transaction certificate extra =
      BS.pack (0x41 : replicate 28 0x22 <> slot <> transaction <> certificate <> extra)
