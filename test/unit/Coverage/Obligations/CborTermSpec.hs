module Coverage.Obligations.CborTermSpec where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm)
import Coverage.Obligations.Cbor
import Coverage.Obligations.CborTerm (entryCount, followPath, hasWideHead, hasWidestHead, isIndefinite)
import Coverage.Obligations.LiteralValue (LiteralValue (..))
import Coverage.Obligations.Shape (Shape (..))
import Coverage.Obligations.Step (Step (..))
import Coverage.Obligations.Trail (trailOf)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

spec = do
  describe "Choices" $ do
    -- A choice narrows the schema without moving in the term, so the step has
    -- to decide which branch the term took rather than descend into it.
    it "stays on the same term, because a choice picks a branch and not a position" $
      followPath (trailOf [StepChoice 0 (ShapeArray Nothing)]) redeemerList
        `shouldBe` [redeemerList]

    -- Both redeemers branches forbid an empty container. Without this check an
    -- empty array answers for the empty map as well, which is what the corpus
    -- has: five empty arrays and no empty map.
    it "refuses a branch of the wrong kind, so an array does not answer for a map" $
      followPath (trailOf [StepChoice 1 ShapeMap]) emptyList `shouldBe` []

    -- drep is [0, addr_keyhash // 1, script_hash // 2 // 3]. The alternatives
    -- are all arrays, so only the leading uint separates them.
    it "tells discriminated branches apart by the uint they open with" $ do
      followPath (trailOf [StepChoice 0 (ShapeArray (Just 0))]) drepKeyHash
        `shouldBe` [drepKeyHash]
      followPath (trailOf [StepChoice 2 (ShapeArray (Just 2))]) drepKeyHash
        `shouldBe` []

    it "accepts any term for a branch that is not a container of its own" $
      followPath (trailOf [StepChoice 0 ShapeOther]) (uint 7) `shouldBe` [uint 7]

  describe "Containers" $ do
    it "indexes an array, definite or not" $ do
      followPath (trailOf [StepArray 1]) (array [uint 7, uint 8]) `shouldBe` [uint 8]
      followPath (trailOf [StepArray 1]) (indefiniteArray [uint 7, uint 8]) `shouldBe` [uint 8]

    it "finds a keyed entry by its key, wherever the sample put it" $
      -- A schema numbers its keyed entries in the order the specification lists
      -- them, and a sample carries only the fields it has, so the two numberings
      -- are unrelated. Looking the key up is what makes them line up.
      followPath (trailOf [StepMapKey (LitUInt 13), StepValue]) (mapOf [(uint 0, uint 1), (uint 13, uint 9)])
        `shouldBe` [uint 9]

    it "finds nothing when the sample leaves an optional key out" $
      followPath (trailOf [StepMapKey (LitUInt 13), StepValue]) (mapOf [(uint 0, uint 1)])
        `shouldBe` []

    -- An entry whose key is not a literal has no key to look up, so position is all there is.
    it "takes either side of an entry found by position" $ do
      followPath (trailOf [StepMapAt 0, StepKey]) oneEntryMap `shouldBe` [uint 5]
      followPath (trailOf [StepMapAt 0, StepValue]) oneEntryMap `shouldBe` [uint 9]

    -- [a, ? b, c] has 3 elements with b and 2 without it, so c is at position 2 or 1, and b is absent from 2 elements.
    it "finds an element after an optional one by the length of the array" $ do
      followPath (trailOf [StepArrayOf 2 3]) (array [uint 7, uint 8, uint 9]) `shouldBe` [uint 9]
      followPath (trailOf [StepArrayOf 2 3]) (array [uint 7, uint 9]) `shouldBe` [uint 9]
      followPath (trailOf [StepArrayOf 1 3, StepOccur 0]) (array [uint 7, uint 8, uint 9]) `shouldBe` [uint 8]
      followPath (trailOf [StepArrayOf 1 3, StepOccur 0]) (array [uint 7, uint 9]) `shouldBe` []
      followPath (trailOf [StepArrayOf 2 3]) (array [uint 7]) `shouldBe` []

    -- A repeated entry is one entry in the schema and many in the term, and a constraint on it holds for each of them.
    it "reaches every occurrence of a repeated entry" $ do
      followPath (trailOf [StepArray 0, StepRepeat 0]) (array [uint 7, uint 8]) `shouldBe` [uint 7, uint 8]
      followPath (trailOf [StepArray 1, StepRepeat 0]) (indefiniteArray [uint 7, uint 8, uint 9]) `shouldBe` [uint 8, uint 9]
      followPath (trailOf [StepMapAt 0, StepRepeat 0, StepValue]) (mapOf [(uint 5, uint 9), (uint 6, uint 10)])
        `shouldBe` [uint 9, uint 10]
      followPath (trailOf [StepMapAmong 1 [LitUInt 0], StepRepeat 0, StepKey]) (mapOf [(uint 0, uint 1), (uint 5, uint 9), (uint 6, uint 10)])
        `shouldBe` [uint 5, uint 6]

    it "reaches only its position for an entry that appears at most once" $
      followPath (trailOf [StepArray 1, StepOccur 0]) (array [uint 7, uint 8, uint 9]) `shouldBe` [uint 8]

    it "stops at an index the term does not have" $
      followPath (trailOf [StepArray 2]) (array [uint 7]) `shouldBe` []

    it "stops when the term is not the kind the step expects" $
      followPath (trailOf [StepArray 0]) oneEntryMap `shouldBe` []

    -- An optional entry is an occurrence wrapped around the keyed entry, so the
    -- path reads map{13}.occur.value and the wrapper has to be stepped over
    -- between finding the entry and picking a side of it.
    it "steps over a wrapper sitting between a map entry and the side of it" $
      followPath (trailOf [StepMapKey (LitUInt 13), StepOccur 0, StepValue]) (mapOf [(uint 13, uint 9)])
        `shouldBe` [uint 9]

  describe "Wrappers" $ do
    -- These say something about the schema and nothing about the encoding, so
    -- the term stays where it is while the path moves on.
    it "steps over the wrappers that leave no trace in the term" $
      followPath (trailOf [StepOccur 0, StepGenerator, StepValidator, StepControl, StepArray 0]) (array [uint 7])
        `shouldBe` [uint 7]

    it "descends a tag only when the number matches" $ do
      followPath (trailOf [StepTag 258, StepArray 0]) (tagged 258 (array [uint 7]))
        `shouldBe` [uint 7]
      followPath (trailOf [StepTag 258, StepArray 0]) (tagged 259 (array [uint 7]))
        `shouldBe` []

    it "returns the term itself for an empty path" $
      followPath (trailOf []) redeemerList `shouldBe` [redeemerList]

  describe "Reading a container" $ do
    it "counts entries whichever length form was used" $ do
      entryCount emptyList `shouldBe` Just 0
      entryCount (array [uint 7, uint 8]) `shouldBe` Just 2
      entryCount oneEntryMap `shouldBe` Just 1
      entryCount (uint 7) `shouldBe` Nothing

    it "reports the length form, which the specification permits either way" $ do
      isIndefinite (indefiniteArray []) `shouldBe` Just True
      isIndefinite (array []) `shouldBe` Just False
      isIndefinite (uint 7) `shouldBe` Nothing

  describe "The spelling of a head" $ do
    let read' = readCbor . BS.pack

    it "reads an array whose head is wider than its length needs" $ do
      read' [0x98, 0x02, 0x01, 0x02] `shouldBe` Right (wideArray [uint 1, uint 2])
      read' [0x99, 0x00, 0x02, 0x01, 0x02] `shouldBe` Right (wideArray [uint 1, uint 2])
      read' [0x9a, 0x00, 0x00, 0x00, 0x00] `shouldBe` Right (wideArray [])
      -- the 8-byte head is a case of its own, since cborg reports it as another token type
      read' [0x9b, 0, 0, 0, 0, 0, 0, 0, 0x01, 0x01] `shouldBe` Right (CArray (Definite Widest) [uint 1])

    it "reads the minimal head, the indefinite form, and a head that its length fills" $ do
      read' [0x82, 0x01, 0x02] `shouldBe` Right (array [uint 1, uint 2])
      read' [0x9f, 0x01, 0xff] `shouldBe` Right (indefiniteArray [uint 1])
      read' ([0x98, 0x18] <> replicate 24 0x00) `shouldBe` Right (array (replicate 24 (uint 0)))

    it "says which head of a nested value is wide" $ do
      -- [[1], 98 01 [2], [3]]
      read' [0x83, 0x81, 0x01, 0x98, 0x01, 0x02, 0x81, 0x03]
        `shouldBe` Right (array [array [uint 1], wideArray [uint 2], array [uint 3]])
      -- a map whose key is an empty array and whose value is a wide empty array
      read' [0xa1, 0x80, 0x98, 0x00] `shouldBe` Right (mapOf [(array [], wideArray [])])

    it "reads the heads of integers, strings, maps and tags" $ do
      read' [0x18, 0x01] `shouldBe` Right (CInteger Wide 1)
      read' [0x01] `shouldBe` Right (uint 1)
      read' [0x38, 0x00] `shouldBe` Right (CInteger Wide (-1))
      read' [0x58, 0x01, 0x07] `shouldBe` Right (CBytes (Definite Wide) "\7")
      read' [0x78, 0x01, 0x61] `shouldBe` Right (CText (Definite Wide) "a")
      read' [0xb8, 0x00] `shouldBe` Right (CMap (Definite Wide) [])
      read' [0xd8, 0x01, 0x40] `shouldBe` Right (CTag Wide 1 (byteString ""))
      read' [0xd8, 0x18, 0x40] `shouldBe` Right (CTag Minimal 24 (byteString ""))

    it "reads through strings, indefinite ones included" $ do
      -- a chunked byte string, then a wide array: [5f 41 00 ff, 98 00]
      read' [0x82, 0x5f, 0x41, 0x00, 0xff, 0x98, 0x00]
        `shouldBe` Right (array [CBytes Indefinite "\0", wideArray []])

    it "refuses bytes that are not exactly one value" $ do
      read' [0x98] `shouldSatisfy` isLeft
      read' [0x82, 0x01] `shouldSatisfy` isLeft
      read' [0x01, 0x01] `shouldSatisfy` isLeft
      read' [0x62, 0xff, 0xff] `shouldSatisfy` isLeft

    it "reads the first value and says where it ends, when bytes follow" $ do
      readCborPrefix (BS.pack [0x82, 0x01, 0x02, 0x00]) `shouldBe` Right (array [uint 1, uint 2], 3)
      readCborPrefix (BS.pack [0x01, 0xff]) `shouldBe` Right (uint 1, 1)
      readCborPrefix (BS.pack [0x82, 0x01]) `shouldSatisfy` isLeft

    it "answers whether a head is wide, for the values that have one" $ do
      hasWideHead (wideArray []) `shouldBe` Just True
      hasWideHead (array []) `shouldBe` Just False
      hasWideHead (indefiniteArray []) `shouldBe` Just False
      hasWideHead (CInteger Wide 1) `shouldBe` Just True
      hasWideHead (CInteger Widest 1) `shouldBe` Just True
      hasWidestHead (CInteger Widest 1) `shouldBe` Just True
      hasWidestHead (CInteger Wide 1) `shouldBe` Just False
      hasWidestHead (uint 1) `shouldBe` Just False
      -- an 8-byte head that the number needs is the minimal one
      hasWidestHead (CInteger Minimal 4294967296) `shouldBe` Just False
      hasWideHead CNull `shouldBe` Nothing

    -- Reading the spelling must not change the value. cborg is the reference for what the bytes say, except that it
    -- folds a bignum into an integer, which 'toTerm' does not.
    it "gives the value that cborg reads, whatever the spelling"
      $ for_
        [ [0x98, 0x02, 0x01, 0x02],
          [0x9f, 0x01, 0xa1, 0x01, 0x02, 0xff],
          [0x82, 0x5f, 0x41, 0x00, 0xff, 0x7f, 0x61, 0x61, 0xff],
          [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
          [0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
          [0x83, 0xf4, 0xf6, 0xf7],
          [0xf9, 0x3c, 0x00],
          [0xfb, 0x3f, 0xf0, 0, 0, 0, 0, 0, 0]
        ]
      $ \input ->
        rightToMaybe (toTerm <$> read' input)
          `shouldBe` either (const Nothing) (Just . snd) (deserialiseFromBytes decodeTerm (LBS.pack input))

-- HELPERS

-- | @9fff@: an empty indefinite array
emptyList :: Cbor
emptyList = indefiniteArray []

-- | @[[0, 0, h'', [0, 0]]]@, the shape of a one element redeemers array.
redeemerList :: Cbor
redeemerList = array [array [uint 0, uint 0, byteString "", array [uint 0, uint 0]]]

-- | @[0, h'']@: the key hash branch of drep.
drepKeyHash :: Cbor
drepKeyHash = array [uint 0, byteString ""]

-- | @a{5: 9}@, a map with one entry.
oneEntryMap :: Cbor
oneEntryMap = mapOf [(uint 5, uint 9)]
