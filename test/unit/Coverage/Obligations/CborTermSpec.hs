module Coverage.Obligations.CborTermSpec where

import Codec.CBOR.Term (Term (..))
import Coverage.Obligations.CborTerm (entryCount, followPath, isIndefinite)
import Coverage.Obligations.LiteralValue (LiteralValue (..))
import Coverage.Obligations.Shape (Shape (..))
import Coverage.Obligations.Step (Step (..))
import Coverage.Obligations.Trail (trailOf)
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
      followPath (trailOf [StepChoice 0 ShapeOther]) (TInt 7) `shouldBe` [TInt 7]

  describe "Containers" $ do
    it "indexes an array, definite or not" $ do
      followPath (trailOf [StepArray 1]) (TList [TInt 7, TInt 8]) `shouldBe` [TInt 8]
      followPath (trailOf [StepArray 1]) (TListI [TInt 7, TInt 8]) `shouldBe` [TInt 8]

    it "finds a keyed entry by its key, wherever the sample put it" $
      -- A schema numbers its keyed entries in the order the specification lists
      -- them, and a sample carries only the fields it has, so the two numberings
      -- are unrelated. Looking the key up is what makes them line up.
      followPath (trailOf [StepMapKey (LitUInt 13), StepValue]) (TMap [(TInt 0, TInt 1), (TInt 13, TInt 9)])
        `shouldBe` [TInt 9]

    it "finds nothing when the sample leaves an optional key out" $
      followPath (trailOf [StepMapKey (LitUInt 13), StepValue]) (TMap [(TInt 0, TInt 1)])
        `shouldBe` []

    -- An entry whose key is not a literal has no key to look up, so position is all there is.
    it "takes either side of an entry found by position" $ do
      followPath (trailOf [StepMapAt 0, StepKey]) oneEntryMap `shouldBe` [TInt 5]
      followPath (trailOf [StepMapAt 0, StepValue]) oneEntryMap `shouldBe` [TInt 9]

    -- [a, ? b, c] has 3 elements with b and 2 without it, so c is at position 2 or 1, and b is absent from 2 elements.
    it "finds an element after an optional one by the length of the array" $ do
      followPath (trailOf [StepArrayOf 2 3]) (TList [TInt 7, TInt 8, TInt 9]) `shouldBe` [TInt 9]
      followPath (trailOf [StepArrayOf 2 3]) (TList [TInt 7, TInt 9]) `shouldBe` [TInt 9]
      followPath (trailOf [StepArrayOf 1 3, StepOccur 0]) (TList [TInt 7, TInt 8, TInt 9]) `shouldBe` [TInt 8]
      followPath (trailOf [StepArrayOf 1 3, StepOccur 0]) (TList [TInt 7, TInt 9]) `shouldBe` []
      followPath (trailOf [StepArrayOf 2 3]) (TList [TInt 7]) `shouldBe` []

    -- A repeated entry is one entry in the schema and many in the term, and a constraint on it holds for each of them.
    it "reaches every occurrence of a repeated entry" $ do
      followPath (trailOf [StepArray 0, StepRepeat 0]) (TList [TInt 7, TInt 8]) `shouldBe` [TInt 7, TInt 8]
      followPath (trailOf [StepArray 1, StepRepeat 0]) (TListI [TInt 7, TInt 8, TInt 9]) `shouldBe` [TInt 8, TInt 9]
      followPath (trailOf [StepMapAt 0, StepRepeat 0, StepValue]) (TMap [(TInt 5, TInt 9), (TInt 6, TInt 10)])
        `shouldBe` [TInt 9, TInt 10]
      followPath (trailOf [StepMapAmong 1 [LitUInt 0], StepRepeat 0, StepKey]) (TMap [(TInt 0, TInt 1), (TInt 5, TInt 9), (TInt 6, TInt 10)])
        `shouldBe` [TInt 5, TInt 6]

    it "reaches only its position for an entry that appears at most once" $
      followPath (trailOf [StepArray 1, StepOccur 0]) (TList [TInt 7, TInt 8, TInt 9]) `shouldBe` [TInt 8]

    it "stops at an index the term does not have" $
      followPath (trailOf [StepArray 2]) (TList [TInt 7]) `shouldBe` []

    it "stops when the term is not the kind the step expects" $
      followPath (trailOf [StepArray 0]) oneEntryMap `shouldBe` []

    -- An optional entry is an occurrence wrapped around the keyed entry, so the
    -- path reads map{13}.occur.value and the wrapper has to be stepped over
    -- between finding the entry and picking a side of it.
    it "steps over a wrapper sitting between a map entry and the side of it" $
      followPath (trailOf [StepMapKey (LitUInt 13), StepOccur 0, StepValue]) (TMap [(TInt 13, TInt 9)])
        `shouldBe` [TInt 9]

  describe "Wrappers" $ do
    -- These say something about the schema and nothing about the encoding, so
    -- the term stays where it is while the path moves on.
    it "steps over the wrappers that leave no trace in the term" $
      followPath (trailOf [StepOccur 0, StepGenerator, StepValidator, StepControl, StepArray 0]) (TList [TInt 7])
        `shouldBe` [TInt 7]

    it "descends a tag only when the number matches" $ do
      followPath (trailOf [StepTag 258, StepArray 0]) (TTagged 258 (TList [TInt 7]))
        `shouldBe` [TInt 7]
      followPath (trailOf [StepTag 258, StepArray 0]) (TTagged 259 (TList [TInt 7]))
        `shouldBe` []

    it "returns the term itself for an empty path" $
      followPath (trailOf []) redeemerList `shouldBe` [redeemerList]

  describe "Reading a container" $ do
    it "counts entries whichever length form was used" $ do
      entryCount emptyList `shouldBe` Just 0
      entryCount (TList [TInt 7, TInt 8]) `shouldBe` Just 2
      entryCount oneEntryMap `shouldBe` Just 1
      entryCount (TInt 7) `shouldBe` Nothing

    it "reports the length form, which the specification permits either way" $ do
      isIndefinite (TListI []) `shouldBe` Just True
      isIndefinite (TList []) `shouldBe` Just False
      isIndefinite (TInt 7) `shouldBe` Nothing

-- HELPERS

-- | @9fff@: an empty indefinite array
emptyList :: Term
emptyList = TListI []

-- | @[[0, 0, h'', [0, 0]]]@, the shape of a one element redeemers array.
redeemerList :: Term
redeemerList = TList [TList [TInt 0, TInt 0, TBytes "", TList [TInt 0, TInt 0]]]

-- | @[0, h'']@: the key hash branch of drep.
drepKeyHash :: Term
drepKeyHash = TList [TInt 0, TBytes ""]

-- | @a{5: 9}@, a map with one entry.
oneEntryMap :: Term
oneEntryMap = TMap [(TInt 5, TInt 9)]
