module CborTermSpec (spec) where

import CborTerm (entryCount, followPath, isIndefinite)
import Codec.CBOR.Term (Term (..))
import Obligations (LiteralValue (..), Shape (..), Step (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Choices" $ do
    -- A choice narrows the schema without moving in the term, so the step has
    -- to decide which branch the term took rather than descend into it.
    it "stays on the same term, because a choice picks a branch and not a position" $
      followPath [StepChoice 0 (ShapeArray Nothing)] redeemerList
        `shouldBe` Just redeemerList

    -- Both redeemers branches forbid an empty container. Without this check an
    -- empty array answers for the empty map as well, which is what the corpus
    -- has: five empty arrays and no empty map.
    it "refuses a branch of the wrong kind, so an array does not answer for a map" $
      followPath [StepChoice 1 ShapeMap] emptyList `shouldBe` Nothing

    -- drep is [0, addr_keyhash // 1, script_hash // 2 // 3]. The alternatives
    -- are all arrays, so only the leading uint separates them.
    it "tells discriminated branches apart by the uint they open with" $ do
      followPath [StepChoice 0 (ShapeArray (Just 0))] drepKeyHash
        `shouldBe` Just drepKeyHash
      followPath [StepChoice 2 (ShapeArray (Just 2))] drepKeyHash
        `shouldBe` Nothing

    it "accepts any term for a branch that is not a container of its own" $
      followPath [StepChoice 0 ShapeOther] (TInt 7) `shouldBe` Just (TInt 7)

  describe "Containers" $ do
    it "indexes an array, definite or not" $ do
      followPath [StepArray 1] (TList [TInt 7, TInt 8]) `shouldBe` Just (TInt 8)
      followPath [StepArray 1] (TListI [TInt 7, TInt 8]) `shouldBe` Just (TInt 8)

    it "finds a keyed entry by its key, wherever the sample put it" $
      -- A schema numbers its keyed entries in the order the specification lists
      -- them, and a sample carries only the fields it has, so the two numberings
      -- are unrelated. Looking the key up is what makes them line up.
      followPath [StepMapKey (LitUInt 13), StepValue] (TMap [(TInt 0, TInt 1), (TInt 13, TInt 9)])
        `shouldBe` Just (TInt 9)

    it "finds nothing when the sample leaves an optional key out" $
      followPath [StepMapKey (LitUInt 13), StepValue] (TMap [(TInt 0, TInt 1)])
        `shouldBe` Nothing

    -- A repeated entry stands for every runtime entry at once, so there is no
    -- key to look up and position is all there is.
    it "takes either side of a repeated entry by position" $ do
      followPath [StepMapAt 0, StepKey] oneEntryMap `shouldBe` Just (TInt 5)
      followPath [StepMapAt 0, StepValue] oneEntryMap `shouldBe` Just (TInt 9)

    it "stops at an index the term does not have" $
      followPath [StepArray 2] (TList [TInt 7]) `shouldBe` Nothing

    it "stops when the term is not the kind the step expects" $
      followPath [StepArray 0] oneEntryMap `shouldBe` Nothing

    -- An optional entry is an occurrence wrapped around the keyed entry, so the
    -- path reads map{13}.occur.value and the wrapper has to be stepped over
    -- between finding the entry and picking a side of it.
    it "steps over a wrapper sitting between a map entry and the side of it" $
      followPath [StepMapKey (LitUInt 13), StepOccur 0, StepValue] (TMap [(TInt 13, TInt 9)])
        `shouldBe` Just (TInt 9)

  describe "Wrappers" $ do
    -- These say something about the schema and nothing about the encoding, so
    -- the term stays where it is while the path moves on.
    it "steps over the wrappers that leave no trace in the term" $
      followPath [StepOccur 0, StepGenerator, StepValidator, StepControl, StepArray 0] (TList [TInt 7])
        `shouldBe` Just (TInt 7)

    it "descends a tag only when the number matches" $ do
      followPath [StepTag 258, StepArray 0] (TTagged 258 (TList [TInt 7]))
        `shouldBe` Just (TInt 7)
      followPath [StepTag 258, StepArray 0] (TTagged 259 (TList [TInt 7]))
        `shouldBe` Nothing

    it "returns the term itself for an empty path" $
      followPath [] redeemerList `shouldBe` Just redeemerList

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
