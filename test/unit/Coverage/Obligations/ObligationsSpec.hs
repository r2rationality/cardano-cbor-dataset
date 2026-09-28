module Coverage.Obligations.ObligationsSpec where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (CTreeRoot))
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Obligation
import Coverage.Obligations.Obligations
import Coverage.Obligations.Primitive
import Coverage.Obligations.Shape
import Coverage.Obligations.SpecRule
import Coverage.Obligations.Step
import Coverage.Obligations.Trail
import Data.Map qualified as Map
import Data.Text qualified as T
import Test.Hspec

-- | This specification follows the derivation table in @COVERAGE.md@, one construct at a time,
--   and shows how obligations are created from rules elements.
spec = do
  describe "Literals" $ do
    it "An alternative that can be specified with just a uint" $ do
      branchShapes "network_id"
        `shouldBe` [ShapeLiteral (LitUInt 0), ShapeLiteral (LitUInt 1)]

      -- Denying the value is denying the shape the alternative is found by, so it is stated once at the choice, where
      -- every alternative is in view, and not again under each of them: -1 and 2 are the numbers that no alternative
      -- admits.
      obligationKinds "network_id"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeLiteral (LitUInt 0), ShapeLiteral (LitUInt 1)]),
                            (".", BelowRange 0),
                            (".", AboveRange 1),
                            ("choice[0:=0]", AcceptLiteral (LitUInt 0)),
                            ("choice[1:=1]", AcceptLiteral (LitUInt 1))
                          ]

  describe "Ranges" $ do
    -- A range admits integers only, so a value of another type, such as null, is refused whatever its place.
    it "We produce obligations for low bound / high bound, inside range and type" $ do
      -- vote = 0 .. 2
      obligationKinds "vote"
        `shouldMatchList` [ (".", AcceptBoundary 0),
                            (".", AcceptBoundary 2),
                            (".", InsideRange 0 2),
                            (".", BelowRange 0),
                            (".", AboveRange 2),
                            (".", WrongType PrimInt)
                          ]

    it "We follow references to read a range endpoint" $
      -- positive_coin = 1 .. max_word64, where max_word64 is a rule of its own.
      -- A range endpoint is followed through the reference.
      obligationKinds "positive_coin"
        `shouldMatchList` [ (".", AcceptBoundary 1),
                            (".", AcceptBoundary 18446744073709551615),
                            (".", InsideRange 1 18446744073709551615),
                            (".", BelowRange 1),
                            (".", AboveRange 18446744073709551615),
                            (".", WrongType PrimInt)
                          ]

  describe "Tags" $ do
    it "We check the tag of a tagged rule" $ do
      -- big_uint = #6.2(bounded_bytes), with bounded_bytes = bytes .size (0 .. 64)
      -- Tags 2 and 3 are bignums: CBOR itself requires a byte string inside, so big_uint states nothing about the type
      -- of its payload. CBOR does not limit the length of that byte string, so the 64 bytes are still stated.
      obligationKinds "big_uint"
        `shouldMatchList` [ (".", AcceptTag 2),
                            (".", WrongTag 2),
                            ("tag2.control", AcceptSize 0 64),
                            ("tag2.control", ViolateSizeAbove 64)
                          ]

      -- Tag 258 on the other hand has no such encoding rule, so the payload of a set keeps its obligations, under tag258.
      [path | (path, _) <- obligationKinds "%set<addr_keyhash>", "tag258." `T.isInfixOf` path]
        `shouldSatisfy` (not . null)

    it "We refuse a value without the tag, except for a bignum" $ do
      -- unit_interval = #6.30([uint, uint])
      -- A plain 0 is refused by no obligation under the tag, since none of them can reach it.
      [kind | ("gen", kind) <- obligationKinds "unit_interval"]
        `shouldMatchList` [AcceptTag 30, WrongTag 30, MissingTag 30]
      -- The decoder folds a bignum that fits into a plain integer, so an untagged integer is not a missing tag 2.
      [kind | (_, kind@(MissingTag _)) <- obligationKinds "big_uint"] `shouldBe` []

    it "We check the tag of a tagged map" $
      -- auxiliary_data_map =
      --   #6.259(
      --     { ? 0 : metadata
      --     , ? 1 : [* native_script]
      --     , ? 2 : [* plutus_v1_script]
      --     , ? 3 : [* plutus_v2_script]
      --     , ? 4 : [* plutus_v3_script]
      --     }
      --   )
      obligationKinds "auxiliary_data_map"
        `shouldMatchList` [ (".", AcceptTag 259),
                            (".", WrongTag 259),
                            (".", MissingTag 259),
                            ("tag259", WrongType PrimMap),
                            ("tag259", AcceptDefinite),
                            ("tag259", AcceptIndefinite),
                            ("tag259", TooMany 5),
                            ("tag259", DuplicateKey),
                            ("tag259", NoKeyMatches [LitUInt 0, LitUInt 1, LitUInt 2, LitUInt 3, LitUInt 4]),
                            ("tag259.map{0}.occur0.value", AcceptReference (Name "metadata")),
                            ("tag259.map{0}.occur0.value", WrongReference (Name "metadata")),
                            ("tag259.map{1}.occur0.value", WrongType PrimArray),
                            ("tag259.map{1}.occur0.value", AcceptDefinite),
                            ("tag259.map{1}.occur0.value", AcceptIndefinite),
                            ("tag259.map{1}.occur0.value", AcceptArity 2),
                            ("tag259.map{1}.occur0.value.array[0].repeat0", WrongReference (Name "native_script")),
                            ("tag259.map{2}.occur0.value", WrongType PrimArray),
                            ("tag259.map{2}.occur0.value", AcceptDefinite),
                            ("tag259.map{2}.occur0.value", AcceptIndefinite),
                            ("tag259.map{2}.occur0.value", AcceptArity 2),
                            ("tag259.map{2}.occur0.value.array[0].repeat0", WrongReference (Name "plutus_v1_script")),
                            ("tag259.map{3}.occur0.value", WrongType PrimArray),
                            ("tag259.map{3}.occur0.value", AcceptDefinite),
                            ("tag259.map{3}.occur0.value", AcceptIndefinite),
                            ("tag259.map{3}.occur0.value", AcceptArity 2),
                            ("tag259.map{3}.occur0.value.array[0].repeat0", WrongReference (Name "plutus_v2_script")),
                            ("tag259.map{4}.occur0.value", WrongType PrimArray),
                            ("tag259.map{4}.occur0.value", AcceptDefinite),
                            ("tag259.map{4}.occur0.value", AcceptIndefinite),
                            ("tag259.map{4}.occur0.value", AcceptArity 2),
                            ("tag259.map{4}.occur0.value.array[0].repeat0", WrongReference (Name "plutus_v3_script"))
                          ]

    it "An alternative that is a bignum states only the length of its payload" $
      -- big_int = int / big_uint / big_nint. The last two alternatives are bignums, told apart by their tags 2 and 3.
      -- CBOR requires a byte string inside, so each one states only the 64 bytes of bounded_bytes, and NoTagMatches on
      -- the choice already refuses any other tag. A sample of an accepted size also takes the alternative, so no
      -- AcceptBranch is needed.
      obligationKinds "big_int"
        `shouldMatchList` [ (".", NoBranchMatches [ShapePrimitive PrimInt, ShapeTag 2, ShapeTag 3]),
                            (".", NoTagMatches [2, 3]),
                            ("choice[0:int]", AcceptType PrimInt),
                            ("choice[0:int]", AcceptBoundary (-18446744073709551616)),
                            ("choice[0:int]", AcceptBoundary 18446744073709551615),
                            ("choice[1:tag2].tag2.control", AcceptSize 0 64),
                            ("choice[1:tag2].tag2.control", ViolateSizeAbove 64),
                            ("choice[2:tag3].tag3.control", AcceptSize 0 64),
                            ("choice[2:tag3].tag3.control", ViolateSizeAbove 64)
                          ]

  describe "Choices" $ do
    it "We check that each alternative is covered" $
      -- auxiliary_data = metadata / auxiliary_data_array / auxiliary_data_map.
      obligationKinds "auxiliary_data"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeMap, ShapeArray Nothing, ShapeTag 259]),
                            (".", NoTagMatches [259]),
                            ("choice[0:map]", AcceptBranch),
                            ("choice[0:map]", WrongReference (Name "metadata")),
                            ("choice[1:array]", AcceptBranch),
                            ("choice[1:array]", WrongReference (Name "auxiliary_data_array")),
                            ("choice[2:tag259]", AcceptBranch),
                            ("choice[2:tag259]", WrongReference (Name "auxiliary_data_map"))
                          ]

    it "We produce arity obligations for each alternative of a choice" $
      -- drep = [0, addr_keyhash // 1, script_hash // 2 // 3]
      obligationKinds "drep"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeArray (Just 0), ShapeArray (Just 1), ShapeArray (Just 2), ShapeArray (Just 3)]),
                            (".", NoDiscriminantMatches [0, 1, 2, 3]),
                            ("choice[0:array(0)]", AcceptDefinite),
                            ("choice[0:array(0)]", AcceptIndefinite),
                            ("choice[0:array(0)]", TooFew 2),
                            ("choice[0:array(0)]", TooMany 2),
                            ("choice[0:array(0)].array[1]", WrongReference (Name "addr_keyhash")),
                            ("choice[1:array(1)]", AcceptDefinite),
                            ("choice[1:array(1)]", AcceptIndefinite),
                            ("choice[1:array(1)]", TooFew 2),
                            ("choice[1:array(1)]", TooMany 2),
                            ("choice[1:array(1)].array[1]", WrongReference (Name "script_hash")),
                            ("choice[2:array(2)]", AcceptDefinite),
                            ("choice[2:array(2)]", AcceptIndefinite),
                            ("choice[2:array(2)]", TooMany 1),
                            ("choice[3:array(3)]", AcceptDefinite),
                            ("choice[3:array(3)]", AcceptIndefinite),
                            ("choice[3:array(3)]", TooMany 1)
                          ]

    it "An alternative states no constraint that would erase its own discriminant" $ do
      -- gov_action =
      --   [  parameter_change_action
      --   // hard_fork_initiation_action
      --   // treasury_withdrawals_action
      --   // no_confidence
      --   // update_committee
      --   // new_constitution
      --   // info_action
      --   ]
      --
      -- gov_action = ... / [info_action], with info_action = 6. The 6 tells this alternative apart from the others, so
      -- nothing here may remove it: no WrongType array, no TooFew 1, and nothing about the 6 itself. A sample without
      -- the 6 belongs to no alternative. Only the two length forms and TooMany 1 remain.
      branchKinds "gov_action" "choice[6:array(6)]"
        `shouldMatchList` [ ("choice[6:array(6)]", AcceptDefinite),
                            ("choice[6:array(6)]", AcceptIndefinite),
                            ("choice[6:array(6)]", TooMany 1)
                          ]

      -- parameter_change_action =
      --   (0, gov_action_id/ nil, protocol_param_update, guardrails_script_hash/ nil)
      --
      -- A longer alternative keeps its lower bound: three entries where four are required still open with the
      -- discriminant, so such a sample can still be shown to belong here.
      [kind | ("choice[0:array(0)]", kind) <- branchKinds "gov_action" "choice[0:array(0)]"]
        `shouldMatchList` [AcceptDefinite, AcceptIndefinite, TooFew 4, TooMany 4]

    it "An alternative that is a span of numbers is told apart by where a value falls" $
      -- nonzero_int64 = negative_int64 / positive_int64. The choice refuses the numbers that no alternative admits:
      -- below min_int64, above max_int64, and 0 in the gap between the two alternatives.
      obligationKinds "nonzero_int64"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeRange (-9223372036854775808) (-1), ShapeRange 1 9223372036854775807]),
                            (".", BelowRange (-9223372036854775808)),
                            (".", AboveRange 9223372036854775807),
                            (".", InsideGap (-1) 1),
                            ("choice[0:-9223372036854775808..-1]", AcceptBoundary (-9223372036854775808)),
                            ("choice[0:-9223372036854775808..-1]", AcceptBoundary (-1)),
                            ("choice[0:-9223372036854775808..-1]", InsideRange (-9223372036854775808) (-1)),
                            ("choice[1:1..9223372036854775807]", AcceptBoundary 1),
                            ("choice[1:1..9223372036854775807]", AcceptBoundary 9223372036854775807),
                            ("choice[1:1..9223372036854775807]", InsideRange 1 9223372036854775807)
                          ]

    it "We check a group's discriminant" $
      -- script_pubkey = (0, addr_keyhash)
      obligationKinds "script_pubkey"
        `shouldMatchList` [ ("group[0]", AcceptLiteral (LitUInt 0)),
                            ("group[0]", WrongLiteral (LitUInt 0)),
                            ("group[1]", WrongReference (Name "addr_keyhash"))
                          ]

    it "a constrained branch keeps the shape of what it constrains" $ do
      -- A control narrows what a value may be without making it a different value.
      --
      -- metadatum =
      --    { * metadatum => metadatum } /
      --    [ * metadatum ] /
      --    int /
      --    bytes .size (0 .. 64) /
      --    text .size (0 .. 64)
      --
      -- The last 2 constraints apply to 2 different types and we should have obligations for both.
      obligationKinds "metadatum"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeMap, ShapeArray Nothing, ShapePrimitive PrimInt, ShapePrimitive PrimBytes, ShapePrimitive PrimText]),
                            ("choice[0:map]", AcceptDefinite),
                            ("choice[0:map]", AcceptIndefinite),
                            ("choice[0:map]", AcceptArity 2),
                            ("choice[0:map]", DuplicateKey),
                            ("choice[0:map].map[0].repeat0.key", AcceptReference (Name "metadatum")),
                            ("choice[0:map].map[0].repeat0.key", WrongReference (Name "metadatum")),
                            ("choice[0:map].map[0].repeat0.value", AcceptReference (Name "metadatum")),
                            ("choice[0:map].map[0].repeat0.value", WrongReference (Name "metadatum")),
                            ("choice[1:array]", AcceptDefinite),
                            ("choice[1:array]", AcceptIndefinite),
                            ("choice[1:array]", AcceptArity 2),
                            ("choice[1:array].array[0].repeat0", WrongReference (Name "metadatum")),
                            ("choice[2:int]", AcceptType PrimInt),
                            ("choice[2:int]", AcceptBoundary (-18446744073709551616)),
                            ("choice[2:int]", AcceptBoundary 18446744073709551615),
                            ("choice[3:bytes].control", AcceptSize 0 64),
                            ("choice[3:bytes].control", ViolateSizeAbove 64),
                            ("choice[3:bytes].control", AcceptType PrimBytes),
                            ("choice[4:text].control", AcceptSize 0 64),
                            ("choice[4:text].control", ViolateSizeAbove 64),
                            ("choice[4:text].control", AcceptType PrimText)
                          ]

    it "We support ambiguous choices" $ do
      -- account_balance_interval =
      --      [inclusive_lower_bound : coin, exclusive_upper_bound : coin / nil]
      --    / [inclusive_lower_bound : coin / nil, exclusive_upper_bound : coin]
      --    / coin
      --
      -- (coin = uint)
      --
      -- The two array alternatives overlap: [coin, coin] is valid under both.
      --
      -- What the two jointly admit is still exact, and it is stated at the choice instead: one accept per combination
      -- of element shapes, and one rejection for a two element array that is none of them. Without that last one
      -- [nil, nil] would be owed by nothing, since it is an array of the right length whose every element fits the
      -- branch shapes.
      dijkstraObligationKinds "account_balance_interval"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeArray Nothing, ShapeArray Nothing, ShapePrimitive PrimUInt]),
                            (".", AcceptElements [ShapePrimitive PrimUInt, ShapePrimitive PrimUInt]),
                            (".", AcceptElements [ShapePrimitive PrimUInt, ShapePrimitive PrimNil]),
                            (".", AcceptElements [ShapePrimitive PrimNil, ShapePrimitive PrimUInt]),
                            ( ".",
                              NoElementsMatch
                                [ [ShapePrimitive PrimUInt, ShapePrimitive PrimUInt],
                                  [ShapePrimitive PrimUInt, ShapePrimitive PrimNil],
                                  [ShapePrimitive PrimNil, ShapePrimitive PrimUInt]
                                ]
                            ),
                            (".", WrongCombination [ShapePrimitive PrimNil, ShapePrimitive PrimNil]),
                            ("choice[0:ambiguous-array]", AcceptDefinite),
                            ("choice[0:ambiguous-array]", AcceptIndefinite),
                            ("choice[0:ambiguous-array]", TooFew 2),
                            ("choice[0:ambiguous-array]", TooMany 2),
                            ("choice[0:ambiguous-array].array[1].value", NoBranchMatches [ShapePrimitive PrimUInt, ShapePrimitive PrimNil]),
                            ("choice[0:ambiguous-array].array[1].value.choice[0:uint]", AcceptBranch),
                            ("choice[0:ambiguous-array].array[1].value.choice[1:nil]", AcceptType PrimNil),
                            ("choice[1:ambiguous-array]", AcceptDefinite),
                            ("choice[1:ambiguous-array]", AcceptIndefinite),
                            ("choice[1:ambiguous-array]", TooFew 2),
                            ("choice[1:ambiguous-array]", TooMany 2),
                            ("choice[1:ambiguous-array].array[0].value", NoBranchMatches [ShapePrimitive PrimUInt, ShapePrimitive PrimNil]),
                            ("choice[1:ambiguous-array].array[0].value.choice[0:uint]", AcceptBranch),
                            ("choice[1:ambiguous-array].array[0].value.choice[1:nil]", AcceptType PrimNil),
                            ("choice[2:uint]", AcceptBranch)
                          ]

    it "records the shape of each alternative, marking the overlapping ones" $
      dijkstraBranchShapes "account_balance_interval"
        `shouldBe` [ShapeAmbiguous (ShapeArray Nothing), ShapeAmbiguous (ShapeArray Nothing), ShapePrimitive PrimUInt]

    it "We check that a value of the same-shape references is valid for one of them" $
      -- guards = nonempty_set<addr_keyhash> / nonempty_oset<credential>
      --
      -- Both alternatives are a tagged or untagged array, so nothing is stated under them. A value of that shape must
      -- still be a valid value of one of the two rules.
      [kind | (".", kind@(NoReferenceMatches _ _)) <- dijkstraObligationKinds "guards"]
        `shouldBe` [ NoReferenceMatches
                       (ShapeAnyOf [ShapeTag 258, ShapeArray Nothing])
                       [Name "%nonempty_set<addr_keyhash>", Name "%nonempty_oset<credential>"]
                   ]

    it "We check choices in an array" $
      -- datum_option = [0, hash32 // 1, data]
      obligationKinds "datum_option"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeArray (Just 0), ShapeArray (Just 1)]),
                            (".", NoDiscriminantMatches [0, 1]),
                            ("choice[0:array(0)]", AcceptDefinite),
                            ("choice[0:array(0)]", AcceptIndefinite),
                            ("choice[0:array(0)]", TooFew 2),
                            ("choice[0:array(0)]", TooMany 2),
                            ("choice[0:array(0)].array[1]", WrongReference (Name "hash32")),
                            ("choice[1:array(1)]", AcceptDefinite),
                            ("choice[1:array(1)]", AcceptIndefinite),
                            ("choice[1:array(1)]", TooFew 2),
                            ("choice[1:array(1)]", TooMany 2),
                            ("choice[1:array(1)].array[1]", WrongReference (Name "data"))
                          ]

  describe "Arrays" $ do
    -- pool_registration_cert = (3, pool_params), and the Dijkstra pool_params is
    --   (operator, vrf_keyhash, ? bls_key, pledge, cost, margin, reward_account, pool_owners, relays, pool_metadata)
    -- so the certificate has 11 elements with bls_key and 10 without it.
    it "An optional element before others gives each of them its position in the full array" $
      ordNub
        [ T.takeWhile (/= ']') rest
        | (path, _) <- dijkstraObligationKinds "certificate",
          Just rest <- [T.stripPrefix "choice[1:array(3)].array[" path]
        ]
        `shouldBe` ["1", "2", "3/11", "4/11", "5/11", "6/11", "7/11", "8/11", "9/11", "10/11"]

    it "We check both the arity and the definiteness vs indefiniteness of the encoding" $
      -- vkeywitness = [vkey, signature]
      obligationKinds "vkeywitness"
        `shouldMatchList` [ (".", WrongType PrimArray),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            (".", TooFew 2),
                            (".", TooMany 2),
                            ("array[0]", WrongReference (Name "vkey")),
                            ("array[1]", WrongReference (Name "signature"))
                          ]

    it "We check an array arity when there are optional entries" $
      -- alonzo_transaction_output = [address, amount : value, ? datum_hash : hash32].
      obligationKinds "alonzo_transaction_output"
        `shouldMatchList` [ (".", WrongType PrimArray),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            (".", AcceptArity 2),
                            (".", AcceptArity 3),
                            (".", TooFew 2),
                            (".", TooMany 3),
                            ("array[0]", WrongReference (Name "address")),
                            ("array[1].value", WrongReference (Name "value")),
                            ("array[2].occur0.value", WrongReference (Name "hash32"))
                          ]

    it "A fixed-length array states no accepted length" $
      -- transaction_input = [transaction_id : transaction_id, index : uint .size 2].
      -- The array has a fixed length of 2. This will be validated by the AcceptDefinite obligation for example.
      -- (because it cannot be fewer than 2 and more than 2)
      obligationKinds "transaction_input"
        `shouldMatchList` [ (".", WrongType PrimArray),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            (".", TooFew 2),
                            (".", TooMany 2),
                            ("array[0].value", WrongReference (Name "transaction_id")),
                            ("array[1].value.control", AcceptBoundary 65535),
                            ("array[1].value.control", ViolateHighBound 65535),
                            ("array[1].value.control", AcceptType PrimUInt),
                            ("array[1].value.control", WrongType PrimUInt)
                          ]

  describe "Maps" $ do
    it "We check repeated entries arities in a map + definite vs indefinite encoding" $
      -- withdrawals = {+ reward_account => coin}
      obligationKinds "withdrawals"
        `shouldMatchList` [ (".", WrongType PrimMap),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            (".", TooFew 1),
                            (".", AcceptArity 2),
                            (".", DuplicateKey),
                            ("map[0].repeat1.key", WrongReference (Name "reward_account")),
                            ("map[0].repeat1.value", WrongReference (Name "coin"))
                          ]

    it "We support maps with optional, then open keys" $
      -- cost_models =
      --   {? 0 : [* int64], ? 1 : [* int64], ? 2 : [* int64], * 3 .. 255 => [* int64]}
      obligationKinds "cost_models"
        `shouldMatchList` [ (".", WrongType PrimMap),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            (".", AcceptArity 5),
                            (".", DuplicateKey),
                            ("map{0}.occur0.value", WrongType PrimArray),
                            ("map{0}.occur0.value", AcceptDefinite),
                            ("map{0}.occur0.value", AcceptIndefinite),
                            ("map{0}.occur0.value", AcceptArity 2),
                            ("map{0}.occur0.value.array[0].repeat0", AcceptBoundary (-9223372036854775808)),
                            ("map{0}.occur0.value.array[0].repeat0", AcceptBoundary 9223372036854775807),
                            ("map{0}.occur0.value.array[0].repeat0", InsideRange (-9223372036854775808) 9223372036854775807),
                            ("map{0}.occur0.value.array[0].repeat0", BelowRange (-9223372036854775808)),
                            ("map{0}.occur0.value.array[0].repeat0", AboveRange 9223372036854775807),
                            ("map{0}.occur0.value.array[0].repeat0", WrongType PrimInt),
                            ("map{1}.occur0.value", WrongType PrimArray),
                            ("map{1}.occur0.value", AcceptDefinite),
                            ("map{1}.occur0.value", AcceptIndefinite),
                            ("map{1}.occur0.value", AcceptArity 2),
                            ("map{1}.occur0.value.array[0].repeat0", AcceptBoundary (-9223372036854775808)),
                            ("map{1}.occur0.value.array[0].repeat0", AcceptBoundary 9223372036854775807),
                            ("map{1}.occur0.value.array[0].repeat0", InsideRange (-9223372036854775808) 9223372036854775807),
                            ("map{1}.occur0.value.array[0].repeat0", BelowRange (-9223372036854775808)),
                            ("map{1}.occur0.value.array[0].repeat0", AboveRange 9223372036854775807),
                            ("map{1}.occur0.value.array[0].repeat0", WrongType PrimInt),
                            ("map{2}.occur0.value", WrongType PrimArray),
                            ("map{2}.occur0.value", AcceptDefinite),
                            ("map{2}.occur0.value", AcceptIndefinite),
                            ("map{2}.occur0.value", AcceptArity 2),
                            ("map{2}.occur0.value.array[0].repeat0", AcceptBoundary (-9223372036854775808)),
                            ("map{2}.occur0.value.array[0].repeat0", AcceptBoundary 9223372036854775807),
                            ("map{2}.occur0.value.array[0].repeat0", InsideRange (-9223372036854775808) 9223372036854775807),
                            ("map{2}.occur0.value.array[0].repeat0", BelowRange (-9223372036854775808)),
                            ("map{2}.occur0.value.array[0].repeat0", AboveRange 9223372036854775807),
                            ("map{2}.occur0.value.array[0].repeat0", WrongType PrimInt),
                            -- The keys 0, 1 and 2 belong to the siblings, but a negative key is under 3 too, and no
                            -- sibling claims it.
                            ("map[3?0,1,2].repeat0.key", AcceptBoundary 3),
                            ("map[3?0,1,2].repeat0.key", AcceptBoundary 255),
                            ("map[3?0,1,2].repeat0.key", InsideRange 3 255),
                            ("map[3?0,1,2].repeat0.key", BelowRange 3),
                            ("map[3?0,1,2].repeat0.key", AboveRange 255),
                            ("map[3?0,1,2].repeat0.key", WrongType PrimInt),
                            ("map[3?0,1,2].repeat0.value", WrongType PrimArray),
                            ("map[3?0,1,2].repeat0.value", AcceptDefinite),
                            ("map[3?0,1,2].repeat0.value", AcceptIndefinite),
                            ("map[3?0,1,2].repeat0.value", AcceptArity 2),
                            ("map[3?0,1,2].repeat0.value.array[0].repeat0", AcceptBoundary (-9223372036854775808)),
                            ("map[3?0,1,2].repeat0.value.array[0].repeat0", AcceptBoundary 9223372036854775807),
                            ("map[3?0,1,2].repeat0.value.array[0].repeat0", InsideRange (-9223372036854775808) 9223372036854775807),
                            ("map[3?0,1,2].repeat0.value.array[0].repeat0", BelowRange (-9223372036854775808)),
                            ("map[3?0,1,2].repeat0.value.array[0].repeat0", AboveRange 9223372036854775807),
                            ("map[3?0,1,2].repeat0.value.array[0].repeat0", WrongType PrimInt)
                          ]

    it "We support both the required and optional keys" $
      do
        -- babbage_transaction_output =
        --   {   0 : address
        --   ,   1 : value
        --   , ? 2 : datum_option ; new
        --   , ? 3 : script_ref   ; new
        --   }
        obligationKinds "babbage_transaction_output"
        `shouldMatchList` [ (".", WrongType PrimMap),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            -- Two keys required and two optional, so the length is a range: 2 to 4.
                            (".", AcceptArity 2),
                            (".", AcceptArity 3),
                            (".", AcceptArity 4),
                            (".", TooFew 2),
                            (".", TooMany 4),
                            (".", DuplicateKey),
                            -- Keys 2 and 3 are optional, so nothing requires them, but a key outside the four named
                            -- still has to fail.
                            (".", NoKeyMatches [LitUInt 0, LitUInt 1, LitUInt 2, LitUInt 3]),
                            ("map{0}.key", AcceptRequiredKey (LitUInt 0)),
                            ("map{0}.key", MissingRequiredKey (LitUInt 0)),
                            ("map{1}.key", AcceptRequiredKey (LitUInt 1)),
                            ("map{1}.key", MissingRequiredKey (LitUInt 1)),
                            ("map{0}.value", WrongReference (Name "address")),
                            ("map{1}.value", WrongReference (Name "value")),
                            ("map{2}.occur0.value", AcceptReference (Name "datum_option")),
                            ("map{2}.occur0.value", WrongReference (Name "datum_option")),
                            ("map{3}.occur0.value", AcceptReference (Name "script_ref")),
                            ("map{3}.occur0.value", WrongReference (Name "script_ref")),
                            -- script_ref has no corpus of its own, so its obligations are stated here too, where a
                            -- sample actually carries one.
                            ("map{3}.occur0.value", AcceptTag 24),
                            ("map{3}.occur0.value", WrongTag 24),
                            ("map{3}.occur0.value", MissingTag 24),
                            ("map{3}.occur0.value.tag24.control", AcceptCbor),
                            ("map{3}.occur0.value.tag24.control", ViolateCbor),
                            ("map{3}.occur0.value.tag24.control", WrongEmbedded (Name "script")),
                            ("map{3}.occur0.value.tag24.control", AcceptType PrimBytes),
                            ("map{3}.occur0.value.tag24.control", WrongType PrimBytes)
                          ]

    it "We check a map of maps at both levels, including the repeated entries" $
      -- mint = multiasset<nonzero_int64>
      --      = {+ policy_id => {+ asset_name => nonzero_int64}}
      --
      -- Both levels are open (+), so each creates obligations for its arity.
      -- Then we create obligations only for the first entry of the outer map.
      obligationKinds "mint"
        `shouldMatchList` [ (".", WrongType PrimMap),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            (".", TooFew 1),
                            (".", AcceptArity 2),
                            (".", DuplicateKey),
                            ("map[0].repeat1.key", WrongReference (Name "policy_id")),
                            ("map[0].repeat1.value", WrongType PrimMap),
                            ("map[0].repeat1.value", AcceptDefinite),
                            ("map[0].repeat1.value", AcceptIndefinite),
                            ("map[0].repeat1.value", TooFew 1),
                            ("map[0].repeat1.value", AcceptArity 2),
                            ("map[0].repeat1.value", DuplicateKey),
                            ("map[0].repeat1.value.map[0].repeat1.key", WrongReference (Name "asset_name")),
                            ("map[0].repeat1.value.map[0].repeat1.value", WrongReference (Name "nonzero_int64"))
                          ]

    it "We check a large keyed map with required and optional entries" $
      -- transaction_body =
      --   {   0  : set<transaction_input>
      --   ,   1  : [* transaction_output]
      --   ,   2  : coin                            ; fee
      --   , ? 3  : slot                            ; time to live
      --   , ? 4  : certificates
      --   , ? 5  : withdrawals
      --   , ? 7  : auxiliary_data_hash
      --   , ? 8  : slot                            ; validity interval start
      --   , ? 9  : mint
      --   , ? 11 : script_data_hash
      --   , ? 13 : nonempty_set<transaction_input> ; collateral
      --   , ? 14 : required_signers
      --   , ? 15 : network_id
      --   , ? 16 : transaction_output              ; collateral return
      --   , ? 17 : coin                            ; total collateral
      --   , ? 18 : nonempty_set<transaction_input> ; reference inputs
      --   , ? 19 : voting_procedures
      --   , ? 20 : proposal_procedures
      --   , ? 21 : coin                            ; current treasury value
      --   , ? 22 : positive_coin                   ; donation
      --   }
      --
      -- transaction_body is the biggest map in the specification: three required keys, seventeen optional ones, and a
      -- value at every entry that is a rule of its own.
      --
      -- Fields holding sets are folded in and obligations generated from the result.
      [entry | entry@(path, _) <- obligationKinds "transaction_body", not (T.isInfixOf "validator" path)]
        `shouldMatchList` [ (".", WrongType PrimMap),
                            (".", AcceptDefinite),
                            (".", AcceptIndefinite),
                            (".", TooFew 3),
                            (".", TooMany 20),
                            (".", DuplicateKey),
                            (".", NoKeyMatches [LitUInt 0, LitUInt 1, LitUInt 2, LitUInt 3, LitUInt 4, LitUInt 5, LitUInt 7, LitUInt 8, LitUInt 9, LitUInt 11, LitUInt 13, LitUInt 14, LitUInt 15, LitUInt 16, LitUInt 17, LitUInt 18, LitUInt 19, LitUInt 20, LitUInt 21, LitUInt 22]),
                            ("map{0}.key", AcceptRequiredKey (LitUInt 0)),
                            ("map{0}.key", MissingRequiredKey (LitUInt 0)),
                            ("map{1}.key", AcceptRequiredKey (LitUInt 1)),
                            ("map{1}.key", MissingRequiredKey (LitUInt 1)),
                            ("map{2}.key", AcceptRequiredKey (LitUInt 2)),
                            ("map{2}.key", MissingRequiredKey (LitUInt 2)),
                            ("map{0}.value", ValidatorSucceeds),
                            ("map{0}.value", ValidatorFails),
                            ("map{1}.value", WrongType PrimArray),
                            ("map{1}.value", AcceptDefinite),
                            ("map{1}.value", AcceptIndefinite),
                            ("map{1}.value", AcceptArity 2),
                            ("map{1}.value.array[0].repeat0", WrongReference (Name "transaction_output")),
                            ("map{2}.value", WrongReference (Name "coin")),
                            ("map{3}.occur0.value", AcceptReference (Name "slot")),
                            ("map{3}.occur0.value", WrongReference (Name "slot")),
                            ("map{4}.occur0.value", AcceptReference (Name "certificates")),
                            ("map{4}.occur0.value", WrongReference (Name "certificates")),
                            ("map{5}.occur0.value", AcceptReference (Name "withdrawals")),
                            ("map{5}.occur0.value", WrongReference (Name "withdrawals")),
                            ("map{7}.occur0.value", AcceptReference (Name "auxiliary_data_hash")),
                            ("map{7}.occur0.value", WrongReference (Name "auxiliary_data_hash")),
                            ("map{8}.occur0.value", AcceptReference (Name "slot")),
                            ("map{8}.occur0.value", WrongReference (Name "slot")),
                            ("map{9}.occur0.value", AcceptReference (Name "mint")),
                            ("map{9}.occur0.value", WrongReference (Name "mint")),
                            ("map{11}.occur0.value", AcceptReference (Name "script_data_hash")),
                            ("map{11}.occur0.value", WrongReference (Name "script_data_hash")),
                            ("map{13}.occur0.value", AcceptPresent),
                            ("map{13}.occur0.value", ValidatorSucceeds),
                            ("map{13}.occur0.value", ValidatorFails),
                            ("map{14}.occur0.value", AcceptPresent),
                            ("map{14}.occur0.value", ValidatorSucceeds),
                            ("map{14}.occur0.value", ValidatorFails),
                            ("map{15}.occur0.value", AcceptReference (Name "network_id")),
                            ("map{15}.occur0.value", WrongReference (Name "network_id")),
                            ("map{16}.occur0.value", AcceptReference (Name "transaction_output")),
                            ("map{16}.occur0.value", WrongReference (Name "transaction_output")),
                            ("map{17}.occur0.value", AcceptReference (Name "coin")),
                            ("map{17}.occur0.value", WrongReference (Name "coin")),
                            ("map{18}.occur0.value", AcceptPresent),
                            ("map{18}.occur0.value", ValidatorSucceeds),
                            ("map{18}.occur0.value", ValidatorFails),
                            ("map{19}.occur0.value", AcceptReference (Name "voting_procedures")),
                            ("map{19}.occur0.value", WrongReference (Name "voting_procedures")),
                            ("map{20}.occur0.value", AcceptReference (Name "proposal_procedures")),
                            ("map{20}.occur0.value", WrongReference (Name "proposal_procedures")),
                            ("map{21}.occur0.value", AcceptReference (Name "coin")),
                            ("map{21}.occur0.value", WrongReference (Name "coin")),
                            ("map{22}.occur0.value", AcceptReference (Name "positive_coin")),
                            ("map{22}.occur0.value", WrongReference (Name "positive_coin"))
                          ]

  describe "Groups" $ do
    it "Group definitions are flattened in order to determine the expected arity" $
      -- certificate =
      --   [  account_registration_cert
      --   // account_unregistration_cert
      --   // delegation_to_stake_pool_cert
      --   // pool_registration_cert
      --   // pool_retirement_cert
      --   // account_registration_deposit_cert
      --   // account_unregistration_deposit_cert
      --   // delegation_to_drep_cert
      --   // delegation_to_stake_pool_and_drep_cert
      --   // account_registration_delegation_to_stake_pool_cert
      --   // account_registration_delegation_to_drep_cert
      --   // account_registration_delegation_to_stake_pool_and_drep_cert
      --   // committee_authorization_cert
      --   // committee_resignation_cert
      --   // drep_registration_cert
      --   // drep_unregistration_cert
      --   // drep_update_cert
      --   ]
      --
      -- Each certificate alternative splices a group into the array
      -- For each group we much check its arity, so there are 3 obligations: too low, exact, too high.
      --
      -- Some groups are nested: for example pool_registration_cert is [3, pool_params] where pool_params is a group of
      -- 9 elements. This makes a total of 10 elements.
      [ (path, kind)
      | (path, kind) <- obligationKinds "certificate",
        isArity kind,
        not (T.isInfixOf "." path)
      ]
        `shouldBe` concatMap
          arityObligations
          [ ("choice[0:array(0)]", 2),
            ("choice[1:array(1)]", 2),
            ("choice[2:array(2)]", 3),
            ("choice[3:array(3)]", 10),
            ("choice[4:array(4)]", 3),
            ("choice[5:array(7)]", 3),
            ("choice[6:array(8)]", 3),
            ("choice[7:array(9)]", 3),
            ("choice[8:array(10)]", 4),
            ("choice[9:array(11)]", 4),
            ("choice[10:array(12)]", 4),
            ("choice[11:array(13)]", 5),
            ("choice[12:array(14)]", 3),
            ("choice[13:array(15)]", 3),
            ("choice[14:array(16)]", 4),
            ("choice[15:array(17)]", 3),
            ("choice[16:array(18)]", 3)
          ]

    it "A spliced group's fields are addressed by where they land in the bytes" $ do
      -- certificate choice[3] is pool_registration_cert = [3, pool_params], and pool_params is itself a group of nine.
      -- On the wire that is one flat array of ten, so the operator sits at index 1, not one level down in a group of
      -- its own. Addressing has to agree with the arity above, which already counts the splice.
      [(path, name) | (path, WrongReference name) <- branchKinds "certificate" "choice[3:array(3)]"]
        `shouldMatchList` [ ("choice[3:array(3)].array[1].value", Name "pool_keyhash"),
                            ("choice[3:array(3)].array[2].value", Name "vrf_keyhash"),
                            ("choice[3:array(3)].array[3].value", Name "coin"),
                            ("choice[3:array(3)].array[4].value", Name "coin"),
                            ("choice[3:array(3)].array[5].value", Name "unit_interval"),
                            ("choice[3:array(3)].array[6].value", Name "reward_account"),
                            (owners <> ".choice[0:tag258].tag258.array[0].repeat0", Name "addr_keyhash"),
                            (owners <> ".choice[1:array].array[0].repeat0", Name "addr_keyhash"),
                            ("choice[3:array(3)].array[8].value.array[0].repeat0", Name "relay"),
                            ("choice[3:array(3)].array[9].value.choice[0:array]", Name "pool_metadata")
                          ]

      -- The arity above says the same thing in the other direction: ten, not two, because the nine spliced fields are
      -- counted where they land.
      [kind | ("choice[3:array(3)]", kind) <- obligationKinds "certificate", isArity kind]
        `shouldBe` [TooFew 10, TooMany 10]

  describe "References" $ do
    it "Ranges described with intervals are folded" $ do
      -- nonzero_int64 = negative_int64 / positive_int64
      map (foldedRule . Name) ["negative_int64", "positive_int64"]
        `shouldBe` [True, True]
      foldedRule (Name "nonzero_int64") `shouldBe` False

      obligationKinds "nonzero_int64"
        `shouldMatchList` [ ( ".",
                              NoBranchMatches
                                [ ShapeRange (-9223372036854775808) (-1),
                                  ShapeRange 1 9223372036854775807
                                ]
                            ),
                            (".", BelowRange (-9223372036854775808)),
                            (".", AboveRange 9223372036854775807),
                            (".", InsideGap (-1) 1),
                            -- Each half is folded in, so its span is stated on the branch that carries it. Neither half
                            -- denies a value outside its own span: that is how the branch was picked, and what no
                            -- branch admits is stated once above.
                            ("choice[0:-9223372036854775808..-1]", AcceptBoundary (-9223372036854775808)),
                            ("choice[0:-9223372036854775808..-1]", AcceptBoundary (-1)),
                            ("choice[0:-9223372036854775808..-1]", InsideRange (-9223372036854775808) (-1)),
                            ("choice[1:1..9223372036854775807]", AcceptBoundary 1),
                            ("choice[1:1..9223372036854775807]", AcceptBoundary 9223372036854775807),
                            ("choice[1:1..9223372036854775807]", InsideRange 1 9223372036854775807)
                          ]

    it "A range nothing decodes alone is stated where it is read" $ do
      -- redeemer_tag = 0 .. 5
      --
      -- No type decodes a bare redeemer so we create obligations where that reference is used
      [entry | entry@("array[0].value", _) <- obligationKinds "redeemer"]
        `shouldMatchList` [ ("array[0].value", WrongReference (Name "redeemer_tag")),
                            ("array[0].value", AcceptBoundary 0),
                            ("array[0].value", AcceptBoundary 5),
                            ("array[0].value", InsideRange 0 5),
                            ("array[0].value", BelowRange 0),
                            ("array[0].value", AboveRange 5),
                            ("array[0].value", WrongType PrimInt)
                          ]

    it "Some references are folded when there are no spec entries for them" $ do
      -- transaction_witness_set =
      --   { ? 0 : nonempty_list<vkeywitness>
      --   , ? 1 : nonempty_list<native_script>
      --   , ? 2 : nonempty_list<bootstrap_witness>
      --   , ? 3 : nonempty_set<plutus_v1_script>
      --   , ? 4 : nonempty_list<plutus_data>
      --   , ? 5 : redeemers
      --   , ? 6 : nonempty_set<plutus_v2_script>
      --   , ? 7 : nonempty_set<plutus_v3_script>
      --   }
      --
      -- There are no spec entries for the individual fields of the transaction_witness_set that are nonempty lists or sets.
      -- so those references are folded.
      [name | (_, kind) <- obligationKinds "transaction_witness_set", name <- referenced kind]
        `shouldSatisfy` (not . any (T.isPrefixOf "%nonempty_"))

      -- What survives is that the entry may be absent.
      [path | (path, AcceptPresent) <- obligationKinds "transaction_witness_set"]
        `shouldBe` map
          (\index -> "map{" <> show index <> "}.occur0.value")
          [0, 1, 2, 3, 4, 6, 7 :: Int]

      -- A folded list states what the generic rule asks for: nonempty_list<a> = #6.258([+ a]) / [+ a]. Each branch
      -- refuses an empty array with TooFew 1, and checks its element at two positions.
      kindsBelow "transaction_witness_set" "map{0}.occur0.value"
        `shouldMatchList` [ (".", AcceptPresent),
                            (".", NoBranchMatches [ShapeTag 258, ShapeArray Nothing]),
                            (".", NoTagMatches [258]),
                            ("choice[0:tag258].tag258", WrongType PrimArray),
                            ("choice[0:tag258].tag258", TooFew 1),
                            ("choice[0:tag258].tag258", AcceptArity 2),
                            ("choice[0:tag258].tag258", AcceptDefinite),
                            ("choice[0:tag258].tag258", AcceptIndefinite),
                            ("choice[0:tag258].tag258.array[0].repeat1", WrongReference (Name "vkeywitness")),
                            ("choice[1:array]", TooFew 1),
                            ("choice[1:array]", AcceptArity 2),
                            ("choice[1:array]", AcceptDefinite),
                            ("choice[1:array]", AcceptIndefinite),
                            ("choice[1:array].array[0].repeat1", WrongReference (Name "vkeywitness"))
                          ]

      -- A folded set states the same, under the validator that refuses a repeated element.
      kindsBelow "transaction_witness_set" "map{3}.occur0.value"
        `shouldMatchList` [ (".", AcceptPresent),
                            (".", ValidatorSucceeds),
                            (".", ValidatorFails),
                            ("validator.gen", NoBranchMatches [ShapeTag 258, ShapeArray Nothing]),
                            ("validator.gen", NoTagMatches [258]),
                            ("validator.gen.choice[0:tag258].tag258", WrongType PrimArray),
                            ("validator.gen.choice[0:tag258].tag258", TooFew 1),
                            ("validator.gen.choice[0:tag258].tag258", AcceptArity 2),
                            ("validator.gen.choice[0:tag258].tag258", AcceptDefinite),
                            ("validator.gen.choice[0:tag258].tag258", AcceptIndefinite),
                            ("validator.gen.choice[0:tag258].tag258.array[0].repeat1", WrongReference (Name "plutus_v1_script")),
                            ("validator.gen.choice[1:array]", TooFew 1),
                            ("validator.gen.choice[1:array]", AcceptArity 2),
                            ("validator.gen.choice[1:array]", AcceptDefinite),
                            ("validator.gen.choice[1:array]", AcceptIndefinite),
                            ("validator.gen.choice[1:array].array[0].repeat1", WrongReference (Name "plutus_v1_script"))
                          ]

  describe "Controls" $ do
    it ".size produces obligations to check the exact size" $
      -- vrf_vkey = bytes .size 32
      obligationKinds "vrf_vkey"
        `shouldMatchList` [ ("control", AcceptSize 32 32),
                            ("control", ViolateSizeBelow 32),
                            ("control", ViolateSizeAbove 32),
                            ("control", AcceptType PrimBytes),
                            ("control", WrongType PrimBytes)
                          ]

    it ".size with bounds produces obligations to check the bounds" $
      -- url = text .size (0 .. 128)
      obligationKinds "url"
        `shouldMatchList` [ ("control", AcceptSize 0 128),
                            ("control", ViolateSizeAbove 128),
                            ("control", AcceptType PrimText),
                            ("control", WrongType PrimText)
                          ]

    it ".size on a number produces obligations to check the number as a ceiling on the value" $
      -- epoch_interval = uint .size 4, so the value has to fit in four bytes.
      -- On a number .size is not a length but a ceiling, and the ceiling is 256^n - 1. Chosen over a two byte field so
      -- the conversion is visible: port below says the same thing as `.le 65535` and would otherwise read identically.
      obligationKinds "epoch_interval"
        `shouldMatchList` [ ("control", AcceptBoundary 4294967295),
                            ("control", ViolateHighBound 4294967295),
                            ("control", AcceptType PrimUInt),
                            ("control", WrongType PrimUInt)
                          ]

    it ".le check the maximum size of a number" $
      -- port = uint .le 65535. The only .le in the specification. It states the same value set as `uint .size 2`, and
      -- normalising both spellings to one obligation is the point: what a decoder owes should not depend on which the
      -- specification author wrote.
      obligationKinds "port"
        `shouldMatchList` [ ("control", AcceptBoundary 65535),
                            ("control", ViolateHighBound 65535),
                            ("control", AcceptType PrimUInt),
                            ("control", WrongType PrimUInt)
                          ]

    it ".cbor checks the contents of an embedded cbor control" $
      -- script_ref = #6.24(bytes .cbor script). The tag around bytes whose contents are themselves CBOR is also
      -- checked.
      obligationKinds "script_ref"
        `shouldMatchList` [ (".", AcceptTag 24),
                            (".", WrongTag 24),
                            (".", MissingTag 24),
                            ("tag24.control", AcceptCbor),
                            ("tag24.control", ViolateCbor),
                            ("tag24.control", WrongEmbedded (Name "script")),
                            ("tag24.control", AcceptType PrimBytes),
                            ("tag24.control", WrongType PrimBytes)
                          ]

    it "Every control operator of both eras is known" $ do
      -- An unknown control operator stops the derivation with an error, so deriving every obligation of both eras is
      -- enough to show that each control is known.
      length (allKinds conwayRoot) `shouldSatisfy` (> 0)
      length (allKinds dijkstraRoot) `shouldSatisfy` (> 0)

  describe "Validators" $ do
    it "We check the uniqueness of a set, but succeeding or failing its validator" $
      obligationKinds "%set<transaction_input>"
        `shouldMatchList` [ (".", ValidatorSucceeds),
                            (".", ValidatorFails),
                            ("validator.gen", NoBranchMatches [ShapeTag 258, ShapeArray Nothing]),
                            ("validator.gen", NoTagMatches [258]),
                            ("validator.gen.choice[0:tag258].tag258", WrongType PrimArray),
                            ("validator.gen.choice[0:tag258].tag258", AcceptDefinite),
                            ("validator.gen.choice[0:tag258].tag258", AcceptIndefinite),
                            ("validator.gen.choice[0:tag258].tag258", AcceptArity 2),
                            ("validator.gen.choice[0:tag258].tag258.array[0].repeat0", WrongReference (Name "transaction_input")),
                            ("validator.gen.choice[1:array]", AcceptDefinite),
                            ("validator.gen.choice[1:array]", AcceptIndefinite),
                            ("validator.gen.choice[1:array]", AcceptArity 2),
                            ("validator.gen.choice[1:array].array[0].repeat0", WrongReference (Name "transaction_input"))
                          ]

    it "There is no validator check for a simple list (just an arity check)" $
      obligationKinds "%nonempty_list<vkeywitness>"
        `shouldMatchList` [ (".", NoBranchMatches [ShapeTag 258, ShapeArray Nothing]),
                            (".", NoTagMatches [258]),
                            ("choice[0:tag258].tag258", WrongType PrimArray),
                            ("choice[0:tag258].tag258", AcceptDefinite),
                            ("choice[0:tag258].tag258", AcceptIndefinite),
                            ("choice[0:tag258].tag258", TooFew 1),
                            ("choice[0:tag258].tag258", AcceptArity 2),
                            ("choice[0:tag258].tag258.array[0].repeat1", WrongReference (Name "vkeywitness")),
                            ("choice[1:array]", AcceptDefinite),
                            ("choice[1:array]", AcceptIndefinite),
                            ("choice[1:array]", TooFew 1),
                            ("choice[1:array]", AcceptArity 2),
                            ("choice[1:array].array[0].repeat1", WrongReference (Name "vkeywitness"))
                          ]

-- * HELPERS

conwayRoot :: HuddleSpec
conwayRoot = either (error . toText) id (eraRoot "conway")

conwayObligations :: Text -> [Obligation]
conwayObligations = obligationsFor conwayRoot . Name

dijkstraRoot :: HuddleSpec
dijkstraRoot = either (error . toText) id (eraRoot "dijkstra")

dijkstraObligations :: Text -> [Obligation]
dijkstraObligations = obligationsFor dijkstraRoot . Name

-- | Collect all the spec obligations
allObligations :: HuddleSpec -> Map Name [Obligation]
allObligations root@(CTreeRoot rules) = Map.fromList [(name, ruleObligations root rule) | (name, rule) <- Map.toList rules]

-- | The obligations that the rule with this name states, or none when the specification has no such rule.
obligationsFor :: HuddleSpec -> Name -> [Obligation]
obligationsFor root@(CTreeRoot rules) name = maybe [] (ruleObligations root) (Map.lookup name rules)

-- | The shape recorded for each alternative of a rule that is a choice, in the order the specification lists them. This
-- is what tells a sample's branch apart from its siblings, so a branch reading as 'ShapeOther' fits everything and a
-- branch reading as 'ShapeAmbiguous' fits what its siblings of the same shape fit.
branchShapes :: Text -> [Shape]
branchShapes = choiceShapes . conwayObligations

dijkstraObligationKinds :: Text -> [(Text, ObligationKind)]
dijkstraObligationKinds name =
  [ (renderTrail (obligationPath obligation), obligationKind obligation)
  | obligation <- dijkstraObligations name
  ]

dijkstraBranchShapes :: Text -> [Shape]
dijkstraBranchShapes = choiceShapes . dijkstraObligations

choiceShapes :: [Obligation] -> [Shape]
choiceShapes obligations =
  map snd . sortOn fst . ordNub $
    [(index, shape) | o <- obligations, StepChoice index shape : _ <- [steps (obligationPath o)]]

-- | Where the set folded into @transaction_body@'s first field states what it asks for: once under the tag, once bare.
taggedInputs :: Text
taggedInputs = "map{0}.value.validator.gen.choice[0:tag258].tag258"

bareInputs :: Text
bareInputs = "map{0}.value.validator.gen.choice[1:array]"

-- | The set of owners inside a pool registration certificate, named once so the rows that reach into it stay readable.
owners :: Text
owners = "choice[3:array(3)].array[7].value.validator.gen"

-- | The rule an obligation names, where it names one.
referenced :: ObligationKind -> [Text]
referenced = \case
  AcceptReference name -> [unName name]
  WrongReference name -> [unName name]
  _ -> []

-- | What a rule states below one path, with each path written from there, and @.@ for the path itself.
kindsBelow :: Text -> Text -> [(Text, ObligationKind)]
kindsBelow rule prefix =
  [ (if T.null rest then "." else T.drop 1 rest, kind)
  | (path, kind) <- obligationKinds rule,
    Just rest <- [T.stripPrefix prefix path]
  ]

-- | Every obligation a rule states: where it is stated, and what it asks for.
obligationKinds :: Text -> [(Text, ObligationKind)]
obligationKinds name =
  [ (renderTrail (obligationPath obligation), obligationKind obligation)
  | obligation <- conwayObligations name
  ]

-- | What one alternative of a choice states, the alternative named by the leading step of each path.
branchKinds :: Text -> Text -> [(Text, ObligationKind)]
branchKinds name branch =
  [ entry
  | entry@(path, _) <- obligationKinds name,
    path == branch || (branch <> ".") `T.isPrefixOf` path
  ]

-- | Every obligation kind an era states, for a claim about the whole specification rather than about one rule.
allKinds :: HuddleSpec -> [ObligationKind]
allKinds root = [obligationKind obligation | (_, obligations) <- Map.toList $ allObligations root, obligation <- obligations]

kindsOf :: Text -> [ObligationKind]
kindsOf = map obligationKind . conwayObligations

-- | What a fixed length states at one path: the two sizes either side of it, which have to be refused. Nothing states
-- the length itself, since reaching the container in a valid sample forces it.
arityObligations :: (Text, Int) -> [(Text, ObligationKind)]
arityObligations (path, size) =
  [ (path, TooFew (fromIntegral size)),
    (path, TooMany (fromIntegral size))
  ]

isReference :: ObligationKind -> Bool
isReference = \case
  WrongReference _ -> True
  _ -> False

isRequiredKey :: ObligationKind -> Bool
isRequiredKey = \case
  AcceptRequiredKey _ -> True
  MissingRequiredKey _ -> True
  _ -> False

isArity :: ObligationKind -> Bool
isArity = \case
  AcceptArity _ -> True
  TooFew _ -> True
  TooMany _ -> True
  _ -> False

isBound :: ObligationKind -> Bool
isBound = \case
  TooFew _ -> True
  TooMany _ -> True
  _ -> False

isValidator :: ObligationKind -> Bool
isValidator = \case
  ValidatorSucceeds -> True
  ValidatorFails -> True
  _ -> False
