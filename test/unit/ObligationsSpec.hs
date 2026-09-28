-- | What each Conway rule asks a corpus to contain.
--
-- One rule per kind of obligation, chosen to be small enough that the whole
-- list can be asserted rather than sampled: an obligation gained or lost then
-- shows up here instead of quietly changing a coverage number. Between them
-- these rules also exercise every kind of step a path is made of.
module ObligationsSpec (spec) where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Data.Text qualified as T
import Obligations
  ( LiteralValue (..),
    Obligation (..),
    ObligationKind (..),
    Primitive (..),
    Shape (..),
    Step (..),
    foldedRule,
    obligationsFor,
    renderPath,
  )
import Reachability (eraRoot)
import Test.Hspec

spec :: Spec
spec = do
  describe "Arrays" $ do
    it "We check both the arity and the definiteness vs indefiniteness of the encoding" $
      -- For example: vkeywitness = [vkey, signature]
      obligationKinds "vkeywitness"
        `shouldBe` [ (".", WrongType PrimArray),
                     (".", AcceptDefinite),
                     (".", AcceptIndefinite),
                     (".", TooFew 2),
                     (".", TooMany 2),
                     ("array[0]", WrongReference (Name "vkey")),
                     ("array[1]", WrongReference (Name "signature"))
                   ]

    it "Group definitions are flattened in order to determine the expected arity" $
      -- Each certificate alternative splices a group into the array
      -- For each group we much check its arity, so there are 3 obligations: too low, exact, too high.
      -- Some groups are nested: for example pool_registration_cert is [3, pool_params] where
      -- pool_params is a group of 9 elements. This makes a total of 10 elements.
      -- comes to ten, because pool_params is itself a group of nine.
      -- Only the alternatives themselves. Their spliced fields are addressed by
      -- where they land in the array, so the nested containers among them state
      -- arities of their own, and those belong to the fields rather than here.
      [ (path, kind)
      | (path, kind) <- obligationKinds "certificate",
        isArity kind,
        not (T.isInfixOf "." path)
      ]
        `shouldBe` concatMap
          arityObligations
          [ ("choice[0]", 2),
            ("choice[1]", 2),
            ("choice[2]", 3),
            ("choice[3]", 10),
            ("choice[4]", 3),
            ("choice[5]", 3),
            ("choice[6]", 3),
            ("choice[7]", 3),
            ("choice[8]", 4),
            ("choice[9]", 4),
            ("choice[10]", 4),
            ("choice[11]", 5),
            ("choice[12]", 3),
            ("choice[13]", 3),
            ("choice[14]", 4),
            ("choice[15]", 3),
            ("choice[16]", 3)
          ]

    it "We produce arity obligations for each alternative of a choice" $
      -- drep = [0, addr_keyhash // 1, script_hash // 2 // 3]
      obligationKinds "drep"
        `shouldBe` [ (".", NoBranchMatches [ShapeArray (Just 0), ShapeArray (Just 1), ShapeArray (Just 2), ShapeArray (Just 3)]),
                     (".", NoDiscriminantMatches [0, 1, 2, 3]),
                     ("choice[0]", AcceptDefinite),
                     ("choice[0]", AcceptIndefinite),
                     ("choice[0]", TooFew 2),
                     ("choice[0]", TooMany 2),
                     ("choice[0].array[1]", WrongReference (Name "addr_keyhash")),
                     ("choice[1]", AcceptDefinite),
                     ("choice[1]", AcceptIndefinite),
                     ("choice[1]", TooFew 2),
                     ("choice[1]", TooMany 2),
                     ("choice[1].array[1]", WrongReference (Name "script_hash")),
                     ("choice[2]", AcceptDefinite),
                     ("choice[2]", AcceptIndefinite),
                     ("choice[2]", TooMany 1),
                     ("choice[3]", AcceptDefinite),
                     ("choice[3]", AcceptIndefinite),
                     ("choice[3]", TooMany 1)
                   ]

    it "We check repeated entries arities in a map + definite vs indefinite encoding" $
      -- withdrawals = {+ reward_account => coin}
      obligationKinds "withdrawals"
        `shouldBe` [ (".", WrongType PrimMap),
                     (".", AcceptDefinite),
                     (".", AcceptIndefinite),
                     (".", TooFew 1),
                     (".", AcceptArity 2),
                     (".", DuplicateKey),
                     ("map[0].occur.key", WrongReference (Name "reward_account")),
                     ("map[0].occur.value", WrongReference (Name "coin")),
                     ("map[1].occur.key", WrongReference (Name "reward_account")),
                     ("map[1].occur.value", WrongReference (Name "coin"))
                   ]

    it "We check an array arity even with optional entries" $
      -- alonzo_transaction_output = [address, amount : value, ? datum_hash : hash32].
      obligationKinds "alonzo_transaction_output"
        `shouldBe` [ (".", WrongType PrimArray),
                     (".", AcceptDefinite),
                     (".", AcceptIndefinite),
                     (".", AcceptArity 2),
                     (".", AcceptArity 3),
                     (".", TooFew 2),
                     (".", TooMany 3),
                     ("array[0]", WrongReference (Name "address")),
                     ("array[1].value", WrongReference (Name "value")),
                     ("array[2].occur.value", WrongReference (Name "hash32"))
                   ]

  describe "Ranges" $ do
    it "We produce obligations for low bound / high bound and inside range" $ do
      -- vote = 0 .. 2
      obligationKinds "vote"
        `shouldBe` [ (".", AcceptBoundary 0),
                     (".", AcceptBoundary 2),
                     (".", InsideRange 0 2),
                     (".", BelowRange 0),
                     (".", AboveRange 2)
                   ]

    it "We follow references to read a range endpoint" $
      -- positive_coin = 1 .. max_word64, where max_word64 is a rule of its own.
      -- A range endpoint is followed through the reference.
      obligationKinds "positive_coin"
        `shouldBe` [ (".", AcceptBoundary 1),
                     (".", AcceptBoundary 18446744073709551615),
                     (".", InsideRange 1 18446744073709551615),
                     (".", BelowRange 1),
                     (".", AboveRange 18446744073709551615)
                   ]

  describe "Rules nothing decodes on its own" $ do
    it "A half of a range is measured through the union, not under its own name" $ do
      -- nonzero_int64 = negative_int64 / positive_int64, and the ledger decodes
      -- only the union. Each half admits everything on its side of zero and
      -- says nothing about the other, so a corpus of its own could never show
      -- zero being refused and the obligation would stand forever.
      map (foldedRule . Name) ["negative_int64", "positive_int64"]
        `shouldBe` [True, True]

      -- The union is a rule of its own, and it is where refusing zero is both
      -- stated and answerable: the gap between the two spans is the only thing
      -- neither alternative accepts.
      foldedRule (Name "nonzero_int64") `shouldBe` False
      obligationKinds "nonzero_int64"
        `shouldBe` [ ( ".",
                       NoBranchMatches
                         [ ShapeRange (-9223372036854775808) (-1),
                           ShapeRange 1 9223372036854775807
                         ]
                     ),
                     -- Each half is folded in, so its span is stated on the
                     -- branch that carries it. Neither half denies a value
                     -- outside its own span: that is how the branch was picked,
                     -- and what no branch admits is stated once above.
                     ("choice[0]", AcceptBoundary (-9223372036854775808)),
                     ("choice[0]", AcceptBoundary (-1)),
                     ("choice[0]", InsideRange (-9223372036854775808) (-1)),
                     ("choice[1]", AcceptBoundary 1),
                     ("choice[1]", AcceptBoundary 9223372036854775807),
                     ("choice[1]", InsideRange 1 9223372036854775807)
                   ]

    it "A range nothing decodes alone is stated where it is read" $ do
      -- redeemer_tag = 0 .. 5, and no type decodes a bare one: the ledger reads
      -- it as the opening word of a redeemer, which is the only place a word
      -- outside the span is refused. The span is therefore stated at that
      -- position, with the reference obligation still beside it.
      [entry | entry@("array[0].value", _) <- obligationKinds "redeemer"]
        `shouldBe` [ ("array[0].value", WrongReference (Name "redeemer_tag")),
                     ("array[0].value", AcceptBoundary 0),
                     ("array[0].value", AcceptBoundary 5),
                     ("array[0].value", InsideRange 0 5),
                     ("array[0].value", BelowRange 0),
                     ("array[0].value", AboveRange 5)
                   ]

      -- The span is stated at the carrier but owed by the rule it came from, so
      -- a page reports it under redeemer_tag. Only the reference itself is
      -- redeemer's own.
      [ obligationOrigin obligation
        | obligation <- conwayObligations "redeemer",
          renderPath (obligationPath obligation) == "array[0].value"
        ]
        `shouldBe` Nothing : replicate 5 (Just (Name "redeemer_tag"))

    it "A spliced group's fields are addressed by where they land in the bytes" $ do
      -- certificate choice[3] is pool_registration_cert = [3, pool_params], and
      -- pool_params is itself a group of nine. On the wire that is one flat
      -- array of ten, so the operator sits at index 1, not one level down in a
      -- group of its own. Addressing has to agree with the arity above, which
      -- already counts the splice.
      [(path, name) | (path, WrongReference name) <- branchKinds "certificate" "choice[3]"]
        `shouldBe` [ ("choice[3].array[1].value", Name "pool_keyhash"),
                     ("choice[3].array[2].value", Name "vrf_keyhash"),
                     ("choice[3].array[3].value", Name "coin"),
                     ("choice[3].array[4].value", Name "coin"),
                     ("choice[3].array[5].value", Name "unit_interval"),
                     ("choice[3].array[6].value", Name "reward_account"),
                     (owners <> ".choice[0].tag258.array[0].occur", Name "addr_keyhash"),
                     (owners <> ".choice[0].tag258.array[1].occur", Name "addr_keyhash"),
                     (owners <> ".choice[1].array[0].occur", Name "addr_keyhash"),
                     (owners <> ".choice[1].array[1].occur", Name "addr_keyhash"),
                     ("choice[3].array[8].value.array[0].occur", Name "relay"),
                     ("choice[3].array[8].value.array[1].occur", Name "relay"),
                     ("choice[3].array[9].value.choice[0]", Name "pool_metadata")
                   ]

      -- The arity above says the same thing in the other direction: ten, not
      -- two, because the nine spliced fields are counted where they land.
      [kind | ("choice[3]", kind) <- obligationKinds "certificate", isArity kind]
        `shouldBe` [TooFew 10, TooMany 10]

    it "A folded rule is named nowhere, and the entry it sits on still is" $ do
      -- transaction_witness_set's first field holds %nonempty_list<vkeywitness>,
      -- which no node decodes alone: the ledger reaches it through
      -- NonEmpty (WitVKey Witness) and Amaru through NonEmptyVector<Witness>,
      -- both from inside the witness set. Folding states what it asks for here
      -- and drops the name, so nothing can ask a consumer for a rule that is no
      -- longer listed.
      [name | (_, kind) <- obligationKinds "transaction_witness_set", name <- referenced kind]
        `shouldSatisfy` all (not . T.isPrefixOf "%nonempty_")

      -- What survives is that the entry may be absent, which is the only thing
      -- the reference obligation said that the folded body does not. Every
      -- field bar key 5, which holds redeemers: that is a rule of its own, with
      -- a decoder of its own, so it still names what it refers to.
      [path | (path, AcceptPresent) <- obligationKinds "transaction_witness_set"]
        `shouldBe` map
          (\index -> "map{" <> show index <> "}.occur.value")
          [0, 1, 2, 3, 4, 6, 7 :: Int]

  describe "Control modifiers" $ do
    it ".size produces obligations to check the exact size" $
      -- vrf_vkey = bytes .size 32
      obligationKinds "vrf_vkey"
        `shouldBe` [ ("control", AcceptSize 32 32),
                     ("control", ViolateSizeBelow 32),
                     ("control", ViolateSizeAbove 32),
                     ("control", AcceptType PrimBytes),
                     ("control", WrongType PrimBytes)
                   ]

    it ".size with bounds produces obligations to check the bounds" $
      -- url = text .size (0 .. 128)
      obligationKinds "url"
        `shouldBe` [ ("control", AcceptSize 0 128),
                     ("control", ViolateSizeAbove 128),
                     ("control", AcceptType PrimText),
                     ("control", WrongType PrimText)
                   ]

    it ".size on a number produces obligations to check the number as a ceiling on the value" $
      -- epoch_interval = uint .size 4, so the value has to fit in four bytes.
      -- On a number .size is not a length but a ceiling, and the ceiling is
      -- 256^n - 1. Chosen over a two byte field so the conversion is visible:
      -- port below says the same thing as `.le 65535` and would otherwise read
      -- identically.
      obligationKinds "epoch_interval"
        `shouldBe` [ ("control", AcceptBoundary 4294967295),
                     ("control", ViolateHighBound 4294967295),
                     ("control", AcceptType PrimUInt),
                     ("control", WrongType PrimUInt)
                   ]

    it ".le check the maximum size of a number" $
      -- port = uint .le 65535. The only .le in the specification. It states the
      -- same value set as `uint .size 2`, and normalising both spellings to one
      -- obligation is the point: what a decoder owes should not depend on which
      -- the specification author wrote.
      obligationKinds "port"
        `shouldBe` [ ("control", AcceptBoundary 65535),
                     ("control", ViolateHighBound 65535),
                     ("control", AcceptType PrimUInt),
                     ("control", WrongType PrimUInt)
                   ]

    it ".cbor checks the contents of an embedded cbor control" $
      -- script_ref = #6.24(bytes .cbor script). The tag around bytes whose
      -- contents are themselves CBOR is also checked.
      obligationKinds "script_ref"
        `shouldBe` [ (".", AcceptTag 24),
                     (".", WrongTag 24),
                     ("tag24.control", AcceptCbor),
                     ("tag24.control", ViolateCbor),
                     ("tag24.control", AcceptType PrimBytes),
                     ("tag24.control", WrongType PrimBytes)
                   ]

    -- A control narrows what a value may be without making it a different
    -- value, so an alternative stating `bytes .size (0..64)` is still found by
    -- being bytes: denying that one step down denies the shape just as it would
    -- at the alternative itself, and the choice refuses it at the root.
    it "a constrained branch keeps the shape of what it constrains" $ do
      -- metadatum = { * metadatum => metadatum } / [ * metadatum ] / int
      --           / bytes .size (0 .. 64) / text .size (0 .. 64)
      -- The last 2 constraints apply to 2 different types and we should have obligations for both.
      obligationKinds "metadatum"
        `shouldBe` [ (".", NoBranchMatches [ShapeMap, ShapeArray Nothing, ShapePrimitive PrimInt, ShapePrimitive PrimBytes, ShapePrimitive PrimText]),
                     ("choice[0]", AcceptDefinite),
                     ("choice[0]", AcceptIndefinite),
                     ("choice[0]", AcceptArity 2),
                     ("choice[0]", DuplicateKey),
                     ("choice[0].map[0].occur.key", AcceptReference (Name "metadatum")),
                     ("choice[0].map[0].occur.key", WrongReference (Name "metadatum")),
                     ("choice[0].map[0].occur.value", AcceptReference (Name "metadatum")),
                     ("choice[0].map[0].occur.value", WrongReference (Name "metadatum")),
                     ("choice[0].map[1].occur.key", AcceptReference (Name "metadatum")),
                     ("choice[0].map[1].occur.key", WrongReference (Name "metadatum")),
                     ("choice[0].map[1].occur.value", AcceptReference (Name "metadatum")),
                     ("choice[0].map[1].occur.value", WrongReference (Name "metadatum")),
                     ("choice[1]", AcceptDefinite),
                     ("choice[1]", AcceptIndefinite),
                     ("choice[1]", AcceptArity 2),
                     ("choice[1].array[0].occur", WrongReference (Name "metadatum")),
                     ("choice[1].array[1].occur", WrongReference (Name "metadatum")),
                     ("choice[2]", AcceptType PrimInt),
                     ("choice[2]", AcceptBoundary (-18446744073709551616)),
                     ("choice[2]", AcceptBoundary 18446744073709551615),
                     ("choice[3].control", AcceptSize 0 64),
                     ("choice[3].control", ViolateSizeAbove 64),
                     ("choice[3].control", AcceptType PrimBytes),
                     ("choice[4].control", AcceptSize 0 64),
                     ("choice[4].control", ViolateSizeAbove 64),
                     ("choice[4].control", AcceptType PrimText)
                   ]

  describe "Tags and discriminants" $ do
    it "We check the tag of a tagged rule" $ do
      -- big_uint = #6.2(bounded_bytes): the tag is all this rule adds, and all
      -- it can ask for. Tag 2 is a bignum, so CBOR itself admits only a byte
      -- string under it: tag 2 over anything else stops being decodable before
      -- the specification is consulted, and an obligation about that payload
      -- could never be answered.
      obligationKinds "big_uint"
        `shouldBe` [(".", AcceptTag 2), (".", WrongTag 2)]

      -- A tag the encoding says nothing about keeps its payload stated, since
      -- there the rule is the only thing deciding what may be wrapped.
      [path | (path, _) <- obligationKinds "%set<addr_keyhash>", "tag258." `T.isInfixOf` path]
        `shouldSatisfy` (not . null)

    it "An alternative that is a span of numbers is told apart by where a value falls" $
      -- nonzero_int64 = negative_int64 / positive_int64. Both are integers, so
      -- only the span separates them, and the rule each names is that span: a
      -- value either rule refuses is a value of the other alternative, so the
      -- choice states it once at the root.
      obligationKinds "nonzero_int64"
        `shouldBe` [ (".", NoBranchMatches [ShapeRange (-9223372036854775808) (-1), ShapeRange 1 9223372036854775807]),
                     ("choice[0]", AcceptBoundary (-9223372036854775808)),
                     ("choice[0]", AcceptBoundary (-1)),
                     ("choice[0]", InsideRange (-9223372036854775808) (-1)),
                     ("choice[1]", AcceptBoundary 1),
                     ("choice[1]", AcceptBoundary 9223372036854775807),
                     ("choice[1]", InsideRange 1 9223372036854775807)
                   ]

    it "An alternative that is a bignum states only that it is one" $
      -- big_int = int / big_uint / big_nint, so the last two alternatives are
      -- reached by their tag. Denying what one of them names means a payload
      -- the encoding forbids, which stops being CBOR before the specification
      -- sees it, so only the tags themselves are stated.
      obligationKinds "big_int"
        `shouldBe` [ (".", NoBranchMatches [ShapePrimitive PrimInt, ShapeTag 2, ShapeTag 3]),
                     (".", NoTagMatches [2, 3]),
                     ("choice[0]", AcceptType PrimInt),
                     ("choice[0]", AcceptBoundary (-18446744073709551616)),
                     ("choice[0]", AcceptBoundary 18446744073709551615),
                     ("choice[1]", AcceptBranch),
                     ("choice[2]", AcceptBranch)
                   ]

    it "We check a group's discriminant" $
      -- script_pubkey = (0, addr_keyhash)
      obligationKinds "script_pubkey"
        `shouldBe` [ ("group[0]", AcceptLiteral (LitUInt 0)),
                     ("group[0]", WrongLiteral (LitUInt 0)),
                     ("group[1]", WrongReference (Name "addr_keyhash"))
                   ]

  describe "Maps" $ do
    it "We support both the required and optional keys" $
      do
        -- babbage_transaction_output =
        --   mp
        --   [ idx 0 ==> huddleRule @"address" p
        --   , idx 1 ==> huddleRule @"value" p
        --   , opt $ idx 2 ==> huddleRule @"datum_option" p //- "new"
        --   , opt $ idx 3 ==> huddleRule @"script_ref" p //- "new"
        --   ]
        --
        -- 2 required and 2 optional keys
        obligationKinds "babbage_transaction_output"
        `shouldBe` [ (".", WrongType PrimMap),
                     (".", AcceptDefinite),
                     (".", AcceptIndefinite),
                     -- Two keys required and two optional, so the length is a range: 2 to 4.
                     (".", AcceptArity 2),
                     (".", AcceptArity 3),
                     (".", AcceptArity 4),
                     (".", TooFew 2),
                     (".", TooMany 4),
                     (".", DuplicateKey),
                     -- Keys 2 and 3 are optional, so nothing requires them,
                     -- but a key outside the four named still has to fail.
                     (".", NoKeyMatches [LitUInt 0, LitUInt 1, LitUInt 2, LitUInt 3]),
                     ("map{0}.key", AcceptRequiredKey (LitUInt 0)),
                     ("map{0}.key", MissingRequiredKey (LitUInt 0)),
                     ("map{1}.key", AcceptRequiredKey (LitUInt 1)),
                     ("map{1}.key", MissingRequiredKey (LitUInt 1)),
                     ("map{0}.value", WrongReference (Name "address")),
                     ("map{1}.value", WrongReference (Name "value")),
                     ("map{2}.occur.value", AcceptReference (Name "datum_option")),
                     ("map{2}.occur.value", WrongReference (Name "datum_option")),
                     ("map{3}.occur.value", AcceptReference (Name "script_ref")),
                     ("map{3}.occur.value", WrongReference (Name "script_ref")),
                     -- script_ref has no corpus of its own, so its obligations are
                     -- stated here too, where a sample actually carries one.
                     ("map{3}.occur.value", AcceptTag 24),
                     ("map{3}.occur.value", WrongTag 24),
                     ("map{3}.occur.value.tag24.control", AcceptCbor),
                     ("map{3}.occur.value.tag24.control", ViolateCbor),
                     ("map{3}.occur.value.tag24.control", AcceptType PrimBytes),
                     ("map{3}.occur.value.tag24.control", WrongType PrimBytes)
                   ]

    it "We check the tag of a tagged map" $
      -- auxiliary_data_map =
      -- tag
      --   259
      --   ( mp
      --       [ opt (idx 0 ==> huddleRule @"metadata" p)
      --       , opt (idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)])
      --       , opt (idx 2 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)])
      --       , opt (idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v2_script" p)])
      --       , opt (idx 4 ==> arr [0 <+ a (huddleRule @"plutus_v3_script" p)])
      --       ]
      --   )
      obligationKinds "auxiliary_data_map"
        `shouldBe` [ (".", AcceptTag 259),
                     (".", WrongTag 259),
                     ("tag259", WrongType PrimMap),
                     ("tag259", AcceptDefinite),
                     ("tag259", AcceptIndefinite),
                     ("tag259", TooMany 5),
                     ("tag259", DuplicateKey),
                     ("tag259", NoKeyMatches [LitUInt 0, LitUInt 1, LitUInt 2, LitUInt 3, LitUInt 4]),
                     ("tag259.map{0}.occur.value", AcceptReference (Name "metadata")),
                     ("tag259.map{0}.occur.value", WrongReference (Name "metadata")),
                     ("tag259.map{1}.occur.value", WrongType PrimArray),
                     ("tag259.map{1}.occur.value", AcceptDefinite),
                     ("tag259.map{1}.occur.value", AcceptIndefinite),
                     ("tag259.map{1}.occur.value", AcceptArity 2),
                     ("tag259.map{1}.occur.value.array[0].occur", WrongReference (Name "native_script")),
                     ("tag259.map{1}.occur.value.array[1].occur", WrongReference (Name "native_script")),
                     ("tag259.map{2}.occur.value", WrongType PrimArray),
                     ("tag259.map{2}.occur.value", AcceptDefinite),
                     ("tag259.map{2}.occur.value", AcceptIndefinite),
                     ("tag259.map{2}.occur.value", AcceptArity 2),
                     ("tag259.map{2}.occur.value.array[0].occur", WrongReference (Name "plutus_v1_script")),
                     ("tag259.map{2}.occur.value.array[1].occur", WrongReference (Name "plutus_v1_script")),
                     ("tag259.map{3}.occur.value", WrongType PrimArray),
                     ("tag259.map{3}.occur.value", AcceptDefinite),
                     ("tag259.map{3}.occur.value", AcceptIndefinite),
                     ("tag259.map{3}.occur.value", AcceptArity 2),
                     ("tag259.map{3}.occur.value.array[0].occur", WrongReference (Name "plutus_v2_script")),
                     ("tag259.map{3}.occur.value.array[1].occur", WrongReference (Name "plutus_v2_script")),
                     ("tag259.map{4}.occur.value", WrongType PrimArray),
                     ("tag259.map{4}.occur.value", AcceptDefinite),
                     ("tag259.map{4}.occur.value", AcceptIndefinite),
                     ("tag259.map{4}.occur.value", AcceptArity 2),
                     ("tag259.map{4}.occur.value.array[0].occur", WrongReference (Name "plutus_v3_script")),
                     ("tag259.map{4}.occur.value.array[1].occur", WrongReference (Name "plutus_v3_script"))
                   ]

    it "We support maps with optional, then open keys" $
      -- cost_models =
      --   mp
      --     [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"int64" p)]
      --     , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"int64" p)]
      --     , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"int64" p)]
      --     , 0 <+ asKey ((3 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a (huddleRule @"int64" p)]
      --     ]
      --
      -- cost_models ends with `* 3 .. 255 => [* int64]`
      obligationKinds "cost_models"
        `shouldBe` [ (".", WrongType PrimMap),
                     (".", AcceptDefinite),
                     (".", AcceptIndefinite),
                     (".", AcceptArity 5),
                     (".", DuplicateKey),
                     ("map{0}.occur.value", WrongType PrimArray),
                     ("map{0}.occur.value", AcceptDefinite),
                     ("map{0}.occur.value", AcceptIndefinite),
                     ("map{0}.occur.value", AcceptArity 2),
                     ("map{0}.occur.value.array[0].occur", AcceptBoundary (-9223372036854775808)),
                     ("map{0}.occur.value.array[0].occur", AcceptBoundary 9223372036854775807),
                     ("map{0}.occur.value.array[0].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map{0}.occur.value.array[0].occur", BelowRange (-9223372036854775808)),
                     ("map{0}.occur.value.array[0].occur", AboveRange 9223372036854775807),
                     ("map{0}.occur.value.array[1].occur", AcceptBoundary (-9223372036854775808)),
                     ("map{0}.occur.value.array[1].occur", AcceptBoundary 9223372036854775807),
                     ("map{0}.occur.value.array[1].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map{0}.occur.value.array[1].occur", BelowRange (-9223372036854775808)),
                     ("map{0}.occur.value.array[1].occur", AboveRange 9223372036854775807),
                     ("map{1}.occur.value", WrongType PrimArray),
                     ("map{1}.occur.value", AcceptDefinite),
                     ("map{1}.occur.value", AcceptIndefinite),
                     ("map{1}.occur.value", AcceptArity 2),
                     ("map{1}.occur.value.array[0].occur", AcceptBoundary (-9223372036854775808)),
                     ("map{1}.occur.value.array[0].occur", AcceptBoundary 9223372036854775807),
                     ("map{1}.occur.value.array[0].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map{1}.occur.value.array[0].occur", BelowRange (-9223372036854775808)),
                     ("map{1}.occur.value.array[0].occur", AboveRange 9223372036854775807),
                     ("map{1}.occur.value.array[1].occur", AcceptBoundary (-9223372036854775808)),
                     ("map{1}.occur.value.array[1].occur", AcceptBoundary 9223372036854775807),
                     ("map{1}.occur.value.array[1].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map{1}.occur.value.array[1].occur", BelowRange (-9223372036854775808)),
                     ("map{1}.occur.value.array[1].occur", AboveRange 9223372036854775807),
                     ("map{2}.occur.value", WrongType PrimArray),
                     ("map{2}.occur.value", AcceptDefinite),
                     ("map{2}.occur.value", AcceptIndefinite),
                     ("map{2}.occur.value", AcceptArity 2),
                     ("map{2}.occur.value.array[0].occur", AcceptBoundary (-9223372036854775808)),
                     ("map{2}.occur.value.array[0].occur", AcceptBoundary 9223372036854775807),
                     ("map{2}.occur.value.array[0].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map{2}.occur.value.array[0].occur", BelowRange (-9223372036854775808)),
                     ("map{2}.occur.value.array[0].occur", AboveRange 9223372036854775807),
                     ("map{2}.occur.value.array[1].occur", AcceptBoundary (-9223372036854775808)),
                     ("map{2}.occur.value.array[1].occur", AcceptBoundary 9223372036854775807),
                     ("map{2}.occur.value.array[1].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map{2}.occur.value.array[1].occur", BelowRange (-9223372036854775808)),
                     ("map{2}.occur.value.array[1].occur", AboveRange 9223372036854775807),
                     -- No lower bound: the open entry is found by its key
                     -- being none of 0, 1 and 2, so a key under 3 would have
                     -- been read as one of them instead and could never be
                     -- navigated to here. The upper bound is open and stays.
                     ("map[3?].occur.key", AcceptBoundary 3),
                     ("map[3?].occur.key", AcceptBoundary 255),
                     ("map[3?].occur.key", InsideRange 3 255),
                     ("map[3?].occur.key", AboveRange 255),
                     ("map[3?].occur.value", WrongType PrimArray),
                     ("map[3?].occur.value", AcceptDefinite),
                     ("map[3?].occur.value", AcceptIndefinite),
                     ("map[3?].occur.value", AcceptArity 2),
                     ("map[3?].occur.value.array[0].occur", AcceptBoundary (-9223372036854775808)),
                     ("map[3?].occur.value.array[0].occur", AcceptBoundary 9223372036854775807),
                     ("map[3?].occur.value.array[0].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map[3?].occur.value.array[0].occur", BelowRange (-9223372036854775808)),
                     ("map[3?].occur.value.array[0].occur", AboveRange 9223372036854775807),
                     ("map[3?].occur.value.array[1].occur", AcceptBoundary (-9223372036854775808)),
                     ("map[3?].occur.value.array[1].occur", AcceptBoundary 9223372036854775807),
                     ("map[3?].occur.value.array[1].occur", InsideRange (-9223372036854775808) 9223372036854775807),
                     ("map[3?].occur.value.array[1].occur", BelowRange (-9223372036854775808)),
                     ("map[3?].occur.value.array[1].occur", AboveRange 9223372036854775807)
                   ]

    it "We support maps with only an open key" $
      -- withdrawals =
      --   mp [0 <+ asKey (huddleRule @"reward_account" p) ==> huddleRule @"coin" p].
      obligationKinds "withdrawals"
        `shouldBe` [ (".", WrongType PrimMap),
                     (".", AcceptDefinite),
                     (".", AcceptIndefinite),
                     (".", TooFew 1),
                     (".", AcceptArity 2),
                     (".", DuplicateKey),
                     ("map[0].occur.key", WrongReference (Name "reward_account")),
                     ("map[0].occur.value", WrongReference (Name "coin")),
                     ("map[1].occur.key", WrongReference (Name "reward_account")),
                     ("map[1].occur.value", WrongReference (Name "coin"))
                   ]

    it "We check a large keyed map with required and optional entries" $
      -- transaction_body =
      --    mp
      --        [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
      --        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
      --        , idx 2 ==> huddleRule @"coin" p //- "fee"
      --        , opt (idx 3 ==> huddleRule @"slot" p) //- "time to live"
      --        , opt (idx 4 ==> huddleRule @"certificates" p)
      --        , opt (idx 5 ==> huddleRule @"withdrawals" p)
      --        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
      --        , opt (idx 8 ==> huddleRule @"slot" p) //- "validity interval start"
      --        , opt (idx 9 ==> huddleRule @"mint" p)
      --        , opt (idx 11 ==> huddleRule @"script_data_hash" p)
      --        , opt (idx 13 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p)) //- "collateral"
      --        , opt (idx 14 ==> huddleRule @"required_signers" p)
      --        , opt (idx 15 ==> huddleRule @"network_id" p)
      --        , opt (idx 16 ==> huddleRule @"transaction_output" p) //- "collateral return"
      --        , opt (idx 17 ==> huddleRule @"coin" p) //- "total collateral"
      --        , opt (idx 18 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p))
      --            //- "reference inputs"
      --        , opt (idx 19 ==> huddleRule @"voting_procedures" p)
      --        , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
      --        , opt (idx 21 ==> huddleRule @"coin" p) //- "current treasury value"
      --        , opt (idx 22 ==> huddleRule @"positive_coin" p) //- "donation"
      --        ]
      --
      -- transaction_body is the biggest map in the specification: three required keys, seventeen optional ones,
      -- and a value at every entry that is a rule of its own.
      -- Its length spans 3 to 20, too wide for the length to be the interesting thing, so it states no accepted
      -- length at all.
      -- The sets these fields hold are folded in, and what they ask for is left
      -- to the example that pins spliced addressing. Here the map itself is the
      -- subject: its keys, its length, and what each field refers to.
      [entry | entry@(path, _) <- obligationKinds "transaction_body", not (T.isInfixOf "validator" path)]
        `shouldBe` [ (".", WrongType PrimMap),
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
                     ("map{1}.value.array[0].occur", WrongReference (Name "transaction_output")),
                     ("map{1}.value.array[1].occur", WrongReference (Name "transaction_output")),
                     ("map{2}.value", WrongReference (Name "coin")),
                     ("map{3}.occur.value", AcceptReference (Name "slot")),
                     ("map{3}.occur.value", WrongReference (Name "slot")),
                     ("map{4}.occur.value", AcceptPresent),
                     ("map{4}.occur.value", ValidatorSucceeds),
                     ("map{4}.occur.value", ValidatorFails),
                     ("map{5}.occur.value", AcceptReference (Name "withdrawals")),
                     ("map{5}.occur.value", WrongReference (Name "withdrawals")),
                     ("map{7}.occur.value", AcceptReference (Name "auxiliary_data_hash")),
                     ("map{7}.occur.value", WrongReference (Name "auxiliary_data_hash")),
                     ("map{8}.occur.value", AcceptReference (Name "slot")),
                     ("map{8}.occur.value", WrongReference (Name "slot")),
                     ("map{9}.occur.value", AcceptReference (Name "mint")),
                     ("map{9}.occur.value", WrongReference (Name "mint")),
                     ("map{11}.occur.value", AcceptReference (Name "script_data_hash")),
                     ("map{11}.occur.value", WrongReference (Name "script_data_hash")),
                     ("map{13}.occur.value", AcceptPresent),
                     ("map{13}.occur.value", ValidatorSucceeds),
                     ("map{13}.occur.value", ValidatorFails),
                     ("map{14}.occur.value", AcceptPresent),
                     ("map{14}.occur.value", ValidatorSucceeds),
                     ("map{14}.occur.value", ValidatorFails),
                     ("map{15}.occur.value", AcceptReference (Name "network_id")),
                     ("map{15}.occur.value", WrongReference (Name "network_id")),
                     ("map{16}.occur.value", AcceptReference (Name "transaction_output")),
                     ("map{16}.occur.value", WrongReference (Name "transaction_output")),
                     ("map{17}.occur.value", AcceptReference (Name "coin")),
                     ("map{17}.occur.value", WrongReference (Name "coin")),
                     ("map{18}.occur.value", AcceptPresent),
                     ("map{18}.occur.value", ValidatorSucceeds),
                     ("map{18}.occur.value", ValidatorFails),
                     ("map{19}.occur.value", AcceptReference (Name "voting_procedures")),
                     ("map{19}.occur.value", WrongReference (Name "voting_procedures")),
                     ("map{20}.occur.value", AcceptPresent),
                     ("map{20}.occur.value", ValidatorSucceeds),
                     ("map{20}.occur.value", ValidatorFails),
                     ("map{21}.occur.value", AcceptReference (Name "coin")),
                     ("map{21}.occur.value", WrongReference (Name "coin")),
                     ("map{22}.occur.value", AcceptReference (Name "positive_coin")),
                     ("map{22}.occur.value", WrongReference (Name "positive_coin"))
                   ]

    it "We check a map of maps at both levels, including the repeated entries" $
      -- mint = multiasset<nonzero_int64>
      --      = {+ policy_id => {+ asset_name => nonzero_int64}}
      --
      -- Both levels are open, so each states its own arity of two: a map of one
      -- entry satisfies everything a decoder that stops after the first would
      -- do, and nothing else would catch it. The second position repeats what
      -- the entry says directly and no more, which is why the inner map of the
      -- outer second entry carries its container obligations but not the
      -- asset_name and nonzero_int64 references again.
      obligationKinds "mint"
        `shouldBe` [ (".", WrongType PrimMap),
                     (".", AcceptDefinite),
                     (".", AcceptIndefinite),
                     (".", TooFew 1),
                     (".", AcceptArity 2),
                     (".", DuplicateKey),
                     ("map[0].occur.key", WrongReference (Name "policy_id")),
                     ("map[0].occur.value", WrongType PrimMap),
                     ("map[0].occur.value", AcceptDefinite),
                     ("map[0].occur.value", AcceptIndefinite),
                     ("map[0].occur.value", TooFew 1),
                     ("map[0].occur.value", AcceptArity 2),
                     ("map[0].occur.value", DuplicateKey),
                     ("map[0].occur.value.map[0].occur.key", WrongReference (Name "asset_name")),
                     ("map[0].occur.value.map[0].occur.value", WrongReference (Name "nonzero_int64")),
                     ("map[0].occur.value.map[1].occur.key", WrongReference (Name "asset_name")),
                     ("map[0].occur.value.map[1].occur.value", WrongReference (Name "nonzero_int64")),
                     ("map[1].occur.key", WrongReference (Name "policy_id")),
                     ("map[1].occur.value", WrongType PrimMap),
                     ("map[1].occur.value", AcceptDefinite),
                     ("map[1].occur.value", AcceptIndefinite),
                     ("map[1].occur.value", TooFew 1),
                     ("map[1].occur.value", AcceptArity 2),
                     ("map[1].occur.value", DuplicateKey)
                   ]

  describe "Choices" $ do
    it "We check that each alternative is covered" $
      -- auxiliary_data = metadata / auxiliary_data_array / auxiliary_data_map.
      obligationKinds "auxiliary_data"
        `shouldBe` [ (".", NoBranchMatches [ShapeMap, ShapeArray Nothing, ShapeTag 259]),
                     (".", NoTagMatches [259]),
                     ("choice[0]", AcceptBranch),
                     ("choice[0]", WrongReference (Name "metadata")),
                     ("choice[1]", AcceptBranch),
                     ("choice[1]", WrongReference (Name "auxiliary_data_array")),
                     ("choice[2]", AcceptBranch),
                     ("choice[2]", WrongReference (Name "auxiliary_data_map"))
                   ]

    it "We check choices in an array" $
      -- datum_option = [0, hash32 // 1, data]
      obligationKinds "datum_option"
        `shouldBe` [ (".", NoBranchMatches [ShapeArray (Just 0), ShapeArray (Just 1)]),
                     (".", NoDiscriminantMatches [0, 1]),
                     ("choice[0]", AcceptDefinite),
                     ("choice[0]", AcceptIndefinite),
                     ("choice[0]", TooFew 2),
                     ("choice[0]", TooMany 2),
                     ("choice[0].array[1]", WrongReference (Name "hash32")),
                     ("choice[1]", AcceptDefinite),
                     ("choice[1]", AcceptIndefinite),
                     ("choice[1]", TooFew 2),
                     ("choice[1]", TooMany 2),
                     ("choice[1].array[1]", WrongReference (Name "data"))
                   ]

    it "An alternative states no constraint that would erase its own discriminant" $ do
      -- gov_action = ... / [info_action], and info_action = 6, so the sole
      -- element of this array is the uint the alternative is told apart by.
      -- Demanding a non-array, or an array with no room for that uint, asks for
      -- a sample that carries no trace of belonging here. The shape of the
      -- container is still stated: it is an array of exactly one entry, and
      -- both length forms and the entry itself remain.
      -- info_action states nothing of its own: it is the literal 6 and the
      -- specification will make it a group, so what it holds belongs here.
      branchKinds "gov_action" "choice[6]"
        `shouldBe` [ ("choice[6]", AcceptDefinite),
                     ("choice[6]", AcceptIndefinite),
                     ("choice[6]", TooMany 1)
                   ]

      -- A longer alternative keeps its lower bound: three entries where four
      -- are required still open with the discriminant, so such a sample can
      -- still be shown to belong here.
      [kind | ("choice[0]", kind) <- branchKinds "gov_action" "choice[0]"]
        `shouldBe` [AcceptDefinite, AcceptIndefinite, TooFew 4, TooMany 4]

    it "An alternative that is one value is told apart by that value" $ do
      -- network_id = 0 / 1. Both are uints, so nothing but the value itself
      -- separates them: without a shape of its own each alternative would
      -- answer for a sample that took the other, and both would be left opaque.
      branchShapes "network_id"
        `shouldBe` [ShapeLiteral (LitUInt 0), ShapeLiteral (LitUInt 1)]

      -- Denying the value is denying the shape the alternative is found by, so
      -- it is stated once at the choice, where every alternative is in view,
      -- and not again under each of them.
      obligationKinds "network_id"
        `shouldBe` [ (".", NoBranchMatches [ShapeLiteral (LitUInt 0), ShapeLiteral (LitUInt 1)]),
                     ("choice[0]", AcceptLiteral (LitUInt 0)),
                     ("choice[1]", AcceptLiteral (LitUInt 1))
                   ]

    it "We support ambiguous choices" $ do
      -- account_balance_interval =
      --      [inclusive_lower_bound : coin, exclusive_upper_bound : coin / nil]
      --    / [inclusive_lower_bound : coin / nil, exclusive_upper_bound : coin]
      --    / coin
      --
      -- (reminder: coin = uint)
      --
      -- The two array alternatives overlap: [coin, coin] is valid under both.
      --
      -- What the two jointly admit is still exact, and it is stated at the
      -- choice instead: one accept per combination of element shapes, and one
      -- rejection for a two element array that is none of them. Without that
      -- last one [nil, nil] would be owed by nothing, since it is an array of
      -- the right length whose every element fits the branch shapes.
      dijkstraObligationKinds "account_balance_interval"
        `shouldBe` [ (".", NoBranchMatches [ShapeArray Nothing, ShapeArray Nothing, ShapePrimitive PrimUInt]),
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
                     ("choice[0]", WrongType PrimArray),
                     ("choice[0]", AcceptDefinite),
                     ("choice[0]", AcceptIndefinite),
                     ("choice[0]", TooFew 2),
                     ("choice[0]", TooMany 2),
                     ("choice[0].array[1].value", NoBranchMatches [ShapePrimitive PrimUInt, ShapePrimitive PrimNil]),
                     ("choice[0].array[1].value.choice[0]", AcceptBranch),
                     ("choice[0].array[1].value.choice[1]", AcceptType PrimNil),
                     ("choice[1]", WrongType PrimArray),
                     ("choice[1]", AcceptDefinite),
                     ("choice[1]", AcceptIndefinite),
                     ("choice[1]", TooFew 2),
                     ("choice[1]", TooMany 2),
                     ("choice[1].array[0].value", NoBranchMatches [ShapePrimitive PrimUInt, ShapePrimitive PrimNil]),
                     ("choice[1].array[0].value.choice[0]", AcceptBranch),
                     ("choice[1].array[0].value.choice[1]", AcceptType PrimNil),
                     ("choice[2]", AcceptBranch)
                   ]

    it "records the shape of each alternative, marking the overlapping ones" $
      dijkstraBranchShapes "account_balance_interval"
        `shouldBe` [ShapeAmbiguous, ShapeAmbiguous, ShapePrimitive PrimUInt]

  describe "Custom validators" $ do
    it "We check the uniqueness of a set, but succeeding or failing its validator" $
      obligationKinds "%set<transaction_input>"
        `shouldBe` [ (".", ValidatorSucceeds),
                     (".", ValidatorFails),
                     ("validator.gen", NoBranchMatches [ShapeTag 258, ShapeArray Nothing]),
                     ("validator.gen", NoTagMatches [258]),
                     ("validator.gen.choice[0].tag258", WrongType PrimArray),
                     ("validator.gen.choice[0].tag258", AcceptDefinite),
                     ("validator.gen.choice[0].tag258", AcceptIndefinite),
                     ("validator.gen.choice[0].tag258", AcceptArity 2),
                     ("validator.gen.choice[0].tag258.array[0].occur", WrongReference (Name "transaction_input")),
                     ("validator.gen.choice[0].tag258.array[1].occur", WrongReference (Name "transaction_input")),
                     ("validator.gen.choice[1]", AcceptDefinite),
                     ("validator.gen.choice[1]", AcceptIndefinite),
                     ("validator.gen.choice[1]", AcceptArity 2),
                     ("validator.gen.choice[1].array[0].occur", WrongReference (Name "transaction_input")),
                     ("validator.gen.choice[1].array[1].occur", WrongReference (Name "transaction_input"))
                   ]

    it "There is no validator check for a simple list (just an arity check)" $
      obligationKinds "%nonempty_list<vkeywitness>"
        `shouldBe` [ (".", NoBranchMatches [ShapeTag 258, ShapeArray Nothing]),
                     (".", NoTagMatches [258]),
                     ("choice[0].tag258", WrongType PrimArray),
                     ("choice[0].tag258", AcceptDefinite),
                     ("choice[0].tag258", AcceptIndefinite),
                     ("choice[0].tag258", TooFew 1),
                     ("choice[0].tag258", AcceptArity 2),
                     ("choice[0].tag258.array[0].occur", WrongReference (Name "vkeywitness")),
                     ("choice[0].tag258.array[1].occur", WrongReference (Name "vkeywitness")),
                     ("choice[1]", AcceptDefinite),
                     ("choice[1]", AcceptIndefinite),
                     ("choice[1]", TooFew 1),
                     ("choice[1]", AcceptArity 2),
                     ("choice[1].array[0].occur", WrongReference (Name "vkeywitness")),
                     ("choice[1].array[1].occur", WrongReference (Name "vkeywitness"))
                   ]

  describe "Member labels" $ do
    it "They carry no obligation" $
      -- transaction_input = [transaction_id : transaction_id, index : uint .size 2].
      -- transaction_id and index do not generate any obligation
      obligationKinds "transaction_input"
        `shouldBe` [ (".", WrongType PrimArray),
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

-- HELPERS

conwayRoot :: CTreeRoot MonoReferenced
conwayRoot = either (error . toText) id (eraRoot "conway")

conwayObligations :: Text -> [Obligation]
conwayObligations = obligationsFor conwayRoot . Name

dijkstraRoot :: CTreeRoot MonoReferenced
dijkstraRoot = either (error . toText) id (eraRoot "dijkstra")

dijkstraObligations :: Text -> [Obligation]
dijkstraObligations = obligationsFor dijkstraRoot . Name

-- | The shape recorded for each alternative of a rule that is a choice, in the
-- order the specification lists them. This is what tells a sample's branch
-- apart from its siblings, so a branch reading as 'ShapeOther' fits everything
-- and a branch reading as 'ShapeAmbiguous' fits nothing.
branchShapes :: Text -> [Shape]
branchShapes = choiceShapes . conwayObligations

dijkstraObligationKinds :: Text -> [(Text, ObligationKind)]
dijkstraObligationKinds name =
  [ (renderPath (obligationPath obligation), obligationKind obligation)
  | obligation <- dijkstraObligations name
  ]

dijkstraBranchShapes :: Text -> [Shape]
dijkstraBranchShapes = choiceShapes . dijkstraObligations

choiceShapes :: [Obligation] -> [Shape]
choiceShapes obligations =
  map snd . sortOn fst . ordNub $
    [(index, shape) | o <- obligations, StepChoice index shape : _ <- [obligationPath o]]

-- | Where the set folded into @transaction_body@'s first field states what it
-- asks for: once under the tag, once bare.
taggedInputs :: Text
taggedInputs = "map{0}.value.validator.gen.choice[0].tag258"

bareInputs :: Text
bareInputs = "map{0}.value.validator.gen.choice[1]"

-- | The set of owners inside a pool registration certificate, named once so the
-- rows that reach into it stay readable.
owners :: Text
owners = "choice[3].array[7].value.validator.gen"

-- | The rule an obligation names, where it names one.
referenced :: ObligationKind -> [Text]
referenced = \case
  AcceptReference name -> [unName name]
  WrongReference name -> [unName name]
  _ -> []

-- | Every obligation a rule states: where it is stated, and what it asks for.
obligationKinds :: Text -> [(Text, ObligationKind)]
obligationKinds name =
  [ (renderPath (obligationPath obligation), obligationKind obligation)
  | obligation <- conwayObligations name
  ]

-- | What one alternative of a choice states, the alternative named by the
-- leading step of each path.
branchKinds :: Text -> Text -> [(Text, ObligationKind)]
branchKinds name branch =
  [ entry
  | entry@(path, _) <- obligationKinds name,
    path == branch || (branch <> ".") `T.isPrefixOf` path
  ]

kindsOf :: Text -> [ObligationKind]
kindsOf = map obligationKind . conwayObligations

-- | What a fixed length states at one path: the two sizes either side of it,
-- which have to be refused. Nothing states the length itself, since reaching
-- the container in a valid sample forces it.
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
