module Coverage.Obligations.TrailSpec where

import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Obligation
import Coverage.Obligations.Obligations (ruleObligations)
import Coverage.Obligations.Primitive
import Coverage.Obligations.Shape
import Coverage.Obligations.SpecRule (eraRoot)
import Coverage.Obligations.Step
import Coverage.Obligations.Trail
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Test.Hspec

spec = describe "The text of a path" $ do
  -- A waiver names its obligation by the path the coverage report shows, so the text must read back into exactly the
  -- steps it was written from.
  it "reads back every path that the specifications state" $
    for_ ["conway", "dijkstra"] $ \era -> do
      let roundTripsPath path = parseTrail (renderTrail path) == Right path
      -- all paths should round-trip correctly
      [path | path <- obligationPaths era, not (roundTripsPath path)] `shouldBe` []

  it "writes the shape of a choice, the bound of an occurrence and the keys of an open entry" $
    renderTrail
      ( trailOf
          [ StepChoice 0 (ShapeTag 258),
            StepTag 258,
            StepMapAmong 3 [LitUInt 0, LitUInt 1],
            StepRepeat 1,
            StepValue
          ]
      )
      `shouldBe` "choice[0:tag258].tag258.map[3?0,1].repeat1.value"

  it "writes a run of consecutive tags in a choice as a range" $
    renderTrail (trailOf [StepChoice 0 (ShapeAnyOf (map ShapeTag [102, 121, 122, 123, 124]))])
      `shouldBe` "choice[0:(tag102/tag121..124)]"

  it "reads back literals that contain dots, quotes, commas and brackets" $
    roundTrips
      [ StepMapKey (LitText "a.b,\"c\"]}"),
        StepMapKey (LitBytes (BS.pack [0, 10, 255])),
        StepMapKey (LitBignum 12),
        StepMapKey (LitNInt 5),
        StepMapKey (LitFloat 1.5),
        StepMapKey (LitBool False),
        StepMapAmong 0 [LitText "x,y", LitUInt 2]
      ]

  it "reads back every kind of shape" $
    roundTrips
      [ StepChoice index shape
      | (index, shape) <-
          zip
            [0 ..]
            [ ShapeArray Nothing,
              ShapeArray (Just 3),
              ShapeMap,
              ShapeTag 30,
              ShapePrimitive PrimNil,
              ShapePrimitive PrimArray,
              ShapePrimitive PrimMap,
              ShapeLiteral (LitText "x"),
              ShapeRange (-9223372036854775808) (-1),
              ShapeAnyOf [ShapePrimitive PrimUInt, ShapeAnyOf [ShapeMap, ShapeTag 2]],
              ShapeOther,
              ShapeAmbiguous (ShapeAnyOf [ShapeTag 258, ShapeArray Nothing])
            ]
      ]

  it "reads the path at the top of a rule" $
    (parseTrail "." :: Either Text (Trail Absolute)) `shouldBe` Right (trailOf [])

  it "refuses a path written in a form it does not know" $
    (parseTrail "choice[0]" :: Either Text (Trail Absolute)) `shouldBe` Left "not a path: choice[0]"

-- * HELPERS

-- | Whether each step, alone and all together, reads back from its text.
roundTrips :: [Step] -> Expectation
roundTrips taken =
  [trail | trail <- trailOf taken : map (trailOf . pure) taken, parseTrail (renderTrail trail) /= Right trail]
    `shouldBe` ([] :: [Trail Absolute])

-- | The path of every obligation of an era.
obligationPaths :: Text -> [Trail Absolute]
obligationPaths era = case eraRoot era of
  Left message -> error message
  Right root@(CTreeRoot rules) -> [obligationPath o | body <- Map.elems rules, o <- ruleObligations root body]
