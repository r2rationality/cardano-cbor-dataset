module Coverage.Obligations.Shape where

import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Primitive
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Traversable
import Text.ParserCombinators.ReadP hiding (many, optional)

-- | Enough of a choice alternative's shape to tell which branch a term took.
--
-- The generator picks a branch and the encoding keeps no record of which, so navigation has to infer it. A container's
-- major type settles it whenever the alternatives differ in kind, which is the common case; two alternatives of the
-- same kind stay ambiguous and the validation trace is what would lift that.
data Shape
  = -- | An array, carrying the leading @uint@ when the alternative starts with
    -- one. Cardano's unions are all discriminated that way, and without it two
    -- array alternatives are indistinguishable: a two-element @drep@ would
    -- answer for the one-element branch as readily as its own.
    ShapeArray (Maybe Word64)
  | ShapeMap
  | ShapeTag Word64
  | -- | A branch that is a bare primitive. @anchor \/ nil@ is told apart by this and nothing else, and without it the
    -- nil branch resolves for a sample that took the other one.
    ShapePrimitive Primitive
  | -- | A branch that is one value. @network_id@ is @0 \/ 1@: both are uints, so nothing but the value itself separates
    -- them, and without this each branch answers for a sample that took the other.
    ShapeLiteral LiteralValue
  | -- | A branch that is itself a choice, carrying what its own alternatives admit.
    --
    -- @plutus_data@ has @%constr<plutus_data>@ and @big_int@ side by side, and each is a choice of its own. Left opaque
    -- both would fit every term, so neither could be told from the other and nothing stated under either could be
    -- answered.
    ShapeAnyOf [Shape]
  | -- | A branch that is a span of numbers, both ends included.
    --
    -- @nonzero_int64@ is @negative_int64 \/ positive_int64@: both are integers, so only where a value falls separates
    -- them. Two spans that overlap are still indistinguishable and are left so by the ambiguity check.
    ShapeRange Integer Integer
  | ShapeOther
  | -- | A branch sharing its shape with a sibling, carrying that shape. Nothing in a decoded term says which of the two
    -- a sample took, so a term of that shape reaches both, and what is stated under each is judged on it.
    ShapeAmbiguous Shape
  deriving (Eq, Show, Ord, Generic)

instance ToJSON Shape

instance FromJSON Shape

-- | Every combination the alternatives admit somewhere, position by position.
--
-- Wider than what they admit as whole combinations, and the difference is the interesting part: an element that is fine
-- where it sits, in company no alternative allows.
byPosition :: [[[Shape]]] -> [[Shape]]
byPosition signatures = for (transpose signatures) (ordNub . concat)

-- | The shape that finds an alternative among the shapes of all the alternatives of its choice.
--
-- Two alternatives of the same shape cannot be told apart in a decoded term, so a path into either would resolve for a
-- sample that took the other and answer about the wrong value. Such a branch is left opaque, which is what suppresses
-- everything stated under it.
tellsBranchApart :: [Shape] -> Shape -> Shape
tellsBranchApart shapes shape
  | length (filter (== shape) shapes) == 1 = shape
  | otherwise = ShapeAmbiguous shape

-- * RENDERING

-- | One element-shape combination, as it reads in an obligation.
-- | A shape as a path writes it, which 'shapeP' reads back exactly: @array@, @array(3)@, @map@, @tag258@, a primitive
-- such as @uint@ or @nil@, @=0@ for a literal, @0..255@ for a range, @(uint/nil)@ for a choice, @any@ and @ambiguous-array@.
-- The container primitives are written @prim(array)@ and @prim(map)@, so they cannot be read as the array and map
-- shapes.
writeShape :: Shape -> Text
writeShape = \case
  ShapeArray Nothing -> "array"
  ShapeArray (Just discriminant) -> "array(" <> show discriminant <> ")"
  ShapeMap -> "map"
  ShapeTag number -> "tag" <> show number
  ShapePrimitive PrimArray -> "prim(array)"
  ShapePrimitive PrimMap -> "prim(map)"
  ShapePrimitive primitive -> renderPrimitive primitive
  ShapeLiteral value -> "=" <> writeLiteral value
  ShapeRange low high -> show low <> ".." <> show high
  ShapeAnyOf shapes -> "(" <> T.intercalate "/" (map writeRun (tagRuns shapes)) <> ")"
  ShapeOther -> "any"
  ShapeAmbiguous shape -> "ambiguous-" <> writeShape shape

-- | Read a shape written by 'writeShape'.
shapeP :: ReadP Shape
shapeP =
  choice
    [ ShapeArray Nothing <$ string "array",
      ShapeArray . Just <$> (string "array(" *> naturalP <* char ')'),
      ShapeMap <$ string "map",
      ShapeTag <$> (string "tag" *> naturalP),
      ShapePrimitive PrimArray <$ string "prim(array)",
      ShapePrimitive PrimMap <$ string "prim(map)",
      choice [ShapePrimitive primitive <$ string (toString (renderPrimitive primitive)) | primitive <- scalars],
      ShapeLiteral <$> (char '=' *> literalP),
      ShapeRange <$> integerP <*> (string ".." *> integerP),
      ShapeAnyOf . concat <$> between (char '(') (char ')') (sepBy (tagRunP +++ (pure <$> shapeP)) (char '/')),
      ShapeOther <$ string "any",
      ShapeAmbiguous <$> (string "ambiguous-" *> shapeP)
    ]
  where
    scalars = [PrimUInt, PrimNInt, PrimInt, PrimBytes, PrimText, PrimBool, PrimNil]

-- | The shapes of a choice, with each run of three or more consecutive tags kept together. @plutus_data@ admits 129
-- tags, and most of them are two runs.
tagRuns :: [Shape] -> [Either (Word64, Word64) Shape]
tagRuns = \case
  ShapeTag low : rest
    | (high, others) <- consecutive low rest,
      high >= low + 2 ->
        Left (low, high) : tagRuns others
  shape : rest -> Right shape : tagRuns rest
  [] -> []
  where
    consecutive previous = \case
      ShapeTag next : rest | next == previous + 1 -> consecutive next rest
      rest -> (previous, rest)

-- | A run of tags as @tag121..127@, or one shape.
writeRun :: Either (Word64, Word64) Shape -> Text
writeRun = \case
  Left (low, high) -> "tag" <> show low <> ".." <> show high
  Right shape -> writeShape shape

-- | Read a run of tags written by 'writeRun'.
tagRunP :: ReadP [Shape]
tagRunP = do
  low <- string "tag" *> naturalP
  high <- string ".." *> naturalP
  if high >= low + 2 then pure (map ShapeTag [low .. high]) else pfail

-- | A natural number, in decimal.
naturalP :: (Read a) => ReadP a
naturalP = munch1 isDigit >>= maybe pfail pure . readMaybe

-- | An integer, in decimal, with an optional minus sign.
integerP :: ReadP Integer
integerP = (<>) <$> option "" (string "-") <*> munch1 isDigit >>= maybe pfail pure . readMaybe

renderShapes :: [Shape] -> Text
renderShapes shapes = "(" <> T.intercalate ", " (map renderShape shapes) <> ")"

renderShape :: Shape -> Text
renderShape = \case
  ShapeArray Nothing -> "array"
  ShapeArray (Just discriminant) -> "array opening with " <> show discriminant
  ShapeMap -> "map"
  ShapeTag number -> "tag " <> show number
  ShapePrimitive primitive -> renderPrimitive primitive
  ShapeLiteral value -> "the value " <> renderLiteral value
  ShapeRange low high -> "a value from " <> show low <> " through " <> show high
  ShapeAnyOf shapes -> "any of " <> show (map renderShape shapes)
  ShapeOther -> "any value"
  ShapeAmbiguous _ -> "an indistinguishable alternative"
