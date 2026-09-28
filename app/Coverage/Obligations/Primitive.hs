module Coverage.Obligations.Primitive where

import Data.Aeson (FromJSON, ToJSON)

-- | A CBOR primitive type, as the specification's postlude names it.
--
-- Named here rather than by importing cuddle's own type, for the same reason the control operator is carried as text:
-- nothing should break when a constructor is renamed upstream.
data Primitive
  = PrimUInt
  | PrimNInt
  | PrimInt
  | PrimBytes
  | PrimText
  | PrimBool
  | PrimNil
  | -- | The two container major types. No postlude names them, but a container node has to state what it is: without
    -- this an array rule is satisfied by a map, since its length and its two length forms say nothing about which kind
    -- of container it is.
    PrimArray
  | PrimMap
  deriving (Eq, Show, Ord, Generic)

instance ToJSON Primitive

instance FromJSON Primitive

primitiveNamed :: Text -> Maybe Primitive
primitiveNamed = \case
  "PTUInt" -> Just PrimUInt
  "PTNInt" -> Just PrimNInt
  "PTInt" -> Just PrimInt
  "PTBytes" -> Just PrimBytes
  "PTText" -> Just PrimText
  "PTBool" -> Just PrimBool
  "PTNil" -> Just PrimNil
  _ -> Nothing

renderPrimitive :: Primitive -> Text
renderPrimitive = \case
  PrimUInt -> "uint"
  PrimNInt -> "nint"
  PrimInt -> "int"
  PrimBytes -> "bytes"
  PrimText -> "text"
  PrimBool -> "bool"
  PrimNil -> "nil"
  PrimArray -> "array"
  PrimMap -> "map"

-- | Return the bounds of a primitive type if it admits one
primitiveRange :: Primitive -> Maybe (Integer, Integer)
primitiveRange = \case
  PrimUInt -> Just (0, maxWord)
  PrimNInt -> Just (negate (maxWord + 1), -1)
  PrimInt -> Just (negate (maxWord + 1), maxWord)
  _ -> Nothing
  where
    maxWord = toInteger (maxBound :: Word64)
