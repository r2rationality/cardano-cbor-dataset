module Coverage.Obligations.Step where

import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Shape
import Data.Text qualified as T
import Text.ParserCombinators.ReadP hiding (many, optional)

-- | One step of the path from a rule to where an obligation sits in it.
data Step
  = -- | The alternative at this index of a choice, with the shape that tells it apart from its siblings.
    StepChoice Int Shape
  | -- | The element at this position of an array.
    StepArray Int
  | -- | The element at this position of an array that has one optional element before its last one, with the length
    -- of the array when the optional element is present.
    --
    -- Such an array is one element shorter without the optional element, and every element after it moves back by one.
    -- The length of the array tells the two forms apart, so the position is given for the full form and the length
    -- of the full form goes with it.
    StepArrayOf Int Int
  | -- | The map entry with this literal key.
    StepMapKey LiteralValue
  | -- | The map entry at this position, for an entry whose key is not a literal.
    StepMapAt Int
  | -- | An entry whose key is open sitting among literal-keyed siblings, carrying the keys those siblings claim.
    --
    -- Its own key matches nothing in particular and its position in the schema is not its position in the bytes, so it
    -- is found by elimination instead: an entry keyed by none of its siblings' literals can only be this one.
    StepMapAmong Int [LiteralValue]
  | -- | The entry at this position of a group, which is spliced into the container that holds it.
    StepGroup Int
  | -- | The key of a map entry.
    StepKey
  | -- | The value of a map entry.
    StepValue
  | -- | An entry that appears at most once, carrying its lower bound: zero means whatever sits below it may never appear
    -- in any sample at all.
    StepOccur Word64
  | -- | An entry that can appear more than once, carrying its lower bound. A path through it reaches every occurrence.
    StepRepeat Word64
  | -- | The value under this tag.
    StepTag Word64
  | -- | The target of a control operator, such as @.size@.
    StepControl
  | -- | The value under a custom generator, which the specification adds for the data generation.
    StepGenerator
  | -- | The value under a custom validator, which the specification adds for the data validation.
    StepValidator
  deriving (Eq, Ord, Show)

-- * PATH PREDICATES

-- | Whether a step stays on the value of a map or array entry, rather than going into the value.
staysOnTheEntry :: Step -> Bool
staysOnTheEntry = \case
  StepOccur _ -> True
  StepRepeat _ -> True
  StepKey -> True
  StepValue -> True
  StepControl -> True
  StepGenerator -> True
  StepValidator -> True
  _ -> False

-- | Whether a step stays on the same value: a tag, a control, a generator or a validator.
staysOnTheValue :: Step -> Bool
staysOnTheValue = \case
  StepTag _ -> True
  StepControl -> True
  StepGenerator -> True
  StepValidator -> True
  _ -> False

-- | Whether a step goes to the first element of an array or a group.
leadingPosition :: Step -> Bool
leadingPosition = \case
  StepArray 0 -> True
  StepArrayOf 0 _ -> True
  StepGroup 0 -> True
  _ -> False

-- | Whether a step goes into an element of a map, an array or a group.
isContainerStep :: Step -> Bool
isContainerStep = \case
  StepMapKey _ -> True
  StepMapAt _ -> True
  StepMapAmong _ _ -> True
  StepArray _ -> True
  StepArrayOf _ _ -> True
  StepGroup _ -> True
  _ -> False

-- * RENDERING

-- | A step as a path writes it, which 'stepP' reads back exactly.
renderStep :: Step -> Text
renderStep = \case
  StepChoice index shape -> "choice[" <> show index <> ":" <> writeShape shape <> "]"
  StepArray index -> "array[" <> show index <> "]"
  StepArrayOf index full -> "array[" <> show index <> "/" <> show full <> "]"
  StepMapKey key -> "map{" <> writeLiteral key <> "}"
  StepMapAt index -> "map[" <> show index <> "]"
  StepMapAmong index keys -> "map[" <> show index <> "?" <> T.intercalate "," (map writeLiteral keys) <> "]"
  StepGroup index -> "group[" <> show index <> "]"
  StepKey -> "key"
  StepValue -> "value"
  StepOccur low -> "occur" <> show low
  StepRepeat low -> "repeat" <> show low
  StepTag number -> "tag" <> show number
  StepControl -> "control"
  StepGenerator -> "gen"
  StepValidator -> "validator"

-- | Read a step written by 'renderStep'.
stepP :: ReadP Step
stepP =
  choice
    [ StepChoice <$> (string "choice[" *> naturalP) <*> (char ':' *> shapeP <* char ']'),
      StepArray <$> indexed "array",
      StepArrayOf <$> (string "array[" *> naturalP) <*> (char '/' *> naturalP <* char ']'),
      StepMapKey <$> (string "map{" *> literalP <* char '}'),
      StepMapAt <$> indexed "map",
      StepMapAmong <$> (string "map[" *> naturalP) <*> (char '?' *> sepBy literalP (char ',') <* char ']'),
      StepGroup <$> indexed "group",
      StepKey <$ string "key",
      StepValue <$ string "value",
      StepOccur <$> (string "occur" *> naturalP),
      StepRepeat <$> (string "repeat" *> naturalP),
      StepTag <$> (string "tag" *> naturalP),
      StepControl <$ string "control",
      StepGenerator <$ string "gen",
      StepValidator <$ string "validator"
    ]
  where
    indexed name = string (name <> "[") *> naturalP <* char ']'
