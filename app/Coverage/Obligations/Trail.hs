module Coverage.Obligations.Trail where

import Coverage.Obligations.LiteralValue
import Coverage.Obligations.Shape
import Coverage.Obligations.Step
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.List (nub)
import Data.Sequence (ViewR (..), viewr, (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Text.ParserCombinators.ReadP hiding (many, optional)
import Text.Show (Show (..))

-- | Steps down a rule, in reading order.
--
-- The origin says where the first step starts. An 'Absolute' trail starts at the rule itself, and a 'Relative' one
-- starts at some point below it. The two read the same way, so the type keeps a trail of one kind from being used
-- where the other is expected.
newtype Trail origin = Trail (Seq Step)
  deriving (Eq, Ord)

-- | The origin of a trail that starts at the rule.
data Absolute

-- | The origin of a trail that starts at some point below the rule.
data Relative

-- | Shown as the list of its steps, which is how a reader writes a path down.
instance Show (Trail origin) where
  showsPrec precedence = showsPrec precedence . steps

-- | Written as its text, which 'parseTrail' reads back exactly.
instance ToJSON (Trail origin) where
  toJSON = toJSON . renderTrail

instance FromJSON (Trail origin) where
  parseJSON = withText "path" (either (fail . toString) pure . parseTrail)

-- | The trail of the rule starting from the root, with no steps yet
startTrail :: Trail Absolute
startTrail = Trail Seq.empty

-- | The trail made of these steps, in reading order.
trailOf :: [Step] -> Trail origin
trailOf = Trail . Seq.fromList

-- | Add one step further down.
descend :: Step -> Trail origin -> Trail origin
descend step (Trail trail) = Trail (trail |> step)

-- | Return the steps of a trail, in reading order.
steps :: Trail origin -> [Step]
steps (Trail trail) = toList trail

-- | The last step taken, if any.
latest :: Trail origin -> Maybe Step
latest (Trail trail) = case viewr trail of
  EmptyR -> Nothing
  _ :> step -> Just step

-- | The steps a trail takes past another one, when it goes through it.
--
-- With one letter for each step:
--
-- > stepsBelowPrefix a.b a.b.c.d == Just c.d
-- > stepsBelowPrefix a.b a.b     == Just .
-- > stepsBelowPrefix a.b a.x.c   == Nothing
-- > stepsBelowPrefix a.b.c a.b   == Nothing
stepsBelowPrefix :: Trail Absolute -> Trail Absolute -> Maybe (Trail Relative)
stepsBelowPrefix (Trail above) (Trail trail)
  | start == above = Just (Trail rest)
  | otherwise = Nothing
  where
    (start, rest) = Seq.splitAt (Seq.length above) trail

renderTrail :: Trail origin -> Text
renderTrail trail = case steps trail of
  [] -> "."
  taken -> T.intercalate "." (map renderStep taken)

-- | Read a trail written by 'renderTrail': @.@ for the trail at the rule, or its steps joined by dots.
parseTrail :: Text -> Either Text (Trail origin)
parseTrail text = case nub [trail | (trail, "") <- readP_to_S trailP (toString text)] of
  [trail] -> Right trail
  [] -> Left ("not a path: " <> text)
  _ -> Left ("a path with several readings: " <> text)
  where
    trailP = (trailOf [] <$ char '.') +++ (trailOf <$> sepBy1 stepP (char '.'))

-- | The steps from the bottom of a trail up to the rule.
upwards :: Trail origin -> [Step]
upwards (Trail trail) = toList (Seq.reverse trail)

-- * PREDICATES

-- | Whether a trail goes to one element of an array and then stays on it.
directlyUnderAnEntry :: Trail Relative -> Bool
directlyUnderAnEntry trail = case steps trail of
  StepArray _ : rest -> all staysOnTheEntry rest
  StepArrayOf _ _ : rest -> all staysOnTheEntry rest
  _ -> False

-- | Whether a trail lands on the same value the branch does.
--
-- Reaching what is stated beyond one of them is reaching the branch; a step into a container or past an occurrence is
-- not, since the sub-term it finds may be absent or may be one of several.
reachesWithoutDescending :: Trail Relative -> Bool
reachesWithoutDescending = all staysOnTheValue . steps

-- | Whether a trail is directly under a control.
--
-- A control already states the ceiling it allows, and the type's own is above it. Asking for the type's would ask for
-- a value the specification refuses, so under one the type states no boundary of its own.
underAControl :: Trail Absolute -> Bool
underAControl trail = latest trail == Just StepControl

-- | Whether a trail goes through an entry whose occurrence admits none of it.
--
-- Such an entry may never appear in a sample, so whether the corpus reaches it at all is an open question.
mayGoUnreached :: Trail Absolute -> Bool
mayGoUnreached = any (`elem` [StepOccur 0, StepRepeat 0]) . steps

-- | Whether a trail goes through a choice branch that fits any term, or that cannot be told apart from another one.
underAnOpenBranch :: Trail Absolute -> Bool
underAnOpenBranch = any opensTheBranch . steps
  where
    opensTheBranch = \case
      StepChoice _ ShapeOther -> True
      StepChoice _ (ShapeAmbiguous _) -> True
      _ -> False

-- | Whether the choice just above was picked by this tag.
--
-- Navigation picks a choice branch by what the term looks like, so a value the branch was picked by is not open:
-- asking a sample to carry it repeats the branch obligation, and asking for anything else is unsatisfiable, because
-- navigation refused every other value on the way in.
choiceChoseTag :: Word64 -> Trail Absolute -> Bool
choiceChoseTag number trail = case latest trail of
  Just (StepChoice _ (ShapeTag chosen)) -> chosen == number
  _ -> False

-- | Whether the choice just above was picked by a span of numbers.
--
-- Navigation picks such a branch only for an integer, so a value that is not an integer can never be asked for there:
-- the choice states it instead, as a value that matches none of its alternatives.
choiceChoseRange :: Trail Absolute -> Bool
choiceChoseRange trail = case latest trail of
  Just (StepChoice _ (ShapeRange _ _)) -> True
  _ -> False

-- | Whether the choice above was picked by this value, as the first element of an array.
choiceChoseValue :: LiteralValue -> Trail Absolute -> Bool
choiceChoseValue value trail = case (value, span leadingPosition (upwards trail)) of
  (LitUInt number, (leading, StepChoice _ (ShapeArray (Just chosen)) : _)) ->
    not (null leading) && number == chosen
  _ -> False

-- | Whether the key of a map entry appears in the bytes and is not already used to find the entry.
--
-- A keyed entry is found by its key, so asking for a key other than the one just looked up is unsatisfiable, and
-- asking for the one just looked up is vacuous; what that entry really owes is the required-key pair, stated at the
-- map. Under an array or a group the key is a label that never reaches the bytes at all. A repeated entry is found by
-- position, so there its key is genuinely open.
keyIsEncoded :: Trail Absolute -> Bool
keyIsEncoded trail = case dropWhile (not . isContainerStep) (upwards trail) of
  StepMapAt _ : _ -> True
  StepMapAmong _ _ : _ -> True
  _ -> False
