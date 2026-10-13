{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to navigate and inspect the rules defined in a Huddle specification.
module HuddleRule
  ( renderRule,
    renderVariant,
    literalText,
    nameText,
    renderOccurrence,
    occurrenceBounds,
    entryWidth,
    totalWidth,
  )
where

import Codec.CBOR.Cuddle.CDDL
  ( Name (..),
    OccurrenceIndicator (..),
    Value (..),
    ValueVariant (..),
  )
import Codec.CBOR.Cuddle.CDDL.CTree (CTree)
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

type HuddleSpec = CTree.CTreeRoot MonoReferenced

-- RENDERING

-- | Print the given rule as a tree, one node per line.
renderRule :: HuddleSpec -> Name -> Maybe [Text]
renderRule (CTree.CTreeRoot rules) name =
  (\tree -> nameText name : go 1 tree) <$> Map.lookup name rules
  where
    pad :: Int -> Text
    pad depth = T.replicate (depth * 2) " "
    branch depth label entries =
      (pad depth <> label <> " (" <> show (length entries) <> ")")
        : concatMap (go (depth + 1)) entries

    go :: Int -> CTree.CTree MonoReferenced -> [Text]
    go depth = \case
      -- The two wrappers a CDDL rendering cannot show.
      CTree.CTreeE (MGenerator _ inner) ->
        (pad depth <> "<custom generator>") : go (depth + 1) inner
      CTree.CTreeE (MValidator _ inner) ->
        (pad depth <> "<custom validator>") : go (depth + 1) inner
      CTree.CTreeE (MRuleRef ref) -> [pad depth <> "-> " <> nameText ref]
      CTree.Choice alternatives ->
        (pad depth <> "choice (" <> show (length alternatives) <> " alternatives)")
          : concat
            [ (pad (depth + 1) <> "[" <> show index <> "]") : go (depth + 2) alternative
            | (index, alternative) <- zip [0 :: Int ..] (NE.toList alternatives)
            ]
      CTree.Array entries -> branch depth "array" entries
      CTree.Map entries -> branch depth "map" entries
      CTree.Group entries -> branch depth "group" entries
      CTree.KV key value cut ->
        (pad depth <> "entry" <> if cut then " (cut)" else "")
          : (pad (depth + 1) <> "key:")
          : go (depth + 2) key
            <> ((pad (depth + 1) <> "value:") : go (depth + 2) value)
      CTree.Occur item occurs ->
        (pad depth <> "occurs " <> renderOccurrence occurs) : go (depth + 1) item
      CTree.Range from to bound ->
        (pad depth <> "range " <> show bound) : go (depth + 1) from <> go (depth + 1) to
      CTree.Control op target controller ->
        (pad depth <> "control " <> show op)
          : go (depth + 1) target
            <> go (depth + 1) controller
      CTree.Enum inner -> (pad depth <> "enum") : go (depth + 1) inner
      CTree.Unwrap inner -> (pad depth <> "unwrap") : go (depth + 1) inner
      CTree.Tag number inner -> (pad depth <> "tag " <> show number) : go (depth + 1) inner
      CTree.Literal (Value value _) -> [pad depth <> "literal " <> renderVariant value]
      CTree.Postlude term -> [pad depth <> show term]

nameText :: Name -> Text
nameText (Name text) = text

renderOccurrence :: OccurrenceIndicator -> Text
renderOccurrence = \case
  OIOptional -> "0 or 1 (?)"
  OIZeroOrMore -> "0 or more (*)"
  OIOneOrMore -> "1 or more (+)"
  OIBounded lower upper ->
    maybe "0" show lower <> " .. " <> maybe "*" show upper

renderVariant :: ValueVariant -> Text
renderVariant = \case
  VText text -> text
  VBytes bytes -> "bytes " <> show (BS.length bytes) <> "B"
  VUInt n -> show n
  VNInt n -> "-" <> show n
  VBignum n -> show n
  VBool b -> show b
  VFloat16 f -> show f
  VFloat32 f -> show f
  VFloat64 d -> show d

-- | Lower bound, and upper bound when there is one.
occurrenceBounds :: OccurrenceIndicator -> (Word64, Maybe Word64)
occurrenceBounds = \case
  OIOptional -> (0, Just 1)
  OIZeroOrMore -> (0, Nothing)
  OIOneOrMore -> (1, Nothing)
  OIBounded lower upper -> (fromMaybe 0 lower, upper)

-- | How many elements one entry contributes to the container holding it.
--
-- A type is one element. A group splices its own entries in, so it contributes
-- as many as they do, which is the same sum one level down. A choice may do
-- either depending on the alternative taken, so it gives up, as does a group
-- that reaches itself: a width that depends on itself is not a width.
entryWidth :: Map Name (CTree MonoReferenced) -> CTree MonoReferenced -> Maybe (Word64, Maybe Word64)
entryWidth rules = widthOf mempty
  where
    widthOf :: Set Name -> CTree MonoReferenced -> Maybe (Word64, Maybe Word64)
    widthOf seen = \case
      CTree.Occur _ occurs -> Just (occurrenceBounds occurs)
      CTree.CTreeE (MRuleRef ref)
        | ref `Set.member` seen -> Nothing
        | otherwise -> case Map.lookup ref rules of
            Just (CTree.Group entries) -> spliced (Set.insert ref seen) entries
            Just (CTree.Choice alternatives) -> agreed (Set.insert ref seen) alternatives
            Just _ -> Just (1, Just 1)
            Nothing -> Nothing
      CTree.CTreeE (MGenerator _ inner) -> widthOf seen inner
      CTree.CTreeE (MValidator _ inner) -> widthOf seen inner
      CTree.Group entries -> spliced seen entries
      CTree.Choice alternatives -> agreed seen alternatives
      CTree.KV {} -> Just (1, Just 1)
      CTree.Literal _ -> Just (1, Just 1)
      CTree.Postlude _ -> Just (1, Just 1)
      CTree.Array _ -> Just (1, Just 1)
      CTree.Map _ -> Just (1, Just 1)
      CTree.Tag _ _ -> Just (1, Just 1)
      CTree.Range {} -> Just (1, Just 1)
      CTree.Control {} -> Just (1, Just 1)
      _ -> Nothing

    -- A choice contributes what its alternatives contribute, but only when they
    -- all contribute the same: @anchor\/ nil@ is one element whichever way it
    -- goes, while alternatives that splice different numbers of entries leave
    -- the container without a length.
    agreed seen alternatives = do
      widths <- traverse (widthOf seen) (toList alternatives)
      case ordNub widths of
        [width] -> Just width
        _ -> Nothing

    spliced seen entries = do
      widths <- traverse (widthOf seen) entries
      uppers <- traverse snd widths
      pure (sum (map fst widths), Just (sum uppers))

-- | How long a container may be: the sum of what its entries contribute.
--
-- 'Nothing' when any entry's own width is unknown, so nothing can be said. An
-- absent upper bound means a repeated entry with no ceiling.
totalWidth ::
  Map Name (CTree MonoReferenced) ->
  [CTree MonoReferenced] ->
  Maybe (Word64, Maybe Word64)
totalWidth rules entries = do
  widths <- traverse (entryWidth rules) entries
  pure (sum (map fst widths), sum <$> traverse snd widths)

literalText :: CTree MonoReferenced -> Maybe Text
literalText = \case
  CTree.Literal (Value variant _) -> Just (renderVariant variant)
  _ -> Nothing
