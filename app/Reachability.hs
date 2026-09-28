{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Which rules of an era's specification the corpus roots can reach at all.
-- The specification is the Huddle value the generator itself runs on.
module Reachability
  ( RuleFacts (..),
    Reachability (..),
    ReachabilityStats (..),
    reachabilityStats,
    eraReachability,
    eraHuddle,
    eraRoot,
  )
where

import Cardano.Ledger.Conway.HuddleSpec (conwayCDDL)
import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.Huddle qualified as Cuddle
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Cardano.Ledger.Binary.Cuddle (resolveHuddle)
import Prelude hiding (all)

-- | What one rule's tree contains: the rules it names, and whether it carries
-- behaviour that a @.cddl@ rendering would not show.
data RuleFacts = RuleFacts
  { ruleFactsRefs :: !(Set Name),
    ruleFactsCustomGen :: !Bool,
    ruleFactsCustomValidator :: !Bool
  }
  deriving stock (Eq, Show)

instance Semigroup RuleFacts where
  (<>) (RuleFacts r1 c1 v1) (RuleFacts r2 c2 v2) =
    RuleFacts (Set.union r1 r2) (c1 || c2) (v1 || v2)

instance Monoid RuleFacts where
  mempty = RuleFacts Set.empty False False
  mappend = (<>)

-- REACHABILITY

data Reachability = Reachability
  { all :: !(Map Name RuleFacts),
    fromRoots :: !(Set Name),
    unreached :: !(Set Name),
    missingRoots :: !(Set Name)
  }
  deriving stock (Eq, Show)

data ReachabilityStats = ReachabilityStats
  { totalRules :: !Int,
    reachableRules :: !Int,
    unreachableRules :: !Int,
    missingRootsCount :: !Int
  }
  deriving stock (Eq, Show)

reachabilityStats :: Reachability -> ReachabilityStats
reachabilityStats result =
  ReachabilityStats
    { totalRules = Map.size (all result),
      reachableRules = Set.size (fromRoots result),
      unreachableRules = Set.size (unreached result),
      missingRootsCount = Set.size (missingRoots result)
    }

-- | Traverse the Huddle specification to find which rules are reachable from the given corpus roots.
analyze ::
  -- | The specification to analyze.
  CTreeRoot MonoReferenced ->
  -- | The corpus roots to start from.
  [String] ->
  -- | The reachability analysis result.
  Reachability
analyze (CTreeRoot rules) roots =
  Reachability
    { all = facts,
      fromRoots = reached,
      unreached = Map.keysSet facts `Set.difference` reached,
      missingRoots = Set.fromList rootNames `Set.difference` Map.keysSet facts
    }
  where
    facts = Map.map factsFor rules
    rootNames = map (Name . T.pack) roots
    reached = closure Set.empty rootNames
    closure seen = \case
      [] -> seen
      name : rest
        | name `Set.member` seen -> closure seen rest
        | otherwise -> case Map.lookup name facts of
            Nothing -> closure seen rest
            Just rule -> closure (Set.insert name seen) (Set.toList (ruleFactsRefs rule) <> rest)

-- | A custom generator or validator wraps the subtree it applies to, so the
-- walk has to go through it. Stopping there would lose every reference below
-- the collection rules, which is most of the specification.
factsFor :: CTree MonoReferenced -> RuleFacts
factsFor = go mempty
  where
    go acc = \case
      CTree.CTreeE (MRuleRef name) ->
        acc {ruleFactsRefs = Set.insert name (ruleFactsRefs acc)}
      CTree.CTreeE (MGenerator _ inner) ->
        go acc {ruleFactsCustomGen = True} inner
      CTree.CTreeE (MValidator _ inner) ->
        go acc {ruleFactsCustomValidator = True} inner
      tree -> foldl' go acc (children tree)

-- | Extract subtrees to traverse
children :: CTree MonoReferenced -> [CTree MonoReferenced]
children = \case
  CTree.Literal _ -> []
  CTree.Postlude _ -> []
  CTree.Map entries -> entries
  CTree.Array entries -> entries
  CTree.Choice alternatives -> NE.toList alternatives
  CTree.Group entries -> entries
  CTree.KV k v _ -> [k, v]
  CTree.Occur item _ -> [item]
  CTree.Range from to _ -> [from, to]
  CTree.Control _ target controller -> [target, controller]
  CTree.Enum inner -> [inner]
  CTree.Unwrap inner -> [inner]
  CTree.Tag _ inner -> [inner]
  -- traversed by factsFor
  CTree.CTreeE _ -> []

-- ERA SPECIFICATIONS

eraReachability :: String -> [String] -> Either String Reachability
eraReachability era roots = eraRoot era >>= \root -> pure $ analyze root roots

eraRoot :: String -> Either String (CTreeRoot MonoReferenced)
eraRoot era = do
  huddle <- eraHuddle era
  resolveHuddle huddle

-- | The era's Huddle value, resolved with the ledger's own helper, which is the
-- same call @generate-cbor@ makes. Measuring a different tree from the one that
-- generates the data would be measuring the wrong thing.
eraHuddle :: String -> Either String Cuddle.Huddle
eraHuddle = \case
  "conway" -> Right conwayCDDL
  "dijkstra" -> Right dijkstraCDDL
  other -> Left $ "no Huddle specification for era '" <> other <> "'"
