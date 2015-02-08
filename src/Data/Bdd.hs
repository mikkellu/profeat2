{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Binary Decision Diagrams.
module Data.Bdd
  ( -- * Variables
    Variable
  , mkVariable
  , getVariable
  , terminalVariable
    -- * Binary Decision Diagrams
  , Bdd
  , NodeId
  , nodeId
  , isTerminal
  , allNodes
  , eval
  , sat
    -- * Views
  , NodeView(..)
  , viewNode
  ) where

import Data.Monoid
import Data.Sequence                  ( Seq, ViewL(..), (|>), viewl )
import qualified Data.Sequence as Seq
import Data.Set                       ( Set )
import qualified Data.Set as Set

import Data.Bdd.Internal

-- | Check if a 'Bdd' node is terminal.
isTerminal :: Bdd -> Bool
isTerminal (viewNode -> Terminal _) = True
isTerminal _                        = False

-- | Get all nodes of a 'Bdd'.
allNodes :: Bdd -> [Bdd]
allNodes = Set.elems . go Set.empty where
    go ns node = let ns' = Set.insert node ns
                 in case viewNode node of
                        Terminal _     -> ns'
                        Decision _ t e -> go (go ns' t) e

-- | Evaluate the function represented by the given 'Bdd'.
eval :: Bdd -> Seq Bool -> Bool
eval = go 0 where
    go i node val = case viewNode node of
        Terminal b       -> b
        Decision var t e ->
            let (b, vs) = case viewl val of
                              (v :< vs') -> (v, vs')
                              EmptyL     -> (False, val)
            in if | Variable i < var -> go (i + 1) node vs
                  | b                -> go (i + 1) t vs
                  | otherwise        -> go (i + 1) e vs

-- | Get all satisfying variable assignments.
sat :: Int -> Bdd -> Set (Seq Bool)
sat varCount = go Seq.empty 0 where
    go val i node = case viewNode node of
        Terminal False -> Set.empty
        Terminal True
          | i >= varCount -> Set.singleton val
          | otherwise     -> step node node
        Decision var t e
          | Variable i < var -> step node node
          | otherwise        -> step t e
      where
        step t e = go (val |> True) (i + 1) t <> go (val |> False) (i + 1) e


-- | A 'Bdd' node is either terminal (labeled with 0 or 1) or it is
-- a decision node, labeled with a variable and two outgoing edges.
--
-- The left and right sub-'Bdd's represent an assignment of 1 or 0 to the
-- associated variable, respectively.
data NodeView
  = Terminal !Bool
  | Decision !Variable !Bdd !Bdd

-- | View a 'Bdd' as a 'Terminal' or a 'Decision' node.
viewNode :: Bdd -> NodeView
viewNode = \case
    BddTerm b         -> Terminal b
    BddNode _ var t e -> Decision var t e

