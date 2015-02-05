{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Binary Decision Diagrams.
module Data.Bdd
  ( -- * Variables
    Variable
  , mkVariable
  , terminalVariable
    -- * Binary Decision Diagrams
  , Bdd
  , NodeId
  , nodeId
  , isTerminal
  , allNodes
  , sat
    -- * Views
  , NodeView(..)
  , viewNode
  ) where

import Data.Monoid
import Data.Sequence                  ( Seq, (|>) )
import qualified Data.Sequence as Seq
import Data.Set                       ( Set )
import qualified Data.Set as Set

import Data.Bdd.Internal

-- | Check if a 'Bdd' node is terminal.
isTerminal :: Bdd -> Bool
isTerminal (viewNode -> Terminal _) = True
isTerminal _                    = False

-- | Get all nodes of a 'Bdd'.
allNodes :: Bdd -> [Bdd]
allNodes = Set.elems . go Set.empty where
    go ns node = let ns' = Set.insert node ns
                 in case viewNode node of
                        Terminal _     -> ns'
                        Decision _ t e -> go (go ns' t) e

-- | Get all satisfying variable assignments.
sat :: Int -> Bdd -> Set (Seq Bool)
sat varCount = go Seq.empty 0 where
    go val k node = case viewNode node of
        Terminal False -> Set.empty
        Terminal True
          | k == varCount -> Set.singleton val
          | otherwise     -> step node node
        Decision var t e
          | Variable k < var -> step node node
          | otherwise        -> step t e
      where
        step t e = go (val |> True)  (k + 1) t <> go (val |> False) (k + 1) e


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

