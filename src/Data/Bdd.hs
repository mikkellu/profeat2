{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Binary Decision Diagrams.
module Data.Bdd
  ( -- * Variables
    Variable
    -- * Binary Decision Diagrams
  , Bdd
  , NodeId
  , nodeId
  , isTerminal
  , allNodes
    -- * Views
  , NodeView(..)
  , view
  ) where

import qualified Data.Set as Set

import Data.Bdd.Internal

-- | Check if a 'Bdd' node is terminal.
isTerminal :: Bdd -> Bool
isTerminal (view -> Terminal _) = True
isTerminal _                    = False

-- | Get all nodes of a 'Bdd'.
allNodes :: Bdd -> [Bdd]
allNodes = Set.elems . go Set.empty where
    go ns node = let ns' = Set.insert node ns
                 in case view node of
                        Terminal _     -> ns'
                        Decision _ t e -> go (go ns' t) e


-- | A 'Bdd' node is either terminal (labeled with 0 or 1) or it is
-- a decision node, labeled with a variable and two outgoing edges.
--
-- The left and right sub-'Bdd's represent an assignment of 1 or 0 to the
-- associated variable, respectively.
data NodeView
  = Terminal !Bool
  | Decision !Variable !Bdd !Bdd

-- | View a 'Bdd' as a 'Terminal' or a 'Decision' node.
view :: Bdd -> NodeView
view = \case
    BddTerm b         -> Terminal b
    BddNode _ var t e -> Decision var t e

