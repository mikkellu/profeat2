{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE LambdaCase #-}

-- | Internal representation of BDD nodes.
module Data.Bdd.Internal
  ( Variable(..)
  , mkVariable
  , getVariable
  , terminalVariable

  , Bdd(..)
  , NodeId
  , nodeId
  , variable

  , Sobdd(..)
  ) where

import Data.Hashable
import Data.Function ( on )
import Data.Ord      ( comparing )

-- | A boolean variable.
newtype Variable = Variable Int deriving (Eq, Ord, Show)

instance Hashable Variable where
    hashWithSalt salt (Variable v) = hashWithSalt salt v
    hash (Variable v)              = hash v

-- | Create a variable with given index.
mkVariable :: Int -> Variable
mkVariable v
  | v == maxBound = error "mkVariable: illegal index"
  | otherwise     = Variable v

-- | Returns the index of a 'Variable'.
getVariable ::Variable -> Int
getVariable (Variable v) = v

-- | Terminal nodes are not associated with a variable, however it is
-- useful to define that the "variable" of the terminal nodes is the
-- greatest element in any given variable ordering.
terminalVariable :: Variable
terminalVariable = Variable maxBound


-- | Internal ID of a BDD node.
type NodeId = Int

-- | A decision diagram node is either a terminal node labeled with 0 or 1,
-- or a decision node labeled with a variable and two outgoing edges.
data Bdd
  = BddTerm !Bool
  | BddNode !NodeId !Variable Bdd Bdd

instance Eq Bdd where
    (==) = (==) `on` nodeId

instance Ord Bdd where
    compare = comparing nodeId

instance Hashable Bdd where
    hashWithSalt salt = hashWithSalt salt . nodeId
    hash              = hash . nodeId

-- | Returns the internal 'NodeId' of a 'BDD' node.
nodeId :: Bdd -> NodeId
nodeId = \case
    BddTerm False     -> 0
    BddTerm True      -> 1
    BddNode nid _ _ _ -> nid

-- | For decision nodes @variable n@ returns the variable the node is
-- labeled with. If the node @n@ is a terminal node, then
-- 'terminalVariable' is returned.
variable :: Bdd -> Variable
variable = \case
    BddTerm _         -> terminalVariable
    BddNode _ var _ _ -> var


-- | A Shared Ordered Binary Decision Diagram.
newtype Sobdd = Sobdd [Bdd]

