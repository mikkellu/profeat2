module Data.Mtbdd.Internal
  ( Id

  , Mtbdd(..)
  , numberOfVars
  , varOrder
  , rootNode

  , Node(..)
  , NodeType(..)

  , nodeId
  , variable
  , level
  ) where

import Data.Function (on)

import Data.Ord (comparing)

import Data.Hashable

import Data.VarOrder


type Id = Int


data Mtbdd t = Mtbdd !Int !VarOrder (Node t)

numberOfVars :: Mtbdd t -> Int
numberOfVars (Mtbdd numVars _ _) = numVars

varOrder :: Mtbdd t -> VarOrder
varOrder (Mtbdd _ vo _) = vo

rootNode :: Mtbdd t -> Node t
rootNode (Mtbdd _ _ node) = node


data Node t = Node !Id !(NodeType t)

instance Hashable (Node t) where
    hashWithSalt salt = hashWithSalt salt . nodeId
    hash              = hash . nodeId


data NodeType t
  = Terminal !t
  | Decision !Var (Node t) (Node t)

instance Eq (Node t) where
    (==) = (==) `on` nodeId

instance Ord (Node t) where
    compare = comparing nodeId


nodeId :: Node t -> Id
nodeId (Node nid _) = nid


variable :: Node t -> Var
variable (Node _ ty) = case ty of
    Terminal _       -> Var maxBound
    Decision var _ _ -> var


level :: VarOrder -> Node t -> Level
level vo (Node _ ty) = case ty of
    Terminal _       -> Level maxBound
    Decision var _ _ -> lookupLevel vo var
