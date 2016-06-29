module Data.Mtbdd
  ( Id
  , Var(..)
  , Level(..)

  , VarOrder
  , lookupVar
  , lookupLevel

  , Mtbdd
  , varOrder
  , rootNode

  , Node(..)
  , NodeType(..)
  , nodeId
  , isTerminal
  , isInnerNode
  , level
  , variable
  , value

  , allNodes
  , eval
  ) where

import Data.Maybe (fromMaybe)

import qualified Data.HashSet as Set

import Data.Vector (Vector, (!?))

import Data.Mtbdd.Internal
import Data.VarOrder


isTerminal :: Node t -> Bool
isTerminal (Node _ (Terminal _)) = True
isTerminal _                     = False


isInnerNode :: Node t -> Bool
isInnerNode (Node _ Decision {}) = True
isInnerNode _                    = False



variable :: VarOrder -> Node t -> Var
variable vo (Node _ ty) = case ty of
    Terminal _       -> Var maxBound
    Decision lvl _ _ -> lookupVar vo lvl


value :: Node t -> Maybe t
value (Node _ (Terminal v)) = Just v
value _                     = Nothing


allNodes :: Eq t => Mtbdd t -> [Node t]
allNodes = Set.toList . go Set.empty . rootNode where
    go ms m@(Node _ ty) =
        let ms' = Set.insert m ms
        in case ty of
               Terminal _          -> ms'
               Decision _ one zero -> go (go ms' one) zero


eval :: Mtbdd t -> Vector Bool -> t
eval m decisions = go (rootNode m) where
    go (Node _ ty) = case ty of
        Terminal v -> v
        Decision lvl one zero
          | lookupDecision lvl -> go one
          | otherwise          -> go zero
    lookupDecision lvl = fromMaybe False (decisions !? var)
      where
        Var var = lookupVar (varOrder m) lvl
