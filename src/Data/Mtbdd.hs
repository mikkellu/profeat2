module Data.Mtbdd
  ( Id
  , Var(..)
  , Level(..)

  , VarOrder
  , lookupVar
  , lookupLevel

  , Mtbdd
  , numberOfVars
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

  , size
  , allNodes
  , eval
  , sat
  ) where


import qualified Data.HashSet as HashSet

import Data.Maybe (fromMaybe)

import Data.Monoid ((<>))

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V

import Data.Mtbdd.Internal
import Data.VarOrder


isTerminal :: Node t -> Bool
isTerminal (Node _ (Terminal _)) = True
isTerminal _                     = False


isInnerNode :: Node t -> Bool
isInnerNode (Node _ Decision {}) = True
isInnerNode _                    = False


size :: Eq t => Node t -> Int
size = length . allNodes


value :: Node t -> Maybe t
value (Node _ (Terminal v)) = Just v
value _                     = Nothing


allNodes :: Eq t => Node t -> [Node t]
allNodes = HashSet.toList . go HashSet.empty where
    go ms m@(Node _ ty) =
        let ms' = HashSet.insert m ms
        in case ty of
               Terminal _          -> ms'
               Decision _ one zero -> go (go ms' one) zero


eval :: Mtbdd t -> Vector Bool -> t
eval m decisions = go (rootNode m) where
    go (Node _ ty) = case ty of
        Terminal v -> v
        Decision (Var var) one zero
          | lookupDecision var -> go one
          | otherwise          -> go zero
    lookupDecision var = fromMaybe False (decisions !? var)


sat :: (t -> Bool) -> Mtbdd t -> Set (Vector Bool)
sat p (Mtbdd numVars vo root) = go (V.replicate numVars False) (Level 0) root
  where
    go ds lvl@(Level i) node@(Node _ ty) = case ty of
        Terminal v
          | p v -> if i >= numVars
                       then Set.singleton ds
                       else step node node
          | otherwise -> Set.empty
        Decision nodeVar one zero
          | lvl < lookupLevel vo nodeVar -> step node node
          | otherwise                    -> step one zero
      where
        step one zero = go (ds // [(var, True)]) (Level (i + 1)) one <>
                        go (ds // [(var, False)]) (Level (i + 1)) zero
        Var var = lookupVar vo lvl
