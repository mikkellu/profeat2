{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Mtbdd
  ( Id
  , Var(..)

  , Mtbdd(..)
  , Node(..)
  , nodeId
  , isTerminal
  , isInnerNode
  , variable
  , value

  , allNodes
  , eval
  ) where

import Data.Function (on)
import Data.Hashable
import Data.Ord (comparing)
import qualified Data.HashSet as Set


-- | A variable in a binary decision diagram.
newtype Var = Var Int deriving (Eq, Ord, Show, Hashable)


type Id = Int


-- | A multi-terminal binary decision diagram
data Mtbdd t = Mtbdd !Id !(Node t)

instance Hashable (Mtbdd t) where
    hashWithSalt salt = hashWithSalt salt . nodeId
    hash              = hash . nodeId

nodeId :: Mtbdd t -> Id
nodeId (Mtbdd nid _) = nid

data Node t
  = Terminal !t
  | Node !Var (Mtbdd t) (Mtbdd t)

instance Eq t => Eq (Mtbdd t) where
    (==) = (==) `on` nodeId

instance Ord t => Ord (Mtbdd t) where
    compare = comparing nodeId


isTerminal :: Mtbdd t -> Bool
isTerminal (Mtbdd _ (Terminal _)) = True
isTerminal _                      = False


isInnerNode :: Mtbdd t -> Bool
isInnerNode (Mtbdd _ Node {}) = True
isInnerNode _                 = False


variable :: Mtbdd t -> Var
variable (Mtbdd _ node) = case node of
    Terminal _   -> Var maxBound
    Node var _ _ -> var


value :: Mtbdd t -> Maybe t
value (Mtbdd _ (Terminal v)) = Just v
value _                      = Nothing


allNodes :: Eq t => Mtbdd t -> [Mtbdd t]
allNodes = Set.toList . go Set.empty where
    go ms m@(Mtbdd _ node) =
        let ms' = Set.insert m ms
        in case node of
               Terminal _      -> ms'
               Node _ one zero -> go (go ms' one) zero


eval :: Mtbdd t -> [Bool] -> t
eval = go 0 where
    go i mtbdd@(Mtbdd _ node) decisions = case node of
        Terminal v -> v
        Node var one zero ->
            let (d, decisions') = next decisions
            in if | Var i < var -> go (i + 1) mtbdd decisions'
                  | d           -> go (i + 1) one decisions'
                  | otherwise   -> go (i + 1) zero decisions'

    next (d:ds) = (d, ds)
    next []     = (False, [])

