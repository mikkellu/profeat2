{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  ) where

import Data.Function (on)
import Data.Ord (comparing)
import Data.Hashable (Hashable)

-- | A variable in a binary decision diagram.
newtype Var = Var Int deriving (Eq, Ord, Show, Hashable)


type Id = Int


-- | A multi-terminal binary decision diagram
data Mtbdd t = Mtbdd !Id !(Node t)

nodeId :: Mtbdd t -> Id
nodeId (Mtbdd nid _) = nid

data Node t
  = Terminal !t
  | Node !Var (Mtbdd t) (Mtbdd t)

instance Eq a => Eq (Mtbdd a) where
    (==) = (==) `on` nodeId

instance Ord a => Ord (Mtbdd a) where
    compare = comparing nodeId


isTerminal :: Mtbdd a -> Bool
isTerminal (Mtbdd _ (Terminal _)) = True
isTerminal _                      = False


isInnerNode :: Mtbdd a -> Bool
isInnerNode (Mtbdd _ Node {}) = True
isInnerNode _                 = False


variable :: Mtbdd a -> Var
variable (Mtbdd _ n) = case n of
    Terminal _   -> Var maxBound
    Node var _ _ -> var


value :: Mtbdd a -> Maybe a
value (Mtbdd _ (Terminal v)) = Just v
value _                      = Nothing
