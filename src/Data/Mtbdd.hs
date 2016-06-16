module Data.Mtbdd
  ( Id
  , Var(..)

  , Mtbdd(..)
  , isTerminal
  , isInnerNode
  , variable
  , value
  ) where

-- | A variable in a binary decision diagram.
newtype Var = Var Int deriving (Eq, Ord, Show)

type Id = Int

-- | A multi-terminal binary decision diagram
data Mtbdd a
  = Terminal !a
  | Node !Id !Var (Mtbdd a) (Mtbdd a)

instance Eq a => Eq (Mtbdd a) where
    Terminal vx == Terminal vy = vx == vy
    Node nx _ _ _ == Node ny _ _ _ = nx == ny

instance Ord a => Ord (Mtbdd a) where
    compare x y = case x of
        Terminal vx -> case y of
            Terminal vy -> compare vx vy
            _           -> LT
        Node nx _ _ _ -> case y of
            Node ny _ _ _ -> compare nx ny
            _             -> GT

isTerminal :: Mtbdd a -> Bool
isTerminal (Terminal _) = True
isTerminal _            = False

isInnerNode :: Mtbdd a -> Bool
isInnerNode Node {} = True
isInnerNode _       = False

variable :: Mtbdd a -> Var
variable (Terminal _)     = Var maxBound
variable (Node _ var _ _) = var

value :: Mtbdd a -> Maybe a
value (Terminal v) = Just v
value _            = Nothing
