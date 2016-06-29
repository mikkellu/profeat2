{-# LANGUAGE LambdaCase #-}

module Data.Proposition
  ( Proposition(..)
  , eval
  , toBdd
  ) where

import Data.Maybe (fromMaybe)

import Data.Vector (Vector, (!?))

import Test.QuickCheck

import Data.Mtbdd hiding (eval)
import Data.Mtbdd.Builder


data Proposition
  = Binary !BinOp !Proposition !Proposition
  | Not !Proposition
  | Variable !Var
  | Literal !Bool
  deriving (Eq, Show)

data BinOp = And | Or deriving (Bounded, Enum, Eq, Show)

instance Arbitrary Proposition where
    arbitrary = oneof
      [ Binary <$> arbitraryBoundedEnum <*> arbitrary <*> arbitrary
      , Not <$> arbitrary
      , Variable . Var <$> choose (0, 5)
      , Literal <$> arbitrary
      ]
    shrink = \case
        Binary op l r -> [l, r] ++ [Binary op l' r' | (l', r') <- shrink (l, r)]
        Not       p   -> p : fmap Not (shrink p)
        _             -> []


eval :: Proposition -> Vector Bool -> Bool
eval p env = case p of
    Binary op l r      -> binOp op (eval l env) (eval r env)
    Not p'             -> not (eval p' env)
    Variable (Var var) -> fromMaybe False (env !? var)
    Literal b          -> b


toBdd :: Proposition -> Mtbdd Bool
toBdd prop = runBuilder $ do
    result <- build prop
    deref result
  where
    build = \case
        Binary op l r -> bindApply (binOp op) (build l) (build r)
        Not p         -> bindMap not (build p)
        Variable var  -> projection var True False
        Literal b     -> constant b


binOp :: BinOp -> (Bool -> Bool -> Bool)
binOp = \case
    And -> (&&)
    Or  -> (||)
