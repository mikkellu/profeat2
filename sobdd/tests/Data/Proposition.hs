{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Proposition
  ( Valuation(..)
  , Proposition(..)
  , BinOp(..)

  , eval
  , toBdd
  ) where

import Control.Applicative

import Data.Bdd                       ( Bdd, Variable, mkVariable, getVariable )
import Data.Bdd.Builder               ( runBuilder, readRef, proj, true ,false
                                      , andB, nandB, orB, norB, notB , impliesB
                                      , xorB, xnorB )
import Data.Sequence                  ( Seq )
import qualified Data.Sequence as Seq

import Test.QuickCheck

newtype Valuation = Valuation { getValuation :: Seq Bool } deriving (Show)

instance Arbitrary Valuation where
    arbitrary = Valuation . Seq.fromList <$> arbitrary

data Proposition
  = Binary !BinOp      !Proposition !Proposition
  | Not    !Proposition
  | Var    !Variable
  | Lit    !Bool
  deriving (Eq, Show)

instance Arbitrary Proposition where
    arbitrary = oneof
      [ Binary <$> arbitrary <*> arbitrary <*> arbitrary
      , Not    <$> arbitrary
      , Var . mkVariable <$> choose (0, 5)
      , Lit    <$> arbitrary
      ]
    shrink = \case
        Binary op l r -> [l, r] ++ [Binary op l' r' | (l', r') <- shrink (l, r)]
        Not       p   -> p : fmap Not (shrink p)
        _             -> []

data BinOp
  = And
  | NAnd
  | Or
  | NOr
  | Implies
  | Xor
  | XNor
  deriving (Bounded, Enum, Eq, Show)

instance Arbitrary BinOp where arbitrary = arbitraryBoundedEnum

binOp = \case
    And     -> (&&)
    NAnd    -> n (&&)
    Or      -> (||)
    NOr     -> n (||)
    Implies -> (||) . not
    Xor     -> (/=)
    XNor    -> (==)
  where
    n op = \x y -> not (x `op` y)

binOpBuilder = \case
    And     -> andB
    NAnd    -> nandB
    Or      -> orB
    NOr     -> norB
    Implies -> impliesB
    Xor     -> xorB
    XNor    -> xnorB

eval :: Proposition -> Seq Bool -> Bool
eval p env = case p of
    Binary op l r -> binOp op (eval l env) (eval r env)
    Not p'        -> not (eval p' env)
    Var (getVariable -> var)
      | var >= Seq.length env -> False
      | otherwise             -> Seq.index env var
    Lit b -> b

toBdd :: Proposition -> Bdd
toBdd prop = runBuilder (fmap readRef (build prop)) where
    build = \case
        Binary op l r -> binOpBuilder op (build l) (build r)
        Not p         -> notB (build p)
        Var var       -> proj var
        Lit True      -> return true
        Lit False     -> return false

