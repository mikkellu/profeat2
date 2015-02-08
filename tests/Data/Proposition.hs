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
import Data.Bdd.Builder               ( runBuilder, readRef, proj, true , false
                                      , andB, orB, notB , impliesB )
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

data BinOp
  = And
  | Or
  | Implies
  deriving (Bounded, Enum, Eq, Show)

instance Arbitrary BinOp where arbitrary = arbitraryBoundedEnum

binOp = \case
    And     -> (&&)
    Or      -> (||)
    Implies -> (||) . not

binOpBuilder = \case
    And     -> andB
    Or      -> orB
    Implies -> impliesB

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

