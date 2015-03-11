-- Tests.hs -- {{
-- vim: set foldmethod=marker foldmarker={{,}} foldtext=foldtext() :
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Main
  ( main
  ) where

import qualified Data.Bdd      as Bdd
import qualified Data.Foldable as F
import Data.Proposition        as Prop

import Test.Tasty
import Test.Tasty.QuickCheck

import Tests.Instances ()
import Tests.Util

main = defaultMain tests
-- }}
-- Tests -- {{
tests = testGroup "Tests"
  [ testProperty "All satisfying valuations evaluate to true" $
        \f -> F.all (Bdd.eval f) (sat' f)
  , testProperty "BDD is equivalent to represented function" $
        \f (Valuation val) -> Bdd.eval (toBdd f) val == Prop.eval f val
  ]

-- }}

