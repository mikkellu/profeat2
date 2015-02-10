{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Tests.Instances where

import Control.Applicative

import Data.Bdd
import Data.Bdd.Graphviz
import Data.Proposition

import Test.QuickCheck

instance Arbitrary Bdd where
    arbitrary = toBdd <$> arbitrary

instance Show Bdd where
    show = show . renderBdd "test"

