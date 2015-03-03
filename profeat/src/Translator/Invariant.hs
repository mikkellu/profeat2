{-# LANGUAGE ViewPatterns #-}

module Translator.Invariant
  ( Invariants(..)
  , genInvariantGuard
  ) where

import Control.Lens

import Data.Map                  ( Map )
import qualified Data.Map as Map

import Syntax
import Syntax.Util
import Template

newtype Invariants = Invariants [LExpr]

-- | Generate the invariant guard. The 'Update's have to be translated
-- already.
genInvariantGuard :: Invariants -> [LUpdate] -> LExpr
genInvariantGuard (Invariants invs) upds =
    let defs   = assignments upds
        idents = Map.keys defs
    in substitute defs . view conjunction . filterInvariants idents $ invs

filterInvariants :: [Ident] -> [LExpr] -> [LExpr]
filterInvariants idents = filter (idents `occurIn`)

assignments :: [LUpdate] -> Map Ident LExpr
assignments = Map.unions . fmap go . toListOf (traverse.to updAssign.ones)
  where
    go (Assign (viewIdent -> Just ident) e _) = Map.singleton ident e
    go _ = error "Translator.Invariant.assignments: illegal Assign"

occurIn :: [Ident] -> LExpr -> Bool
occurIn idents e = any (`occursIn` e) idents

occursIn :: Ident -> LExpr -> Bool
occursIn ident = any isIdentExpr . universe where
    isIdentExpr (viewIdentExpr -> Just i) = i == ident
    isIdentExpr _                         = False

