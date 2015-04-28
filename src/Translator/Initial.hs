{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Translator.Initial
  ( InitExprs(..)
  , genInit
  ) where

import Control.Lens

import Data.List
import Data.Monoid

import Symbols
import Syntax

import Syntax.Util

import Translator.Invariant
import Translator.Names

newtype InitExprs = InitExprs [LExpr]

instance Monoid InitExprs where
    mempty = InitExprs []
    mappend (InitExprs x) (InitExprs y) = InitExprs (x <> y)

genInit :: InitExprs -> Invariants -> FeatureSymbol -> LDefinition
genInit (InitExprs initExprs) (Invariants invs) root =
    let ctxs      = allContexts root
        cardExprs = fmap featureExpr ctxs

        e = view conjunction . concat $ [cardExprs, invs, initExprs]
    in InitDef (Init e noLoc)

featureExpr :: FeatureContext -> LExpr
featureExpr ctx =
    let fs             = ctx^.this
        (lower, upper) = fs^.fsGroupCard
        childs         = fs^..fsChildren.traverse.traverse
        n              = genericLength childs

        childCtxs             = fmap (`extendContext` ctx) childs
        (optCtxs, nonOptCtxs) = partition (^.this.fsOptional) childCtxs

        nOpt = genericLength optCtxs

        l = (intExpr lower - intExpr nOpt) `lte` sum (fmap activeExpr nonOptCtxs)
        u = sum (fmap activeExpr childCtxs) `lte` intExpr upper

    in if (lower, upper) == (n, n)
           then BoolExpr True noLoc -- all non-optional features are mandatory
                                    -- in this case, no constraint is
                                    -- necessary, because mandatory
                                    -- features have no variable, and the
                                    -- value for optional features can be
                                    -- chosen arbitrarily
           else l `lAnd` u

activeExpr :: FeatureContext -> LExpr
activeExpr ctx = NameExpr (activeName ctx) noLoc

