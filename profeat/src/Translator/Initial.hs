{-# LANGUAGE LambdaCase #-}

module Translator.Initial
  ( genInit
  ) where

import Control.Lens

import Data.Foldable ( toList )
import Data.List

import Symbols
import Syntax

import Analysis.InitialState

import Translator.Constraints
import Translator.Names

genInit :: SymbolTable -> InitialConstraintSet -> ConstraintSet -> LDefinition
genInit symTbl (InitialConstraintSet initConstrs) constrs =
    let ctxs     = symTbl^.rootFeature.to allContexts
        eConstrs = fmap trnsConstraintExpr
                       (toList constrs ++ toList initConstrs)
        eCards   = fmap featureExpr ctxs
        eInits   = fmap varInitExpr (initialState symTbl)

        e = foldr lAnd (BoolExpr True noLoc) (eCards ++ eConstrs ++ eInits)
    in InitDef (Init e noLoc)

varInitExpr :: (QualifiedVar, LExpr) -> LExpr
varInitExpr (QualifiedVar sc ident idx, e) =
    NameExpr (fullyQualifiedName sc ident idx noLoc) noLoc `eq` e

featureExpr :: FeatureContext -> LExpr
featureExpr ctx =
    let fs             = ctx^.this
        (lower, upper) = fs^.fsGroupCard
        childs         = fs^..fsChildren.traverse.traverse
        n              = genericLength childs

        childCtxs             = fmap (`extendContext` ctx) childs
        (optCtxs, nonOptCtxs) = partition (^.this.fsOptional) childCtxs

        nOpt = genericLength optCtxs

        l = intExpr lower - intExpr nOpt `lte` sum (fmap activeExpr nonOptCtxs)
        u = sum (fmap activeExpr childCtxs) `gte` intExpr upper

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

