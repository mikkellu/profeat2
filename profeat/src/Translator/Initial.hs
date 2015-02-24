{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Translator.Initial
  ( genInit
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.List

import Symbols
import Syntax

import Analysis.InitialState
import Error
import Types

import Translator.Common
import Translator.Constraints
import Translator.Names

genInit :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
        => InitialConstraintSet
        -> m LDefinition
genInit (InitialConstraintSet initConstrs) = do
    symTbl  <- view symbolTable
    constrs <- view constraints

    eInit <- case symTbl^.initConfExpr of
                 Just e  -> trnsExpr isBoolType e
                 Nothing -> return $ BoolExpr True noLoc

    let ctxs     = symTbl^.rootFeature.to allContexts
        eConstrs = fmap trnsConstraintExpr
                       (toList constrs ++ toList initConstrs)
        eCards   = fmap featureExpr ctxs
        eInits   = fmap varInitExpr (initialState symTbl)

        e = foldl' lAnd (BoolExpr True noLoc) . concat $
                [eCards, eConstrs, eInits, [eInit]]

    return . InitDef $ Init e noLoc

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

