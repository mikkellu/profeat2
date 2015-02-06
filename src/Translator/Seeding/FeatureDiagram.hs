{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Generation of seeding code based on feature diagram.
module Translator.Seeding.FeatureDiagram
  ( genSeeding
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable             ( toList )
import Data.Set                  ( Set )
import qualified Data.Set as Set
import Data.Traversable

import Symbols
import Syntax
import Syntax.Util

import Translator.Common
import Translator.Constraints
import Translator.Names
import Translator.Seeding.Common

data SeedInfo = SeedInfo
  { _seedLoc         :: !Integer           -- ^ the next seeding step
  , _seedVisited     :: Set FeatureContext -- ^ visited roots of atomic sets
  , _seedConstraints :: Set ConstraintExpr -- ^ remaining constraints
  }

makeLenses ''SeedInfo

genSeeding :: (MonadReader TrnsInfo m)
           => InitialConstraintSet
           -> m ([LStmt], Integer)
genSeeding (InitialConstraintSet initConstrs) = do
    root    <- view rootFeature
    constrs <- view constraints

    let seedInfo = SeedInfo 0 Set.empty (constrs `Set.union` initConstrs)

    return . over _2 _seedLoc . flip runState seedInfo . fmap concat $
        for (allContexts root) seed

seed :: FeatureContext -> State SeedInfo [LStmt]
seed ctx = do
    let fs         = ctx^.this
        childFeats = fs^..fsChildren.traverse.traverse
        atomicSets = filter (not . _fsMandatory) childFeats

    if null atomicSets
        then return []
        else seedAtomicSets ctx atomicSets

seedAtomicSets :: FeatureContext -> [FeatureSymbol] -> State SeedInfo [LStmt]
seedAtomicSets ctx atomicSets = do
    let fs    = ctx^.this
        confs = configurations (^.fsOptional)
                               (fs^.fsGroupCard)
                               (fs^..fsChildren.traverse)
        atomicCtxs = fmap (`extendContext` ctx) atomicSets

    seedVisited %= Set.union (Set.fromList atomicCtxs)

    constrs <- applicableConstraints

    stmts <- for confs (genStmt constrs atomicCtxs)
    seedLoc += 1
    return stmts
  where
    genStmt constrs atomicCtxs conf = do
        i <- use seedLoc
        let confCtxs  = fmap (`extendContext` ctx) conf
            constrs'  = fmap (specialize atomicCtxs confCtxs) constrs
            constrGrd = conjunction $ fmap trnsConstraintExpr constrs'
            locGrd    = identExpr seedVarIdent noLoc `eq` intExpr i
            grd       = locGrd `lAnd` constrGrd
            upd       = genUpdate i confCtxs
        return $ Stmt NoAction grd (Repeatable [One upd]) noLoc

    genUpdate i ctxs =
        let ctxs' = filter (not . _fsMandatory . thisFeature) ctxs
            sAsgn = One (incSeedVar i)
            asgns = fmap (\c -> One $ Assign (activeName c) 1 noLoc) ctxs'
        in Update Nothing (Repeatable $ sAsgn:asgns) noLoc

    specialize atomicCtxs chosenCtxs = transform $ \case
        FeatConstr ctx' -> case atomicSetRoot ctx' of
            Nothing -> BoolConstr True
            Just ctx''
              | ctx'' `elem` atomicCtxs -> BoolConstr (ctx'' `elem` chosenCtxs)
              | otherwise               -> FeatConstr ctx''
        c -> c

    applicableConstraints = do
        visited <- use seedVisited
        (appConstrs, constrs') <-
            Set.partition (canEvalConstraint visited) <$> use seedConstraints

        seedConstraints .= constrs'
        return $ toList appConstrs

