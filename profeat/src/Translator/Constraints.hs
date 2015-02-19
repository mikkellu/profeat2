{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Translator.Constraints
  ( ConstraintExpr(..)
  , ConstraintSet
  , InitialConstraintSet(..)

  , extractConstraints
  , canEvalConstraint
  , refersTo
  , projectToAtomicSets

  , trnsConstraintExpr
  , fromExpr

  , activeGuard
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Maybe
import Data.Set ( Set, fromList, isSubsetOf )
import qualified Data.Set as Set
import Data.Traversable

import Error
import Symbols
import Syntax
import Typechecker

import Translator.Names

data ConstraintExpr
  = BinaryConstr !LogicBinOp ConstraintExpr ConstraintExpr
  | UnaryConstr !LogicUnOp ConstraintExpr
  | FeatConstr FeatureContext
  | BoolConstr !Bool
  deriving (Eq, Ord)

makePrisms ''ConstraintExpr

instance Plated ConstraintExpr where
    plate f c = case c of
        BinaryConstr binOp lhs rhs -> BinaryConstr binOp <$> f lhs
                                                         <*> f rhs
        UnaryConstr unOp c'        -> UnaryConstr unOp <$> f c'
        _                          -> pure c

type    ConstraintSet        = Set ConstraintExpr
newtype InitialConstraintSet = InitialConstraintSet ConstraintSet

extractConstraints :: ( Applicative m
                      , MonadReader r m
                      , MonadError Error m
                      , HasSymbolTable r
                      , HasScope r
                      )
                   => m (InitialConstraintSet, ConstraintSet)
extractConstraints =
    fmap (_1 %~ InitialConstraintSet) . flip execStateT (Set.empty, Set.empty) $
        forAllContexts_ $ \ctx -> for (ctx^.this.fsConstraints) $ \constr -> do
            c <- fromExpr (constrExpr constr)

            let l = if constrInitial constr then _1 else _2
            l %= Set.insert c

canEvalConstraint :: Set FeatureContext -> ConstraintExpr -> Bool
canEvalConstraint ctxs c =
    let constrCtxs = universe c^..traverse._FeatConstr
        asRoots    = catMaybes $ fmap atomicSetRoot constrCtxs -- project all features to their atomic set
    in fromList asRoots `isSubsetOf` ctxs

refersTo :: ConstraintExpr -> FeatureContext -> Bool
refersTo c ctx = ctx `elem` universe c^..traverse._FeatConstr

projectToAtomicSets :: ConstraintExpr -> ConstraintExpr
projectToAtomicSets = transform $ \case
    FeatConstr ctx -> case atomicSetRoot ctx of
        Just atomicCtx -> FeatConstr atomicCtx
        Nothing        -> BoolConstr True
    c -> c

trnsConstraintExpr :: ConstraintExpr -> LExpr
trnsConstraintExpr c = case c of
    BinaryConstr binOp lhs rhs -> binaryExpr (LogicBinOp binOp)
       (trnsConstraintExpr lhs) (trnsConstraintExpr rhs)
    UnaryConstr unOp c' -> unaryExpr (LogicUnOp unOp) (trnsConstraintExpr c')
    FeatConstr ctx      -> activeGuard ctx
    BoolConstr b        -> BoolExpr b noLoc

fromExpr :: ( Applicative m
            , MonadReader r m
            , MonadError Error m
            , HasSymbolTable r
            , HasScope r
            )
         => LExpr
         -> m ConstraintExpr
fromExpr e = case e of
    BinaryExpr (LogicBinOp binOp) lhs rhs _ ->
        BinaryConstr binOp <$> fromExpr lhs <*> fromExpr rhs
    UnaryExpr (LogicUnOp unOp) e' _ ->
        UnaryConstr unOp <$> fromExpr e'
    NameExpr name _ -> FeatConstr <$> getFeature name
    BoolExpr b _    -> pure $ BoolConstr b
    _ -> throw (exprAnnot e) InvalidConstraint

activeGuard :: FeatureContext -> LExpr
activeGuard = flip NameExpr noLoc . activeFormulaName

