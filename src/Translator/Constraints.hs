{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Translator.Constraints
  ( ConstraintExpr(..)
  , ConstraintSet
  , InitialConstraintSet(..)

  , extractConstraints
  , canEvalConstraint
  , refersTo

  , trnsConstraintExpr
  , fromExpr
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
                      , MonadEither Error m
                      , HasSymbolTable r
                      , HasScope r
                      )
                   => FeatureSymbol
                   -> m (InitialConstraintSet, ConstraintSet)
extractConstraints root =
    fmap (_1 %~ InitialConstraintSet) . flip execStateT (Set.empty, Set.empty) $
        void . for (allContexts root) $ \ctx -> local (scope .~ Local ctx) $
            for (ctx^.this.fsConstraints) $ \constr -> do
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

trnsConstraintExpr :: ConstraintExpr -> LExpr
trnsConstraintExpr c = case c of
    BinaryConstr binOp lhs rhs -> binaryExpr (LogicBinOp binOp)
       (trnsConstraintExpr lhs) (trnsConstraintExpr rhs)
    UnaryConstr unOp c' -> unaryExpr (LogicUnOp unOp) (trnsConstraintExpr c')
    FeatConstr ctx      -> identExpr (activeIdent ctx) noLoc `eq` 1
    BoolConstr b        -> BoolExpr b noLoc

fromExpr :: ( Applicative m
            , MonadReader r m
            , MonadEither Error m
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

