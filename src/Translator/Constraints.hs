{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Translator.Constraints
  ( Constraint(..)

  , extractConstraints
  , canEvalConstraint
  , refersTo

  , trnsConstraint
  , fromExpr
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.Set ( Set, fromList, isSubsetOf )
import Data.Traversable

import Error
import Symbols
import Syntax
import Typechecker

import Translator.Names

data Constraint
  = BinaryConstr !LogicBinOp Constraint Constraint
  | UnaryConstr !LogicUnOp Constraint
  | FeatConstr FeatureContext
  | BoolConstr !Bool
  deriving (Eq, Ord)

makePrisms ''Constraint

instance Plated Constraint where
    plate f c = case c of
        BinaryConstr binOp lhs rhs -> BinaryConstr binOp <$> f lhs
                                                         <*> f rhs
        UnaryConstr unOp c'        -> UnaryConstr unOp <$> f c'
        _                          -> pure c

extractConstraints :: ( Applicative m
                      , MonadReader r m
                      , MonadEither Error m
                      , HasSymbolTable r
                      , HasScope r
                      )
                   => FeatureSymbol
                   -> m (Set Constraint)
extractConstraints root = execWriterT $
    void . for (allContexts root) $ \ctx -> local (scope .~ Local ctx) $
        tell . fromList =<< traverse fromExpr (ctx^.this.fsConstraints)

canEvalConstraint :: Set FeatureContext -> Constraint -> Bool
canEvalConstraint ctxs c =
    fromList (universe c^..traverse._FeatConstr) `isSubsetOf` ctxs

refersTo :: Constraint -> FeatureContext -> Bool
refersTo c ctx = ctx `elem` universe c^..traverse._FeatConstr

trnsConstraint :: Constraint -> LExpr
trnsConstraint c = case c of
    BinaryConstr binOp lhs rhs ->
        binaryExpr (LogicBinOp binOp) (trnsConstraint lhs) (trnsConstraint rhs)
    UnaryConstr unOp c' -> unaryExpr (LogicUnOp unOp) (trnsConstraint c')
    FeatConstr ctx      -> identExpr (activeIdent ctx) noLoc `eq` 1
    BoolConstr b        -> BoolExpr b noLoc

fromExpr :: ( Applicative m
            , MonadReader r m
            , MonadEither Error m
            , HasSymbolTable r
            , HasScope r
            )
         => LExpr
         -> m Constraint
fromExpr e = case e of
    BinaryExpr (LogicBinOp binOp) lhs rhs _ ->
        BinaryConstr binOp <$> fromExpr lhs <*> fromExpr rhs
    UnaryExpr (LogicUnOp unOp) e' _ ->
        UnaryConstr unOp <$> fromExpr e'
    NameExpr name _ -> FeatConstr <$> getFeature name
    BoolExpr b _    -> pure $ BoolConstr b
    _ -> throw (exprAnnot e) InvalidConstraint

