{-# LANGUAGE FlexibleContexts #-}

module Translator.Seeding
  ( genOperatingFormula
  , genInitConfLabel
  , genInitConfStmt
  , genSeedVar
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Maybe

import Error
import Symbols
import Syntax
import Syntax.Util
import Types

import Translator.Common
import Translator.Names
import Translator.Seeding.Common

genOperatingFormula :: Maybe Integer -> LDefinition
genOperatingFormula i = FormulaDef Formula
  { frmIdent  = operatingIdent
  , frmParams = []
  , frmExpr   = maybe (BoolExpr True noLoc) seedVarGuard i
  , frmAnnot  = noLoc
  }

genInitConfLabel :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
                 => Integer
                 -> m LDefinition
genInitConfLabel i = do
    lbl <- _Just (trnsExpr isBoolType) =<< view initConfLabel
    return $ LabelDef Label
      { lblIdent = initConfLabelIdent
      , lblExpr  = fromMaybe (seedVarGuard $ i - 1) lbl
      , lblAnnot = noLoc
      }

genInitConfStmt :: (MonadReader r m, HasSymbolTable r)
                => Integer
                -> m (Maybe LStmt, Integer)
genInitConfStmt i = do
    root <- view rootFeature
    lbl  <- view initConfLabel
    return $ if hasSingleConfiguration root || isJust lbl
        then (Nothing, i)
        else let asgn = incSeedVar i
                 upd  = Update Nothing (Repeatable [One asgn]) noLoc
                 stmt = Stmt NoAction (seedVarGuard i) (Repeatable [One upd]) noLoc
             in (Just stmt, i + 1)

genSeedVar :: Integer -> LVarDecl
genSeedVar i = VarDecl
  { declIdent = seedVarIdent
  , declType  = SimpleVarType $ IntVarType (0, intExpr i)
  , declInit  = Just 0
  , declAnnot = noLoc
  }

seedVarGuard :: Integer -> LExpr
seedVarGuard = eq (identExpr seedVarIdent noLoc) . intExpr

