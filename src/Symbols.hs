{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Symbols
  ( GlobalSymbol(..)
  , gsLoc
  , gsType
  , gsVarType
  , gsExpr

  , ConstSymbol(..)
  , csLoc
  , csType
  , csConstType
  , csExpr

  , Table
  , SymbolTable(..)
  , globals
  , constants
  , formulas
  , modules
  , features
  , constValues

  , emptySymbolTable
  , containsSymbol
  , lookupType
  ) where

import Control.Applicative hiding ( empty )
import Control.Monad.Either
import Control.Lens

import Data.Map ( Map, empty )

import Error
import Syntax
import Types

data GlobalSymbol = GlobalSymbol
  { _gsLoc     :: !SrcLoc
  , _gsType    :: !Type
  , _gsVarType :: LVarType
  , _gsExpr    :: Maybe LExpr
  } deriving (Show)

makeLenses ''GlobalSymbol

data ConstSymbol = ConstSymbol
  { _csLoc       :: !SrcLoc
  , _csType      :: !Type
  , _csConstType :: !ConstType
  , _csExpr      :: LExpr
  } deriving (Show)

makeLenses ''ConstSymbol

type Table a = Map Ident a

data SymbolTable = SymbolTable
  { _globals     :: Table GlobalSymbol
  , _constants   :: Table ConstSymbol
  , _formulas    :: Table LFormula
  , _modules     :: Table LModule
  , _features    :: Table LFeature
  , _constValues :: Valuation
  } deriving (Show)

makeLenses ''SymbolTable

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable empty empty empty empty empty empty

containsSymbol :: SymbolTable -> Ident -> Maybe SrcLoc
containsSymbol symTbl ident =
    (symTbl^?globals  .at ident._Just.gsLoc) <|>
    (symTbl^?constants.at ident._Just.csLoc) <|>
    (symTbl^?formulas .at ident._Just.to frmAnnot)

lookupType :: (MonadEither Error m) => Name a -> SrcLoc -> SymbolTable -> m Type
lookupType name l symTbl = case name of
    Name ident -> maybe (throw l $ UndefinedIdentifier ident) return $
        (symTbl^?globals  .at ident._Just.gsType) <|>
        (symTbl^?constants.at ident._Just.csType)
    _ -> undefined -- TODO: implement lookup for qualified names

