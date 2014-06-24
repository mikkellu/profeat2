{-# LANGUAGE TemplateHaskell #-}

module Symbols
  ( GlobalSymbol(..)
  , gsLoc
  , gsType

  , ConstSymbol(..)
  , csLoc
  , csType
  , csValue

  , Table
  , SymbolTable(..)
  , globals
  , constants
  , formulas
  , modules
  , features

  , emptySymbolTable
  ) where

import Control.Lens

import Data.Map ( Map, empty )

import SrcLoc
import Syntax
import Types

data GlobalSymbol = GlobalSymbol
  { _gsLoc  :: !SrcLoc
  , _gsType :: !Type
  }

makeLenses ''GlobalSymbol

data ConstSymbol = ConstSymbol
  { _csLoc   :: !SrcLoc
  , _csType  :: !Type
  , _csValue :: Maybe Value
  }

makeLenses ''ConstSymbol

type Table a = Map Ident a

data SymbolTable = SymbolTable
  { _globals   :: Table GlobalSymbol
  , _constants :: Table ConstSymbol
  , _formulas  :: Table LFormula
  , _modules   :: Table LModule
  , _features  :: Table LFeature
  }

makeLenses ''SymbolTable

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable empty empty empty empty empty

