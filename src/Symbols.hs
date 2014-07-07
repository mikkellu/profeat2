{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, TemplateHaskell #-}

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

  , VarSymbol(..)
  , vsLoc
  , vsType
  , vsPublic

  , FeatureSymbol(..)
  , fsIdent
  , fsIndex
  , fsIsMultiFeature
  , fsGroupCard
  , fsChildren
  , fsMandatory
  , fsOptional
  , fsModules
  , fsVars

  , Table
  , SymbolTable(..)
  , globals
  , constants
  , formulas
  , modules
  , features
  , constValues
  , rootFeature

  , emptySymbolTable
  , emptyFeatureSymbol

  , containsSymbol
  , containsModule
  , containsFeature
  , lookupModule
  , lookupFeature
  , lookupType
  , featureCardinality
  ) where

import Control.Applicative hiding ( empty )
import Control.Monad.Either
import Control.Monad.Reader
import Control.Lens

import Data.Array
import Data.Map ( Map, empty )

import Error
import Syntax
import Types

type Table a = Map Ident a

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

data VarSymbol = VarSymbol
  { _vsLoc     :: !SrcLoc
  , _vsPublic  :: !Bool
  , _vsType    :: !Type
  } deriving (Show)

makeLenses ''VarSymbol

data FeatureSymbol = FeatureSymbol
  { _fsIdent          :: !Ident
  , _fsIndex          :: !Integer
  , _fsIsMultiFeature :: !Bool
  , _fsGroupCard      :: (Integer, Integer)
  , _fsChildren       :: Table (Array Integer FeatureSymbol)
  , _fsMandatory      :: !Bool -- ^ the feature is contained in every product
  , _fsOptional       :: !Bool -- ^ the feature is marked as optional
  , _fsModules        :: Table LModuleBody
  , _fsVars           :: Table VarSymbol
  } deriving (Show)

makeLenses ''FeatureSymbol

data SymbolTable = SymbolTable
  { _globals     :: Table GlobalSymbol
  , _constants   :: Table ConstSymbol
  , _formulas    :: Table LFormula
  , _modules     :: Table LModule
  , _features    :: Table LFeature
  , _constValues :: Valuation
  , _rootFeature :: FeatureSymbol
  } deriving (Show)

makeLenses ''SymbolTable

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable
  { _globals     = empty
  , _constants   = empty
  , _formulas    = empty
  , _modules     = empty
  , _features    = empty
  , _constValues = empty
  , _rootFeature = emptyFeatureSymbol
  }

emptyFeatureSymbol :: FeatureSymbol
emptyFeatureSymbol = FeatureSymbol
  { _fsIdent          = ""
  , _fsIndex          = 0
  , _fsIsMultiFeature = False
  , _fsGroupCard      = (0, 0)
  , _fsChildren       = empty
  , _fsMandatory      = True
  , _fsOptional       = False
  , _fsModules        = empty
  , _fsVars           = empty
  }

containsSymbol :: SymbolTable -> Ident -> Maybe SrcLoc
containsSymbol symTbl ident =
    (symTbl^?globals  .at ident._Just.gsLoc) <|>
    (symTbl^?constants.at ident._Just.csLoc) <|>
    (symTbl^?formulas .at ident._Just.to frmAnnot)

containsModule :: SymbolTable -> Ident -> Maybe SrcLoc
containsModule symTbl ident =
    symTbl^?modules.at ident._Just.to modBody.to modAnnot

containsFeature :: SymbolTable -> Ident -> Maybe SrcLoc
containsFeature symTbl ident = symTbl^?features.at ident._Just.to featAnnot

lookupModule :: (MonadReader SymbolTable m, MonadEither Error m)
             => Ident
             -> SrcLoc
             -> m LModule
lookupModule = checkedLookup modules

lookupFeature :: (MonadReader SymbolTable m, MonadEither Error m)
              => Ident
              -> SrcLoc
              -> m LFeature
lookupFeature = checkedLookup features

checkedLookup :: (MonadReader SymbolTable m, MonadEither Error m)
              => Lens' SymbolTable (Table a)
              -> Ident
              -> SrcLoc
              -> m a
checkedLookup f ident l =
    maybe (throw l $ UndefinedIdentifier ident) return .
    view (f.at ident) =<< ask

lookupType :: (MonadReader SymbolTable m, MonadEither Error m)
           => Name a
           -> SrcLoc
           -> m Type
lookupType name l = ask >>= \symTbl -> case name of
    Name ident -> maybe (throw l $ UndefinedIdentifier ident) return $
        (symTbl^?globals  .at ident._Just.gsType) <|>
        (symTbl^?constants.at ident._Just.csType)
    _ -> undefined -- TODO: implement lookup for qualified names

featureCardinality :: Array Integer FeatureSymbol -> Integer
featureCardinality a = let (lower, upper) = bounds a
                       in upper - lower + 1

