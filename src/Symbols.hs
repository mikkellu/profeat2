{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
           , RankNTypes
           , TemplateHaskell
           , TupleSections #-}

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
  , HasSymbolTable(..)

  , emptySymbolTable
  , emptyFeatureSymbol

  , containsSymbol
  , containsModule
  , containsFeature
  , lookupModule
  , lookupFeature
  , featureCardinality

  , FeatureContext

  , thisFeature
  , extendContext
  , allContexts

  , Scope(..)
  , scope

  , Env(..)
  ) where

import Control.Applicative hiding ( empty )
import Control.Monad.Reader
import Control.Lens

import Data.Array
import Data.List.NonEmpty ( NonEmpty(..), toList )
import qualified Data.List.NonEmpty as L
import Data.Map ( Map, empty )

import Text.PrettyPrint.Leijen.Text hiding ( (<$>), empty )
import qualified Text.PrettyPrint.Leijen.Text as PP

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

makeClassy ''SymbolTable

newtype FeatureContext = FeatureContext (NonEmpty FeatureSymbol)

instance Pretty FeatureContext where
    pretty (FeatureContext fss) =
        hcat . punctuate dot . fmap qualifier . reverse . toList $ fss
      where
        qualifier fs = text (fs^.fsIdent) <> prettyIndex fs
        prettyIndex fs
          | fs^.fsIndex == 0 &&
            not (fs^.fsIsMultiFeature) = PP.empty
          | otherwise                  = brackets . integer $ fs^.fsIndex

data Scope = Global | Local FeatureContext

data Env = Env
  { _scope          :: Scope
  , _envSymbolTable :: SymbolTable
  }

makeLenses ''Env

instance HasSymbolTable Env where
    symbolTable = envSymbolTable

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

lookupModule :: (MonadReader r m, MonadEither Error m, HasSymbolTable r)
             => Ident
             -> SrcLoc
             -> m LModule
lookupModule = checkedLookup modules

lookupFeature :: (MonadReader r m, MonadEither Error m, HasSymbolTable r)
              => Ident
              -> SrcLoc
              -> m LFeature
lookupFeature = checkedLookup features

checkedLookup :: (MonadReader r m, MonadEither Error m, HasSymbolTable r)
              => Lens' SymbolTable (Table a)
              -> Ident
              -> SrcLoc
              -> m a
checkedLookup f ident l =
    maybe (throw l $ UndefinedIdentifier ident) return .
    view (f.at ident) =<< view symbolTable

featureCardinality :: Array Integer FeatureSymbol -> Integer
featureCardinality a = let (lower, upper) = bounds a
                       in upper - lower + 1

extendContext :: FeatureSymbol -> FeatureContext -> FeatureContext
extendContext fs (FeatureContext fss) = FeatureContext (L.cons fs fss)

thisFeature :: FeatureContext -> FeatureSymbol
thisFeature (FeatureContext fss) = L.head fss

allContexts :: FeatureSymbol -> [FeatureContext]
allContexts root = go (\_ _ -> rootContext) rootContext root
  where
    go mkContext ctx fs =
        let self  = mkContext fs ctx
            ctxs' = concatMap (go extendContext self) $
                              fs^..fsChildren.traverse.traverse
        in self:ctxs'
    rootContext = FeatureContext (root :| [])

