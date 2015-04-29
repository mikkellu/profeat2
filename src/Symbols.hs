{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Symbols
  ( GlobalSymbol(..)
  , gsType
  , gsDecl

  , ConstSymbol(..)
  , csLoc
  , csType
  , csConstType
  , csExpr

  , VarSymbol(..)
  , vsLoc
  , vsType
  , vsIsAttrib
  , vsInit

  , ParamSymbol(..)
  , psType
  , psDecl

  , FamilySymbol(..)
  , famsParameters
  , famsConstraints

  , FeatureSymbol(..)
  , fsIdent
  , fsIndex
  , fsIsMultiFeature
  , fsAttributes
  , fsGroupCard
  , fsChildren
  , fsMandatory
  , fsOptional
  , fsBlocking
  , fsModules
  , fsVars
  , fsConstraints
  , fsRewards

  , ControllerSymbol(..)
  , ctsVars
  , ctsBody

  , Table
  , SymbolTable(..)
  , HasSymbolTable(..)

  , emptySymbolTable
  , emptyFeatureSymbol

  , containsSymbol
  , containsModule
  , containsFeature
  , containsController
  , containsInvariant
  , lookupLabel
  , lookupModule
  , lookupFeature
  , featureCardinality

  , FeatureContext
  , getFeatureSymbols

  , atomicSetRoot

  , hasSingleConfiguration
  , isLeafFeature
  , thisFeature
  , this
  , parentContext
  , parentContexts
  , childContexts
  , rootContext
  , extendContext
  , allContexts
  , forAllContexts_
  , forAllContexts

  , Scope(..)
  , HasScope(..)

  , Env(..)

  , paramSymsToGlobalSyms

  , contextIdent
  , indexedIdent
  ) where

import Control.Applicative hiding ( empty )
import Control.Monad.Reader
import Control.Lens

import Data.Array
import Data.List ( tails )
import Data.List.NonEmpty ( NonEmpty(..), toList )
import qualified Data.List.NonEmpty as L
import Data.Map ( Map, empty )
import Data.Maybe
import Data.Text.Lazy ( append, pack )
import qualified Data.Text.Lazy as T

import Text.PrettyPrint.Leijen.Text hiding ( (<$>), empty )
import qualified Text.PrettyPrint.Leijen.Text as PP

import Error
import Syntax
import Types

type Table a = Map Ident a

data GlobalSymbol = GlobalSymbol
  { _gsType     :: !Type
  , _gsDecl     :: LVarDecl
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
  { _vsLoc      :: !SrcLoc
  , _vsIsAttrib :: !Bool
  , _vsType     :: !Type
  , _vsInit     :: Maybe LExpr
  } deriving (Show)

makeLenses ''VarSymbol

data ControllerSymbol = ControllerSymbol
  { _ctsVars :: Table VarSymbol
  , _ctsBody :: LModuleBody
  } deriving (Show)

makeLenses ''ControllerSymbol

data ParamSymbol = ParamSymbol
  { _psType :: !Type
  , _psDecl :: LVarDecl
  } deriving (Show)

makeLenses ''ParamSymbol

data FamilySymbol = FamilySymbol
  { _famsParameters  :: Table ParamSymbol
  , _famsConstraints :: [LExpr]
  } deriving (Show)

makeLenses ''FamilySymbol

data FeatureSymbol = FeatureSymbol
  { _fsIdent          :: !Ident
  , _fsIndex          :: !Integer
  , _fsIsMultiFeature :: !Bool
  , _fsAttributes     :: [LVarDecl]
  , _fsGroupCard      :: (Integer, Integer)
  , _fsChildren       :: Table (Array Integer FeatureSymbol)
  , _fsMandatory      :: !Bool                    -- ^ the feature is mandatory (relative to its parent)
  , _fsOptional       :: !Bool                    -- ^ the feature is marked as optional
  , _fsBlocking       :: [(Ident, Maybe Integer)] -- ^ actions to block when inactive
  , _fsModules        :: Table LModuleBody
  , _fsVars           :: Table VarSymbol
  , _fsConstraints    :: [LConstraint]
  , _fsRewards        :: [LRewards]
  } deriving (Show)

makeLenses ''FeatureSymbol

instance Eq FeatureSymbol where
    x == y = _fsIdent x == _fsIdent y && _fsIndex x == _fsIndex y

instance Ord FeatureSymbol where
    compare x y = compare (x^.fsIdent) (y^.fsIdent) `mappend`
                  compare (x^.fsIndex) (y^.fsIndex)

data SymbolTable = SymbolTable
  { _modelType     :: !ModelType
  , _globals       :: Table GlobalSymbol
  , _constants     :: Table ConstSymbol
  , _formulas      :: Table LFormula
  , _labels        :: Table LLabel
  , _modules       :: Table LModule
  , _features      :: Table LFeature
  , _constValues   :: Valuation
  , _rootFeature   :: FeatureSymbol
  , _controller    :: Maybe ControllerSymbol
  , _initConfExpr  :: Maybe LExpr
  , _invariantExpr :: Maybe LExpr
  , _initConfLabel :: Maybe LExpr
  } deriving (Show)

makeClassy ''SymbolTable

newtype FeatureContext = FeatureContext
  { getFeatureSymbols :: NonEmpty FeatureSymbol
  } deriving (Eq, Ord)

instance Pretty FeatureContext where
    pretty (FeatureContext fss) =
        hcat . punctuate dot . fmap qualifier . reverse . toList $ fss
      where
        qualifier fs = text (fs^.fsIdent) <> prettyIndex fs
        prettyIndex fs
          | fs^.fsIsMultiFeature = brackets . integer $ fs^.fsIndex
          | otherwise            = PP.empty

class HasScope a where
    scope :: Lens' a Scope

data Scope = Global | LocalCtrlr | Local FeatureContext deriving (Eq, Ord)

instance HasScope Scope where
    scope = id

data Env = Env
  { _envScope       :: Scope
  , _envSymbolTable :: SymbolTable
  }

makeLenses ''Env

instance HasSymbolTable Env where
    symbolTable = envSymbolTable

instance HasScope Env where
    scope = envScope

emptySymbolTable :: ModelType -> SymbolTable
emptySymbolTable t = SymbolTable
  { _modelType     = t
  , _globals       = empty
  , _constants     = empty
  , _formulas      = empty
  , _labels        = empty
  , _modules       = empty
  , _features      = empty
  , _constValues   = empty
  , _rootFeature   = emptyFeatureSymbol
  , _controller    = Nothing
  , _initConfExpr  = Nothing
  , _invariantExpr = Nothing
  , _initConfLabel = Nothing
  }

emptyFeatureSymbol :: FeatureSymbol
emptyFeatureSymbol = FeatureSymbol
  { _fsIdent          = ""
  , _fsIndex          = 0
  , _fsAttributes     = []
  , _fsIsMultiFeature = False
  , _fsGroupCard      = (0, 0)
  , _fsChildren       = empty
  , _fsMandatory      = True
  , _fsOptional       = False
  , _fsBlocking       = []
  , _fsModules        = empty
  , _fsVars           = empty
  , _fsConstraints    = []
  , _fsRewards        = []
  }

containsSymbol :: SymbolTable -> Ident -> Maybe SrcLoc
containsSymbol symTbl ident =
    (symTbl^?globals  .at ident._Just.gsDecl.to declAnnot) <|>
    (symTbl^?constants.at ident._Just.csLoc) <|>
    (symTbl^?formulas .at ident._Just.to frmAnnot) <|>
    (symTbl^?labels   .at ident._Just.to lblAnnot)

containsModule :: SymbolTable -> Ident -> Maybe SrcLoc
containsModule symTbl ident =
    symTbl^?modules.at ident._Just.to modBody.to modAnnot

containsFeature :: SymbolTable -> Ident -> Maybe SrcLoc
containsFeature symTbl ident = symTbl^?features.at ident._Just.to featAnnot

containsController :: SymbolTable -> Ident -> Maybe SrcLoc
containsController symTbl _ = symTbl^?controller._Just.ctsBody.to modAnnot

containsInvariant :: SymbolTable -> Ident -> Maybe SrcLoc
containsInvariant symTbl _ = symTbl^?invariantExpr._Just.to exprAnnot

lookupLabel :: (MonadReader r m, MonadError Error m, HasSymbolTable r)
            => Ident
            -> SrcLoc
            -> m LLabel
lookupLabel = checkedLookup labels

lookupModule :: (MonadReader r m, MonadError Error m, HasSymbolTable r)
             => Ident
             -> SrcLoc
             -> m LModule
lookupModule = checkedLookup modules

lookupFeature :: (MonadReader r m, MonadError Error m, HasSymbolTable r)
              => Ident
              -> SrcLoc
              -> m LFeature
lookupFeature = checkedLookup features

checkedLookup :: (MonadReader r m, MonadError Error m, HasSymbolTable r)
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

-- | Returns 'True' if there is only a single possible configuration for
-- the given 'FeatureSymbol'.
hasSingleConfiguration :: FeatureSymbol -> Bool
hasSingleConfiguration = all (^.this.fsMandatory) . allContexts

atomicSetRoot :: FeatureContext -> Maybe FeatureContext
atomicSetRoot ctx
  | not $ ctx^.this.fsMandatory = Just ctx
  | otherwise                   = atomicSetRoot =<< parentContext ctx

isLeafFeature :: FeatureSymbol -> Bool
isLeafFeature = ((0, 0) ==) . _fsGroupCard

rootContext :: FeatureSymbol -> FeatureContext
rootContext fs = FeatureContext (fs :| [])

extendContext :: FeatureSymbol -> FeatureContext -> FeatureContext
extendContext fs = FeatureContext . L.cons fs . getFeatureSymbols

thisFeature :: FeatureContext -> FeatureSymbol
thisFeature = L.head . getFeatureSymbols

this :: Getter FeatureContext FeatureSymbol
this = to thisFeature

parentContext :: FeatureContext -> Maybe FeatureContext
parentContext = fmap FeatureContext . L.nonEmpty . L.tail . getFeatureSymbols

parentContexts :: FeatureContext -> [FeatureContext]
parentContexts = fmap FeatureContext .
                 catMaybes .
                 fmap L.nonEmpty .
                 tails .
                 toList .
                 getFeatureSymbols

childContexts :: FeatureContext -> [FeatureContext]
childContexts ctx =
    let childFeats = ctx^..this.fsChildren.traverse.traverse
    in fmap (`extendContext` ctx) childFeats

allContexts :: FeatureSymbol -> [FeatureContext]
allContexts root = go (\_ _ -> rootCtx) rootCtx root
  where
    go mkContext ctx fs =
        let self  = mkContext fs ctx
            ctxs' = concatMap (go extendContext self) $
                              fs^..fsChildren.traverse.traverse
        in self:ctxs'
    rootCtx = rootContext root

forAllContexts_ :: (MonadReader r m, HasSymbolTable r, HasScope r)
                => (FeatureContext -> m a)
                -> m ()
forAllContexts_ = void . forAllContexts

forAllContexts :: (MonadReader r m, HasSymbolTable r, HasScope r)
               => (FeatureContext -> m a)
               -> m [a]
forAllContexts m =
    traverse inLocalCtx . allContexts =<< view rootFeature
  where
    inLocalCtx ctx = local (scope .~ Local ctx) (m ctx)

paramSymsToGlobalSyms :: Table ParamSymbol -> Table GlobalSymbol
paramSymsToGlobalSyms = traverse %~ (GlobalSymbol <$> _psType <*> _psDecl)

contextIdent :: FeatureContext -> Ident
contextIdent = T.concat . fmap (cons '_' . mkFsIdent) .
               L.tail . L.reverse . getFeatureSymbols
  where
    mkFsIdent fs
      | fs^.fsIsMultiFeature = indexedIdent (fs^.fsIdent) (fs^.fsIndex)
      | otherwise            = fs^.fsIdent

indexedIdent :: Ident -> Integer -> Ident
indexedIdent ident i = ident `append` ('_' `cons` indexToText i)
  where
    indexToText j
      | j >= 0 = pack (show j)
      | otherwise = '_' `cons` (pack . show $ negate j)

