{-# LANGUAGE FlexibleContexts, TupleSections #-}

module ProductLine
  ( rootFeatureSymbol
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Either
import Control.Monad.Reader
import Control.Monad.State

import Data.List ( (\\), group, genericLength, sort )
import Data.Map ( fromList )
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable

import Error
import Eval
import Symbols
import Syntax
import Template
import Typechecker
import Types
import Types.Util

rootFeatureSymbol :: (Applicative m, MonadEither Error m)
                  => SymbolTable
                  -> m FeatureSymbol
rootFeatureSymbol symTbl =
    let roots = rootFeatures symTbl
    in flip runReaderT symTbl $ do
        syms <- for roots $ \ident -> (ident,) <$> toFeatureSymbol' True ident
        return $ case syms of
            []         -> emptyFeatureSymbol
            [(_, sym)] -> sym
            _          ->
                let card = genericLength syms
                in emptyFeatureSymbol & fsGroupCard .~ (card, card)
                                      & fsChildren  .~ fromList syms

-- | Returns all root features, i.e. non-parametrized features that are not
-- referenced in any feature decomposition.
rootFeatures :: SymbolTable -> [Ident]
rootFeatures symTbl =
    let feats = map featIdent .
                filter (null . featParams) $
                symTbl^..features.traverse -- exclude templates as they must be explicitly instantiated

        refFeats = symTbl^..features.traverse
                           .to featDecomp._Just
                           .to decompChildren.traverse
                           .to frInstance
                           .to instIdent
    in feats \\ refFeats

toFeatureSymbol' :: ( Applicative m
                    , MonadReader SymbolTable m
                    , MonadEither Error m
                    )
                 => Bool -- ^ True, if the feature is mandatory
                 -> Ident
                 -> m FeatureSymbol
toFeatureSymbol' mandatory ident =
    let inst = Instance ident [] noLoc
        ref  = FeatureRef False inst Nothing Nothing
    in toFeatureSymbol mandatory ref

-- | Builds a 'FeatureSymbol' and all its child features.
toFeatureSymbol :: ( Applicative m
                   , MonadReader SymbolTable m
                   , MonadEither Error m
                   )
                => Bool -- ^ True, if the feature is mandatory
                -> LFeatureRef
                -> m FeatureSymbol
toFeatureSymbol mandatory (FeatureRef isOptional inst cntExpr _) = do
    let Instance ident args l = inst
    feat <- instantiate ident args l =<< lookupFeature ident l

    (groupCard, childFeats) <- case featDecomp feat of
        Nothing -> return ((0, 0), Map.empty)
        Just decomp@(Decomposition decompOp refs _) -> do
            checkDecomposition decomp

            let mandatory' = case decompOp of
                                 AllOf -> mandatory
                                 _     -> False

            cfs <- fmap fromList . for refs $ \ref ->
                (featRefIdent ref,) <$> toFeatureSymbol mandatory' ref

            let numChilds = sum $ cfs^..traverse.fsCount
            card <- groupCardinality numChilds decompOp

            return (card, cfs)

    cnt <- case cntExpr of
        Just e -> do
            e' <- preprocessExpr e
            checkIfConst e' >> checkIfType_ isIntType e'

            val      <- view constValues
            IntVal v <- eval' val e'

            return v
        Nothing -> return 1

    (mods, varSyms) <- instantiateModules (featModules feat)

    return FeatureSymbol
        { _fsGroupCard = groupCard
        , _fsChildren  = childFeats
        , _fsCount     = cnt
        , _fsMandatory = mandatory && not isOptional
        , _fsOptional  = isOptional
        , _fsModules   = mods
        , _fsVars      = varSyms
        }

instantiateModules :: ( Applicative m
                      , MonadReader SymbolTable m
                      , MonadEither Error m
                      )
                   => [LInstance]
                   -> m (Table LModuleBody, Table VarSymbol)
instantiateModules insts = flip runStateT Map.empty $
    fmap fromList . for insts $ \inst@(Instance ident _ l) -> do
        (body, varSyms) <- instantiateModule inst
        get >>= unionVarTable l varSyms >>= put

        return (ident, body)

unionVarTable :: (MonadEither Error m)
              => SrcLoc
              -> Table VarSymbol
              -> Table VarSymbol
              -> m (Table VarSymbol)
unionVarTable l t1 t2 =
    let isect = Map.keys $ Map.intersection t1 t2
    in case isect of
        (ident:_) -> throw l $ AmbiguousIdentifier ident
        _         -> return $ Map.union t1 t2

instantiateModule :: ( Applicative m
                     , MonadReader SymbolTable m
                     , MonadEither Error m
                     )
                  => LInstance
                  -> m (LModuleBody, Table VarSymbol)
instantiateModule (Instance ident args l) = do
    mod' <- instantiate ident args l =<< lookupModule ident l
    let body'  = modBody mod'
    let public = modProvides mod'

    varSyms <- fmap Map.fromList . for (modVars body') $ \decl ->
                   (declIdent decl,) <$> varSymbol public decl

    return (body', varSyms)

varSymbol :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
          => [Ident]
          -> LVarDecl
          -> m VarSymbol
varSymbol public (VarDecl ident vt _ l) =
    VarSymbol l (ident `elem` public) <$> fromVarType vt

-- | Check if the referenced features are unambiguous.
checkDecomposition :: (MonadEither Error m) => LDecomposition -> m ()
checkDecomposition (Decomposition _ refs l) =
    let ambigiousRefs = filter ((> 1) . length) .
                        group . sort .
                        fmap featRefIdent $ refs
    in case ambigiousRefs of
        ((ident:_):_) -> throw l $ AmbiguousDecomposition ident
        _             -> return ()

featRefIdent :: FeatureRef a -> Ident
featRefIdent fr = fromMaybe (instIdent $ frInstance fr) (frAlias fr)

-- | Returns the group cardinality for the given decomposition operator.
groupCardinality :: ( Applicative m
                    , MonadReader SymbolTable m
                    , MonadEither Error m
                    )
                 => Integer   -- ^ total number of child features
                 -> LDecompOp -- ^ decomposition operator
                 -> m (Integer, Integer)
groupCardinality cnt decompOp = case decompOp of
    AllOf       -> return (cnt, cnt) -- and
    OneOf       -> return (  1,   1) -- xor
    SomeOf      -> return (  1, cnt) -- or
    Group range -> both preprocessExpr range >>= evalRange

