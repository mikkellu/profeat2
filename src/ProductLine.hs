{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections #-}

module ProductLine
  ( rootFeatureSymbol
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.List ( (\\), group, genericLength, sort )
import Data.Map ( fromList )
import qualified Data.Map as Map
import Data.Maybe
import Data.Array
import Data.Traversable

import Error
import Symbols
import Syntax
import Template
import Typechecker
import Types.Util

rootFeatureSymbol :: (Applicative m, MonadError Error m)
                  => SymbolTable
                  -> m FeatureSymbol
rootFeatureSymbol symTbl =
    let roots = rootFeatures symTbl
    in flip runReaderT (Env Global symTbl) $ do
        syms <- for roots $ \ident -> (ident,) <$> toFeatureSymbol' True ident
        return $ case syms of
            []         -> emptyFeatureSymbol & fsIdent .~ "root"
            [(_, sym)] -> sym ! 0
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
                    , MonadReader r m
                    , MonadError Error m
                    , HasSymbolTable r
                    , HasScope r
                    )
                 => Bool -- ^ True, if the feature is mandatory
                 -> Ident
                 -> m (Array Integer FeatureSymbol)
toFeatureSymbol' mandatory ident =
    let inst = Instance ident [] noLoc
        ref  = FeatureRef False inst Nothing Nothing
    in toFeatureSymbols mandatory ref

toFeatureSymbols :: ( Applicative m
                    , MonadReader r m
                    , MonadError Error m
                    , HasSymbolTable r
                    , HasScope r
                    )
                 => Bool
                 -> LFeatureRef
                 -> m (Array Integer FeatureSymbol)
toFeatureSymbols mandatory ref@(FeatureRef isOptional inst _ cntExpr) = do
    let Instance ident args l = inst
    cnt <- evalFeatureCardinality cntExpr

    fss <- for [0..cnt-1] $ \idx -> do
        feat <- lookupFeature ident l >>=
                instantiateWithId idx ident args l

        (groupCard, childFeats) <- case featDecomp feat of
            Nothing -> return ((0, 0), Map.empty)
            Just decomp@(Decomposition decompOp refs _) -> do
                checkDecomposition decomp

                let mandatory' = case decompOp of
                                     AllOf -> True
                                     _     -> False

                cfs <- fmap fromList . for refs $ \ref' ->
                    (featRefIdent ref',) <$> toFeatureSymbols mandatory' ref'

                let numChilds = sum $ cfs^..traverse.to featureCardinality
                card <- groupCardinality numChilds decompOp

                return (card, cfs)


        (mods, varSyms) <- instantiateModules idx (featModules feat)

        return FeatureSymbol
            { _fsIdent          = featRefIdent ref
            , _fsIndex          = idx
            , _fsIsMultiFeature = cnt > 1
            , _fsGroupCard      = groupCard
            , _fsChildren       = childFeats
            , _fsMandatory      = mandatory && not isOptional
            , _fsOptional       = isOptional
            , _fsModules        = mods
            , _fsVars           = varSyms
            , _fsConstraints    = featConstraints feat
            , _fsRewards        = featRewards feat
            }

    return $ listArray (0, cnt - 1) fss

instantiateModules :: ( Applicative m
                      , MonadReader r m
                      , MonadError Error m
                      , HasSymbolTable r
                      , HasScope r
                      )
                   => Integer
                   -> [LInstance]
                   -> m (Table LModuleBody, Table VarSymbol)
instantiateModules idx insts = flip runStateT Map.empty $
    fmap fromList . for insts $ \inst@(Instance ident _ l) -> do
        (body, varSyms) <- instantiateModule idx inst
        get >>= unionVarTable l varSyms >>= put

        return (ident, body)

instantiateModule :: ( Applicative m
                     , MonadReader r m
                     , MonadError Error m
                     , HasSymbolTable r
                     , HasScope r
                     )
                  => Integer
                  -> LInstance
                  -> m (LModuleBody, Table VarSymbol)
instantiateModule idx (Instance ident args l) = do
    mod'  <- instantiateWithId idx ident args l =<< lookupModule ident l
    body' <- prepModuleBody $ modBody mod'
    let public = modPublic mod'

    varSyms <- fmap Map.fromList . for (modVars body') $ \decl ->
                   (declIdent decl,) <$> varSymbol public decl

    return (body', varSyms)

unionVarTable :: (MonadError Error m)
              => SrcLoc
              -> Table VarSymbol
              -> Table VarSymbol
              -> m (Table VarSymbol)
unionVarTable l t1 t2 =
    let isect = Map.keys $ Map.intersection t1 t2
    in case isect of
        (ident:_) -> throw l $ AmbiguousIdentifier ident
        _         -> return $ Map.union t1 t2

varSymbol :: ( Applicative m
             , MonadReader r m
             , MonadError Error m
             , HasSymbolTable r
             , HasScope r
             )
          => [Ident]
          -> LVarDecl
          -> m VarSymbol
varSymbol public (VarDecl ident vt _ l) =
    VarSymbol l (ident `elem` public) <$> fromVarType vt

-- | Check if the referenced features are unambiguous.
checkDecomposition :: (MonadError Error m) => LDecomposition -> m ()
checkDecomposition (Decomposition _ refs l) =
    let ambigiousRefs = filter ((> 1) . length) .
                        group . sort .
                        fmap featRefIdent $ refs
    in case ambigiousRefs of
        ((ident:_):_) -> throw l $ AmbiguousDecomposition ident
        _             -> return ()

evalFeatureCardinality :: ( Applicative m
                          , MonadReader r m
                          , MonadError Error m
                          , HasSymbolTable r
                          , HasScope r
                          )
                       => Maybe LExpr
                       -> m Integer
evalFeatureCardinality cntExpr = case cntExpr of
    Nothing -> return 1
    Just e -> do
        e' <- prepExpr e
        v  <- evalInteger e'
        unless (v > 0) . throw (exprAnnot e) $ InvalidFeatureCardinality v

        return v

-- | Returns the group cardinality for the given decomposition operator.
groupCardinality :: ( Applicative m
                    , MonadReader r m
                    , MonadError Error m
                    , HasSymbolTable r
                    , HasScope r
                    )
                 => Integer   -- ^ total number of child features
                 -> LDecompOp -- ^ decomposition operator
                 -> m (Integer, Integer)
groupCardinality cnt decompOp = case decompOp of
    AllOf   -> return (cnt, cnt) -- and
    OneOf   -> return (  1,   1) -- xor
    SomeOf  -> return (  1, cnt) -- or
    Group r -> both prepExpr r >>= evalRange -- TODO bounds checking

featRefIdent :: FeatureRef a -> Ident
featRefIdent fr = fromMaybe (instIdent $ frInstance fr) (frAlias fr)

