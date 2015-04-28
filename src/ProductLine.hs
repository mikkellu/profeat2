{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module ProductLine
  ( rootFeatureSymbol
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Array
import Data.List ( (\\), group, genericLength, sort )
import Data.Map ( fromList )
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable

import Error
import Symbols
import Syntax
import Template
import Typechecker
import Types.Util

rootFeatureSymbol :: (MonadError Error m) => SymbolTable -> m FeatureSymbol
rootFeatureSymbol symTbl = flip runReaderT (Env Global symTbl) $
    case rootFeatures symTbl of
        [root]    -> (! 0) <$> toFeatureSymbol' True (featIdent root)
        []        -> mkRootFeatureSymbol
        (f1:f2:_) -> throw (featAnnot f2) $ MultipleRootFeatures (featAnnot f1)
  where
    mkRootFeatureSymbol = do
        let roots = unreferencedFeatures symTbl
            mods  = fmap modInstance (unreferencedModules symTbl)

        (mods', varSyms) <- instantiateModules 0 mods
        syms <- for roots $ \ident -> (ident,) <$> toFeatureSymbol' True ident

        let card = genericLength syms
            root = emptyFeatureSymbol & fsGroupCard .~ (card, card)
                                      & fsChildren  .~ fromList syms
                                      & fsModules   .~ mods'
                                      & fsVars      .~ varSyms
        return root

    modInstance m = Instance (modIdent m) [] noLoc

-- | Returns all features that are marked as root features.
rootFeatures :: SymbolTable -> [LFeature]
rootFeatures = filter featIsRoot . toListOf (features.traverse)

unreferencedModules :: SymbolTable -> [LModule]
unreferencedModules symTbl =
    let refMods = symTbl^..features.traverse
                          .to featModules
                          .traverse
                          .to instIdent
    in filter ((`notElem` refMods) . modIdent) .
       filter (null . modParams) $ symTbl^..modules.traverse

-- | Returns all non-parametrized features that are not referenced in any
-- feature decomposition.
unreferencedFeatures :: SymbolTable -> [Ident]
unreferencedFeatures symTbl =
    let feats = map featIdent .
                filter (null . featParams) $
                symTbl^..features.traverse -- exclude templates as they must be explicitly instantiated

        refFeats = symTbl^..features.traverse
                           .to featDecomp._Just
                           .to decompChildren.traverse
                           .to frInstance
                           .to instIdent
    in feats \\ refFeats

toFeatureSymbol' :: ( MonadReader r m
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

toFeatureSymbols :: ( MonadReader r m
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
        Feature{..} <- instantiateWithId idx ident args l =<<
                       lookupFeature ident l

        attribDecls' <- traverse prepExprs featAttributes

        attribVarSyms <- fmap fromList . for attribDecls' $ \decl ->
            (declIdent decl,) <$> toVarSymbol False True decl

        (groupCard, childFeats) <- case featDecomp of
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

        (mods, varSyms) <- instantiateModules idx featModules

        constraints' <- traverse prepExprs featConstraints
        rewards'     <- traverse prepExprs featRewards
        blocking' <- for featBlocking $ \n -> do
            LabelInfo{..} <- getLabelInfo =<< prepExprs n
            return (liIdent, liIndex)

        return FeatureSymbol
            { _fsIdent          = featRefIdent ref
            , _fsIndex          = idx
            , _fsIsMultiFeature = cnt > 1
            , _fsAttributes     = attribDecls'
            , _fsGroupCard      = groupCard
            , _fsChildren       = childFeats
            , _fsMandatory      = mandatory && not isOptional
            , _fsOptional       = isOptional
            , _fsBlocking       = blocking'
            , _fsModules        = mods
            , _fsVars           = Map.union attribVarSyms varSyms
            , _fsConstraints    = constraints'
            , _fsRewards        = rewards'
            }

    return $ listArray (0, cnt - 1) fss

instantiateModules :: ( MonadReader r m
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

instantiateModule :: ( MonadReader r m
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
                   let ident' = declIdent decl
                   in (ident',) <$> toVarSymbol (ident' `elem` public) False decl

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

-- | Check if the referenced features are unambiguous.
checkDecomposition :: (MonadError Error m) => LDecomposition -> m ()
checkDecomposition (Decomposition _ refs l) =
    let ambigiousRefs = filter ((> 1) . length) .
                        group . sort .
                        fmap featRefIdent $ refs
    in case ambigiousRefs of
        ((ident:_):_) -> throw l $ AmbiguousDecomposition ident
        _             -> return ()

evalFeatureCardinality :: ( MonadReader r m
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
groupCardinality :: ( MonadReader r m
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

