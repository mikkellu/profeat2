{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Translator
  ( translateModel
  , translateModelInstances
  , translateSpec
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Array
import Data.Foldable ( toList )
import Data.List ( genericLength, sortBy )
import Data.Map ( Map, union, unions )
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord ( comparing )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Traversable

import Analysis.InitialState
import Error
import Eval
import Symbols
import Syntax
import Syntax.Util
import Template
import Typechecker
import Types

import Translator.Common
import Translator.Controller
import Translator.Initial
import Translator.Invariant
import Translator.Modules
import Translator.Names
import Translator.Properties
import Translator.Rewards

translateModel :: SymbolTable -> Either Error LModel
translateModel symTbl = do
    (initExprs, invs) <- getInitsAndInvariants symTbl
    params            <- getFamilyParameters symTbl
    translateModel' symTbl initExprs invs params

translateModel' :: SymbolTable
                -> InitExprs
                -> Invariants
                -> Set (Scope, Ident)
                -> Either Error LModel
translateModel' symTbl initExprs invs params =
    flip runReaderT (trnsInfo symTbl invs params) $ do
        (controllerDef, lss) <- trnsControllerDef initExprs
        local (labelSets .~ lss) $ do
            modelT      <- view modelType
            constDefs   <- trnsConsts
            globalDefs  <- trnsGlobals
            moduleDefs  <- trnsModules
            labelDefs   <- fmap LabelDef <$>
                               trnsLabels (symTbl^..labels.traverse)
            rewardsDefs <- trnsRewards

            return . Model modelT . sortBy (comparing defAnnot) $ concat
                [ constDefs
                , globalDefs
                , moduleDefs
                , toList controllerDef
                , labelDefs
                , rewardsDefs
                ]

translateModelInstances :: SymbolTable -> Either Error [LModel]
translateModelInstances symTbl = do
    (InitExprs initExprs, Invariants invs) <- getInitsAndInvariants symTbl
    constrs                                <- getFamilyConstraints symTbl
    params                                 <- getFamilyParameters symTbl

    let attribs = attributes (symTbl^.rootFeature)
        params' = if Set.null params
                      then Set.fromList $ fmap (\(ctx, ident, _) -> (Local ctx, ident)) attribs
                      else params

    let defs = activeDefs (symTbl^.rootFeature)
        es   = concatMap (^.from conjunction) (initExprs ++ invs ++ constrs)
        es'  = fmap (substitute defs) es -- substitute active formulas

    vals <- runReaderT (initialValuations params' es') symTbl
    for vals $ \val ->
        let initExprs' = InitExprs initExprs <> valInitExprs val
        in translateModel' symTbl initExprs' (Invariants invs) params

initialValuations :: ( Applicative m
                     , MonadReader r m
                     , MonadError Error m
                     , HasSymbolTable r
                     )
                  => Set (Scope, Ident)
                  -> [LExpr]
                  -> m [Valuation]
initialValuations params es = do
    symTbl   <- view symbolTable
    constVal <- view constValues
    initVal  <- attribInitialValues
    let vals = union <$> initialConfigurations symTbl
                     <*> attribValuations (symTbl^.rootFeature) params
        initVal' = constVal `union` initVal

    flip filterM vals $ \val -> do
        let val' = val `union` initVal'
        fmap and . for es $ \e -> if canEval val' e
            then val' `satisfies` e
            else return True

initialConfigurations :: SymbolTable -> [Valuation]
initialConfigurations symTbl =
    let ctxs = symTbl^.rootFeature.to allContexts
    in unions <$> for ctxs ctxVals
  where
    ctxVals ctx =
        let fs    = ctx^.this
            confs = configurations (^.fsOptional)
                                   (fs^.fsGroupCard)
                                   (fs^..fsChildren.traverse)
            atomicCtxs = fmap (`extendContext` ctx) .
                         filter (not . _fsMandatory) $
                         fs^..fsChildren.traverse.traverse
        in fmap (confVal ctx atomicCtxs) confs

    confVal ctx atomicCtxs fss =
        let confCtxs = fmap (`extendContext` ctx) .
                       filter (not . _fsMandatory) $ fss
        in Map.fromList . flip fmap atomicCtxs $ \ctx' ->
            let v = IntVal $ if ctx' `elem` confCtxs then 1 else 0
            in ((activeIdent ctx', 0), v)

attribValuations :: FeatureSymbol -> Set (Scope, Ident) -> [Valuation]
attribValuations root params =
    let attribs = filter (\(ctx, ident, _) -> (Local ctx, ident) `Set.member` params) .
                  filter (has $ _3.vsInit._Nothing) $ attributes root
    in fmap unions . for attribs $ \(ctx, ident, vs) -> case vs^.vsType of
        CompoundType (ArrayType (Just (lower, upper)) st) ->
            let values = enumValues st
            in fmap unions . for [lower..upper] $ \i ->
                let fqi = fullyQualifiedIdent (Local ctx) ident (Just i)
                in fmap (Map.singleton (fqi, 0)) values
        SimpleType st ->
            let values = enumValues st
                fqi    = fullyQualifiedIdent (Local ctx) ident Nothing
            in fmap (Map.singleton (fqi, 0)) values
        _ -> []

attribInitialValues :: ( Applicative m
                       , MonadReader r m
                       , MonadError Error m
                       , HasSymbolTable r
                       )
                    => m Valuation
attribInitialValues = do
    val  <- view constValues
    root <- view $ symbolTable.rootFeature
    fmap unions . for (attributes root) $ \(ctx, ident, vs) ->
        case vs^.vsInit of
            Just e -> do
                v <- eval' val e
                return $ case vs^.vsType of
                    CompoundType (ArrayType (Just (lower, upper)) _) ->
                        Map.fromList . flip fmap [lower..upper] $ \i ->
                            ((fullyQualifiedIdent (Local ctx) ident (Just i), 0), v)
                    _ -> Map.singleton
                        (fullyQualifiedIdent (Local ctx) ident Nothing, 0) v
            Nothing -> return Map.empty

attributes :: FeatureSymbol -> [(FeatureContext, Ident, VarSymbol)]
attributes = filter (^._3.vsIsAttrib) . localVars

localVars :: FeatureSymbol -> [(FeatureContext, Ident, VarSymbol)]
localVars = concatMap f . allContexts where
    f ctx = flip fmap (ctx^.this.fsVars.to Map.assocs) $ \(ident, vs) ->
        (ctx, ident, vs)

canEval :: Valuation -> Expr a -> Bool
canEval val = all contained . universe where
    contained = \case
        (viewIdentExpr -> Just ident) -> (ident, 0) `Map.member` val
        (NameExpr _ _)                -> False
        _                             -> True

satisfies :: (MonadError Error m) => Valuation -> LExpr -> m Bool
satisfies val e = do
    BoolVal b <- eval' val e
    return b

valInitExprs :: Valuation -> InitExprs
valInitExprs =
    InitExprs .
    fmap (\((ident, _), v) -> identExpr ident noLoc `eq` valueExpr v) .
    Map.assocs

translateSpec :: SymbolTable
              -> LSpecification
              -> Either Error LSpecification
translateSpec symTbl (Specification defs) =
    flip runReaderT (trnsInfo' symTbl) $ do
        -- constDefs <- trnsConsts
        let constDefs = []
        labelDefs <- trnsLabelDefs defs
        propDefs  <- for (defs^..traverse._PropertyDef) $ \prop ->
                         PropertyDef <$> trnsProperty prop

        return . Specification $ concat [constDefs, labelDefs, propDefs]

trnsConsts :: Trans [LDefinition]
trnsConsts =
    fmap concat . traverse (uncurry trnsConst) =<< view (constants.to Map.assocs)

trnsConst :: Ident -> ConstSymbol -> Trans [LDefinition]
trnsConst ident (ConstSymbol l t ct e) = case e of
    ArrayExpr es _ -> fmap concat . for (zip (toList es) [0..]) $ \(e', i) ->
        trnsConst (indexedIdent ident i) (ConstSymbol l t ct e')
    _ -> do
        e' <- trnsExpr (const True) e
        return [ConstDef $ Constant ct ident e' l]

trnsGlobals :: Trans [LDefinition]
trnsGlobals = do
    globalTbl <- view globals
    fmap concat . for (globalTbl^..traverse) $ \(GlobalSymbol t decl) ->
        fmap GlobalDef <$> trnsVarDecl t decl

trnsLabelDefs :: Translator [LDefinition]
trnsLabelDefs defs = fmap LabelDef <$> trnsLabels (defs^..traverse._LabelDef)

trnsLabels :: Translator [LLabel]
trnsLabels = fmap catMaybes . traverse trnsLabel

trnsLabel :: LLabel -> Trans (Maybe LLabel)
trnsLabel (Label ident e l)
  | ident == initConfLabelIdent = return Nothing
  | otherwise = do
      e' <- trnsExpr isBoolType =<< prepExpr e
      return . Just $ Label ident e' l

getInitsAndInvariants :: SymbolTable -> Either Error (InitExprs, Invariants)
getInitsAndInvariants symTbl = flip runReaderT (trnsInfo' symTbl) $
    (,) <$> getInits <*> getInvariants

getInvariants :: Trans Invariants
getInvariants = do
    symTbl <- view symbolTable

    invExprs <- case symTbl^.invariantExpr of
        Just e  -> view (from conjunction) <$> trnsExpr isBoolType e
        Nothing -> return []
    constrs <- getConstraints False

    let invs  = invExprs ++ constrs
        invs' = substitute (activeDefs $ symTbl^.rootFeature) <$> invs -- substitute active formulas

    return (Invariants invs')

activeDefs :: FeatureSymbol -> Map Ident LExpr
activeDefs = Map.fromList . fmap activeDef . allContexts where
    activeDef = (,) <$> activeFormulaIdent <*> activeExpr

getFamilyParameters :: (Applicative m, MonadError Error m)
                    => SymbolTable
                    -> m (Set (Scope, Ident))
getFamilyParameters symTbl = case symTbl^.familySpec of
    Nothing  -> return Set.empty
    Just fam -> flip runReaderT (Env Global symTbl) $
        fmap Set.fromList . for (fam^.fmsParameters) $ \name -> do
            SymbolInfo{..} <- getSymbolInfo name
            return (siScope, siIdent)

getFamilyConstraints :: (Applicative m, MonadError Error m)
                     => SymbolTable
                     -> m [LExpr]
getFamilyConstraints symTbl = case symTbl^.familySpec of
    Nothing  -> return []
    Just fam -> flip runReaderT (trnsInfo' symTbl) $
        for (fam^.fmsConstraints) $ trnsExpr isBoolType

getInits :: Trans InitExprs
getInits = do
    symTbl <- view symbolTable

    let varInit = fmap varInitExpr (initialState symTbl)
    eInit <- case symTbl^.initConfExpr of
        Just e  -> trnsExpr isBoolType e
        Nothing -> return $ BoolExpr True noLoc
    initConstrExprs <- getConstraints True

    return . InitExprs . concat $ [initConstrExprs, varInit, [eInit]]

varInitExpr :: (QualifiedVar, LExpr) -> LExpr
varInitExpr (QualifiedVar sc ident idx, e) =
    NameExpr (fullyQualifiedName sc ident idx noLoc) noLoc `eq` e

getConstraints :: Bool -> Trans [LExpr]
getConstraints initial =
    fmap (catMaybes . concat) . forAllContexts $ \ctx ->
        for (ctx^.this.fsConstraints) $ \(Constraint i e _) ->
            if i == initial
                then Just <$> trnsExpr isBoolType e
                else return Nothing

configurations :: (a -> Bool)
               -> (Integer, Integer)
               -> [Array Integer a]
               -> [[a]]
configurations isOptional (lower, upper) as = filter valid . subsequences $ as
  where
    opt = genericLength . filter isOptional $ as^..traverse.traverse
    valid xs = let cnt  = genericLength xs
                   mand = genericLength . filter (not . isOptional) $ xs
               in lower - opt <= mand && cnt <= upper

subsequences :: [Array Integer a] -> [[a]]
subsequences = powerset . concatMap elems

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

