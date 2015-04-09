{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import Data.List.NonEmpty ( fromList )
import Data.Map ( Map, union, unions )
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord ( comparing )
import Data.Traversable

import Analysis.InitialState
import Error
import Eval
import SymbolTable
import Syntax
import Syntax.Util
import Template
import Typechecker
import Types
import Types.Util

import Translator.Common
import Translator.Controller
import Translator.Initial
import Translator.Invariant
import Translator.Modules
import Translator.Names
import Translator.Properties
import Translator.Rewards

translateModel :: LModel -> Either Error (LModel, SymbolTable)
translateModel (Model modelT defs) = do
    symTbl <- extendSymbolTable (emptySymbolTable modelT) defs

    mFams <- getFamily symTbl defs
    let (defs', symTbl') = case mFams of
            Just (FamilySymbol params constrs) ->
                let gs  = paramSymsToGlobalSyms params
                    i   = genInitDef constrs
                    val = paramValuation params
                in (i ++ defs, symTbl & constValues %~ union val
                                      & globals .~ gs)
            Nothing -> (defs, symTbl)

    symTbl'' <- updateSymbolTable symTbl' defs'

    (initExprs, invs) <- extractInitsAndInvariants symTbl''
    model' <- translateModel' symTbl'' initExprs invs
    return (model', symTbl'')
  where
    genInitDef [] = []
    genInitDef cs = (:[]) . InitDef . flip Init noLoc $ foldr1 lAnd cs

    paramValuation = unions . fmap f . Map.assocs where
        f (ident, ParamSymbol t _) = case t of
            CompoundType (ArrayType (Just (lower, upper)) st) ->
                unions . fmap mkVal $ zip (repeat st) [lower..upper]
            CompoundType (ArrayType _ _) ->
                error "Translator.translateModel: unevaluated bounds"
            SimpleType st -> mkVal (st, 0)
          where
            mkVal (st, idx) = case st of
                IntType (Just (_, upper)) ->
                    Map.singleton (ident, idx) (IntVal upper)
                _ -> Map.empty

translateModelInstances :: LModel -> Either Error ([LModel], SymbolTable)
translateModelInstances (Model modelT defs) = do
    symTbl <- extendSymbolTable (emptySymbolTable modelT) defs

    getFamily symTbl defs >>= \case
        Just fams -> do
            vals <- paramValuations (symTbl^.constValues) fams

            results <- for vals $ \val -> do
                let constTbl = valuationToConstSymbols (fams^.famsParameters) val
                    symTbl' = symTbl & constants   %~ union constTbl
                                     & constValues %~ union val
                symTbl'' <- updateSymbolTable symTbl' defs

                (initExprs, invs) <- extractInitsAndInvariants symTbl''
                model' <- translateModel' symTbl'' initExprs invs
                return (model', symTbl'')

            let (models', symTbls') = unzip results
            return (models', if null results then symTbl else last symTbls')
        Nothing -> do
            symTbl' <- updateSymbolTable symTbl defs
            (InitExprs initExprs, invs) <- extractInitsAndInvariants symTbl'
            (constrs, initConstrs) <-
                flip runReaderT (trnsInfo symTbl' (Invariants [])) $
                    (,) <$> extractConstraints False <*> extractConstraints True

            let e = (constrs ++ initConstrs)^.conjunction
                e' = substitute (activeDefs $ symTbl'^.rootFeature) e -- substitute active formulas

            vals <- runReaderT (initialValuations e') symTbl'
            fmap (, symTbl') . for vals $ \val ->
                let initExprs' = InitExprs initExprs <> valInitExprs val
                in translateModel' symTbl' initExprs' invs

translateModel' :: SymbolTable -> InitExprs -> Invariants -> Either Error LModel
translateModel' symTbl initExprs invs =
    flip runReaderT (trnsInfo symTbl invs) $ do
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

translateSpec :: SymbolTable
              -> LSpecification
              -> Either Error LSpecification
translateSpec symTbl (Specification defs) = do
    symTbl'  <- extendSymbolTable symTbl defs
    symTbl'' <- updateSymbolTable symTbl' defs

    flip runReaderT (trnsInfo symTbl'' (Invariants [])) $ do
        -- constDefs <- trnsConsts
        let constDefs = []
        labelDefs <- trnsLabelDefs defs
        propDefs  <- for (defs^..traverse._PropertyDef) $ \prop ->
                         PropertyDef <$> trnsProperty prop

        return . Specification $ concat [constDefs, labelDefs, propDefs]

paramValuations :: Valuation -> FamilySymbol -> Either Error [Valuation]
paramValuations constVal (FamilySymbol params constrs) = do
    let vals = allParamValuations params -- TODO: evaluate initial values
    flip filterM vals $ \val -> fmap and . for constrs $ \c -> do
        let val' = val `union` constVal
        checkIfConst' val' c
        BoolVal b <- eval' val' c
        return b

valuationToConstSymbols :: Table ParamSymbol -> Valuation -> Table ConstSymbol
valuationToConstSymbols paramTbl val = Map.mapWithKey mkConstSymbol paramTbl
  where
    mkConstSymbol ident ps = case ps^.psType of
        CompoundType (ArrayType (Just (lower, upper)) st) ->
            let ct = toConstType st
                es = fmap (\i -> valueExpr (val Map.! (ident, i))) [lower..upper]
                e  = ArrayExpr (fromList es) noLoc
            in ConstSymbol noLoc (ps^.psType) ct e
        CompoundType (ArrayType _ _) -> error "Translator.valuationToConsts: unevaluated bounds"
        SimpleType st ->
            let ct = toConstType st
                e  = valueExpr (val Map.! (ident, 0))
            in ConstSymbol noLoc (ps^.psType) ct e

allParamValuations :: Table ParamSymbol -> [Valuation]
allParamValuations paramTbl =
    fmap unions . for (Map.assocs paramTbl) $ \(ident, ps) -> case ps^.psType of
        CompoundType (ArrayType (Just (lower, upper)) st) ->
            let values = enumValues st
            in fmap unions . for [lower..upper] $ \i ->
                fmap (Map.singleton (ident, i)) values
        SimpleType st -> fmap (Map.singleton (ident, 0)) (enumValues st)
        _ -> []

initialValuations :: ( Applicative m
                     , MonadReader r m
                     , MonadError Error m
                     , HasSymbolTable r
                     )
                  => LExpr
                  -> m [Valuation]
initialValuations e = do
    symTbl <- view symbolTable
    val <- attribInitialValues
    let vals = union <$> initialConfigurations symTbl
                     <*> attribValuations symTbl
        vals' = fmap (union val) vals
    filterM (`satisfies` e) vals'

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

attribValuations :: SymbolTable -> [Valuation]
attribValuations symTbl =
    let ctxs = symTbl^.rootFeature.to allContexts
    in fmap unions . for ctxs $ \ctx -> do
        let attribs = filter (^._2.vsIsAttrib) $ ctx^.this.fsVars.to Map.assocs
            nondet  = filter (has $ _2.vsInit._Nothing) attribs
        fmap unions . for nondet $ \(ident, vs) ->
            case vs^.vsType of
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
    ctxs <- view $ symbolTable.rootFeature.to allContexts
    fmap unions . for ctxs $ \ctx ->
        fmap unions . for (ctx^.this.fsVars.to Map.assocs) $ \(ident, vs) ->
            case vs^.vsInit of
                Just e | vs^.vsIsAttrib -> do
                    v <- eval' val e
                    return $ case vs^.vsType of
                        CompoundType (ArrayType (Just (lower, upper)) _) ->
                            Map.fromList . flip fmap [lower..upper] $ \i ->
                                ((fullyQualifiedIdent (Local ctx) ident (Just i), 0), v)
                        _ -> Map.singleton
                            (fullyQualifiedIdent (Local ctx) ident Nothing, 0) v
                _ -> return Map.empty

satisfies :: (MonadReader r m, MonadError Error m, HasSymbolTable r)
          => Valuation
          -> LExpr
          -> m Bool
satisfies val e = do
    constVal  <- view constValues
    BoolVal b <- eval' (val `union` constVal) e
    return b

valInitExprs :: Valuation -> InitExprs
valInitExprs =
    InitExprs .
    fmap (\((ident, _), v) -> identExpr ident noLoc `eq` valueExpr v) .
    Map.assocs

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

extractInitsAndInvariants :: SymbolTable -> Either Error (InitExprs, Invariants)
extractInitsAndInvariants symTbl =
    flip runReaderT (trnsInfo symTbl (Invariants [])) $
        (,) <$> extractInits <*> extractInvariants

extractInvariants :: Trans Invariants
extractInvariants = do
    symTbl <- view symbolTable

    invExprs <- case symTbl^.invariantExpr of
        Just e  -> view (from conjunction) <$> trnsExpr isBoolType e
        Nothing -> return []
    constrs <- extractConstraints False

    let invs  = invExprs ++ constrs
        invs' = substitute (activeDefs $ symTbl^.rootFeature) <$> invs -- substitute active formulas

    return (Invariants invs')

activeDefs :: FeatureSymbol -> Map Ident LExpr
activeDefs = Map.fromList . fmap activeDef . allContexts where
    activeDef = (,) <$> activeFormulaIdent <*> activeExpr

extractInits :: Trans InitExprs
extractInits = do
    symTbl <- view symbolTable

    let varInit = fmap varInitExpr (initialState symTbl)
    eInit <- case symTbl^.initConfExpr of
        Just e  -> trnsExpr isBoolType e
        Nothing -> return $ BoolExpr True noLoc
    initConstrExprs <- extractConstraints True

    return . InitExprs . concat $ [initConstrExprs, varInit, [eInit]]

varInitExpr :: (QualifiedVar, LExpr) -> LExpr
varInitExpr (QualifiedVar sc ident idx, e) =
    NameExpr (fullyQualifiedName sc ident idx noLoc) noLoc `eq` e

extractConstraints :: Bool -> Trans [LExpr]
extractConstraints initial =
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

