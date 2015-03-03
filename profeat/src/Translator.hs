{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Translator
  ( translateModel
  , translateSpec
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.List ( sortBy )
import Data.Map ( Map, assocs, fromList )
import Data.Maybe
import Data.Ord ( comparing )
import Data.Traversable

import Analysis.InitialState
import Error
import Symbols
import Syntax
import Syntax.Util
import Template
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
    (initExprs, invs) <- flip runReaderT (trnsInfo symTbl (Invariants [])) $
        (,) <$> extractInits <*> extractInvariants

    flip runReaderT (trnsInfo symTbl invs) $ do
        (controllerDef, lss) <- trnsControllerDef initExprs
        local (labelSets .~ lss) $ do
            constDefs   <- trnsConsts
            globalDefs  <- trnsGlobals
            moduleDefs  <- trnsModules
            labelDefs   <- fmap LabelDef <$>
                               trnsLabels (symTbl^..labels.traverse)
            rewardsDefs <- trnsRewards

            return . Model . sortBy (comparing defAnnot) $ concat
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
translateSpec symTbl (Specification defs) =
    flip runReaderT (trnsInfo symTbl (Invariants [])) $ do
        -- constDefs <- trnsConsts
        let constDefs = []
        labelDefs <- trnsLabelDefs defs
        propDefs  <- for (defs^..traverse._PropertyDef) $ \prop ->
                         PropertyDef <$> trnsProperty prop

        return . Specification $ concat [constDefs, labelDefs, propDefs]

trnsConsts :: Trans [LDefinition]
trnsConsts =
    fmap concat . traverse (uncurry trnsConst) =<< view (constants.to assocs)

trnsConst :: Ident -> ConstSymbol -> Trans [LDefinition]
trnsConst ident (ConstSymbol l t ct e) = case e of
    ArrayExpr es _ -> fmap concat . for (zip (toList es) [0..]) $ \(e', i) ->
        trnsConst (indexedIdent ident i) (ConstSymbol l t ct e')
    _ -> do
        e' <- trnsExpr (const True) e
        return [ConstDef $ Constant ct ident e' l]

trnsGlobals :: Trans [LDefinition]
trnsGlobals = do
    gss <- filter (not . _gsIsAttrib) . toList <$> view globals
    fmap concat . for gss $ \(GlobalSymbol t _ decl) ->
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
activeDefs = fromList . fmap activeDef . allContexts where
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

