{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Translator
  ( SeedingAlg(..)

  , translateModel
  , translateSpec
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.Map ( assocs )
import Data.Maybe
import qualified Data.Set as Set
import Data.Traversable

import Error
import Symbols
import Syntax
import Template
import Types

import Translator.Common
import Translator.Constraints
import Translator.Controller
import Translator.Modules
import Translator.Properties
import Translator.Rewards

translateModel :: SeedingAlg -> SymbolTable -> Either Error LModel
translateModel alg symTbl = do
    (initConstrs, constrs) <- runReaderT extractConstraints (Env Global symTbl)

    flip runReaderT (trnsInfo alg symTbl constrs) $ do
        (controllerDef, lss) <- trnsControllerDef initConstrs
        local (labelSets .~ lss) $ do
            constDefs   <- trnsConsts
            globalDefs  <- trnsGlobals
            moduleDefs  <- trnsModules
            labelDefs   <- fmap LabelDef <$>
                               trnsLabels (symTbl^..labels.traverse)
            rewardsDefs <- trnsRewards

            return . Model $ concat [ constDefs
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
    flip runReaderT (trnsInfo SeedingFeatureDiagram symTbl Set.empty) $ do
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

