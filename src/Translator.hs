{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Translator
  ( translateModel
  , translateSpec
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.Map ( assocs )
import qualified Data.Set as Set
import Data.Traversable

import Error
import Symbols
import Syntax
import Types

import Translator.Common
import Translator.Constraints
import Translator.Controller
import Translator.Modules
import Translator.Properties
import Translator.Rewards

translateModel :: SymbolTable -> Either Error LModel
translateModel symTbl = do
    (initConstrs, constrs) <- runReaderT extractConstraints (Env Global symTbl)

    flip runReaderT (trnsInfo symTbl constrs) $ do
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
    flip runReaderT (trnsInfo symTbl Set.empty) $ do
        constDefs <- trnsConsts
        labelDefs <- trnsLabelDefs defs
        propDefs  <- for (defs^..traverse._PropertyDef) $ \prop ->
                         PropertyDef <$> trnsProperty prop

        return . Specification $ concat [constDefs, labelDefs, propDefs]

trnsConsts :: Trans [LDefinition]
trnsConsts = fmap toConstDef <$> view (constants.to assocs)
  where
    toConstDef (ident, ConstSymbol l _ ct e) = ConstDef $ Constant ct ident e l

trnsGlobals :: Trans [LDefinition]
trnsGlobals = do
    globalTbl <- view globals
    fmap concat . for (globalTbl^..traverse) $ \(GlobalSymbol t decl) ->
        fmap GlobalDef <$> trnsVarDecl t decl

trnsLabelDefs :: Translator [LDefinition]
trnsLabelDefs defs = for (defs^..traverse._LabelDef) $ \lbl ->
    LabelDef <$> trnsLabel lbl

trnsLabels :: Translator [LLabel]
trnsLabels = traverse trnsLabel

trnsLabel :: Translator LLabel
trnsLabel = exprs (trnsExpr isBoolType)

