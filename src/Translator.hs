{-# LANGUAGE FlexibleContexts #-}

module Translator
  ( translateModel
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.Map ( assocs )
import Data.Monoid
import Data.Text.Lazy ( pack )
import Data.Traversable

import Error
import Symbols
import Syntax
import Template
import Typechecker
import Types

type Trans a = ReaderT Env (Either Error) a

translateModel :: SymbolTable -> Either Error LModel
translateModel symTbl = flip runReaderT (Env Global symTbl) $ do
    constDefs     <- trnsConsts
    globalDefs    <- trnsGlobals
    moduleDefs    <- trnsModules
    controllerDef <- trnsController

    return . Model $ concat [ constDefs
                            , globalDefs
                            , moduleDefs
                            , toList controllerDef
                            ]

trnsConsts :: Trans [LDefinition]
trnsConsts = fmap toConstDef <$> view (constants.to assocs)
  where
    toConstDef (ident, ConstSymbol l _ ct e) = ConstDef $ Constant ct ident e l

trnsGlobals :: Trans [LDefinition]
trnsGlobals = do
    globalTbl <- view globals
    fmap concat . for (globalTbl^..traverse) $ \(GlobalSymbol t decl) ->
        fmap GlobalDef <$> trnsVarDecl t decl

trnsModules :: Trans [LDefinition]
trnsModules = return []

trnsController :: Trans (Maybe LDefinition)
trnsController = return Nothing

trnsVarDecl :: Type -> LVarDecl -> Trans [LVarDecl]
trnsVarDecl t (VarDecl ident vt e l) = do
    e' <- _Just preprocessExpr e
    void $ _Just (checkInitialization t) e'

    case t of
        CompoundType (ArrayType (Just (lower, upper)) _) -> do
            let CompoundVarType (ArrayVarType _ svt) = vt
            vt' <- SimpleVarType <$> exprs preprocessExpr svt

            for [lower .. upper] $ \i ->
                let ident' = identIndex i ident
                in return $ VarDecl ident' vt' e' l
        _ -> do
            vt' <- exprs preprocessExpr vt
            return [VarDecl ident vt' e' l]

identIndex :: Integer -> Ident -> Ident
identIndex i ident = ident <> ('_' `cons` indexToText i)
  where
    indexToText j
      | j >= 0 = pack (show j)
      | otherwise = '_' `cons` (pack . show $ negate j)

