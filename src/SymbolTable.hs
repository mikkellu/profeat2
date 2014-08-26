{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}

module SymbolTable
  ( module Symbols
  , extendSymbolTable
  ) where

import Control.Applicative
import Control.Lens hiding ( contains )
import Control.Monad.Reader
import Control.Monad.State

import Data.List ( (\\) )
import Data.Map ( Map, keys, member )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable

import Error
import Eval
import ProductLine
import Symbols
import Syntax
import Syntax.Util
import Template
import Typechecker
import Types.Util

extendSymbolTable :: SymbolTable -> [LDefinition] -> Either Error SymbolTable
extendSymbolTable symTbl defs = flip evalStateT symTbl $ do -- TODO: refactor
    forOf_ (traverse._GlobalDef) defs $ \decl@(VarDecl ident vt _ l) ->
        ifNot containsSymbol ident l $
            globals.at ident .= Just (GlobalSymbol (fromVarType' vt) decl)

    forOf_ (traverse._ConstDef) defs $ \(Constant ct ident e l) ->
        ifNot containsSymbol ident l $ constants.at ident .=
            Just (ConstSymbol l (fromConstType ct) ct e)

    forOf_ (traverse._FormulaDef) defs $ \f@(Formula ident _ _ l) ->
        ifNot containsSymbol ident l $ formulas.at ident .= Just f

    checkIfNonCyclicFormulas =<< use formulas

    forOf_ (traverse._ModuleDef) defs $ \m@(Module ident _ _ _) ->
        ifNot containsModule ident (modAnnot $ modBody m) $
            modules.at ident .= Just m

    forOf_ (traverse._FeatureDef) defs $ \f -> let ident = featIdent f in
        ifNot containsFeature ident (featAnnot f) $ features.at ident .= Just f

    checkIfNonCyclicFeatures =<< use features

    expandExprsOf $ constants.traverse.csExpr
    checkIfNonCyclicConstants =<< use constants
    evalConstValues

    symTbl' <- get

    forOf_ (traverse._ControllerDef) defs $ \(Controller body) ->
        ifNot containsController "controller" (modAnnot body) $ do
            body' <- runReaderT (prepModuleBody body) (Env LocalCtrlr symTbl')
            controller .= Just (ControllerSymbol Map.empty body')
    symTbl' <- get

    symTbl'' <- flip runReaderT (Env Global symTbl') .
                forOf (globals.traverse) symTbl' $ \gs -> do
        t <- fromVarType $ gs^.gsDecl.to declType
        return (gs & gsType .~ t)
    put symTbl''

    root <- rootFeatureSymbol symTbl''
    setControllerVarTypes

    symTbl''' <- get
    return $ symTbl''' & rootFeature .~ root
  where
    ifNot contains ident loc m = do
        st <- get
        case st `contains` ident of
            Just l' -> throw loc $ MultipleDeclarations ident l'
            Nothing -> m

setControllerVarTypes :: ( Applicative m
                         , MonadState SymbolTable m
                         , MonadEither Error m
                         )
                      => m ()
setControllerVarTypes = do
    symTbl <- get
    let cts = symTbl^.controller
    void . flip runReaderT (Env LocalCtrlr symTbl) $
        for (cts^.._Just.ctsBody.to modVars.traverse) $
            \(VarDecl ident vt _ l) -> do
                vs <- VarSymbol l False <$> fromVarType vt
                controller._Just.ctsVars.at ident .= Just vs

expandExprsOf :: (Applicative m, MonadState SymbolTable m, MonadEither Error m)
              => Traversal' SymbolTable LExpr
              -> m ()
expandExprsOf t = do
    symTbl <- get
    put =<< t (expandFormulas (symTbl^.formulas)) symTbl

evalConstValues :: ( Applicative m
                   , MonadState SymbolTable m
                   , MonadEither Error m
                   )
                => m ()
evalConstValues = do
    symTbl <- get
    void . flip runReaderT (Env Global symTbl) $
        for (symTbl^.constants.to keys) evalConstValue

evalConstValue :: ( Applicative m
                  , MonadState SymbolTable m
                  , MonadReader Env m
                  , MonadEither Error m
                  )
               => Ident
               -> m ()
evalConstValue ident = do
    val <- use constValues
    unless (ident `member` val) $ do -- check if we already evaluated the constant
        symTbl <- ask
        case symTbl^.constants.at ident of
            Nothing -> return () -- there is no constant with the given identifier; ignore it, as undefined variables will be catched by the type checker
            Just cs -> do
                let e = cs^.csExpr

                forOf_ (traverse.identifiers) (universe e) evalConstValue -- make sure all referenced constants are evaluated
                val' <- use constValues -- load new valuation

                local (constValues .~ val') $ do
                    e' <- unrollLoopExprs e
                    checkInitialization (cs^.csType) e'

                    v <- eval' val' e'

                    constValues.at ident            .= Just v
                    constants.at ident._Just.csExpr .= e'

checkIfNonCyclicFormulas :: (Applicative m, MonadEither Error m)
                         => Table LFormula
                         -> m ()
checkIfNonCyclicFormulas = checkIfNonCyclic post frmAnnot where
    post (Formula _ params e _) = universe e^..traverse.identifiers \\ params

checkIfNonCyclicConstants :: (Applicative m, MonadEither Error m)
                          => Table ConstSymbol
                          -> m ()
checkIfNonCyclicConstants = checkIfNonCyclic post (view csLoc) where
    post cs = universe (cs^.csExpr)^..traverse.identifiers

checkIfNonCyclicFeatures :: (Applicative m, MonadEither Error m)
                         => Table LFeature
                         -> m ()
checkIfNonCyclicFeatures = checkIfNonCyclic post featAnnot where
    post f = case featDecomp f of
        Just (Decomposition _ refs _) -> map (instIdent . frInstance) refs
        Nothing                       -> []

checkIfNonCyclic :: (Applicative m, MonadEither Error m)
                 => (a -> [Ident])
                 -> (a -> SrcLoc)
                 -> Map Ident a
                 -> m ()
checkIfNonCyclic post annot defs = void . for (Map.assocs defs) $ \(i, x) ->
    when (isCyclicDef post defs i) . throw (annot x) $ CyclicDependency i

isCyclicDef :: (a -> [Ident]) -> Map Ident a -> Ident -> Bool
isCyclicDef post defs = isCyclic Set.empty
  where
    isCyclic is i
      | i `Set.member` is = True
      | otherwise =
        case defs^.at i of
            Just x  -> let is' = Set.insert i is in any (isCyclic is') $ post x
            Nothing -> False

