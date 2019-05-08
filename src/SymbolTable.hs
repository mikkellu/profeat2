{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module SymbolTable
  ( module Symbols

  , extendSymbolTable
  , updateSymbolTable
  ) where

import Control.Lens hiding ( contains )
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable ( for_, toList )
import Data.List ( (\\) )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map ( Map, keys, member )
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Traversable

import Error
import Eval
import Functions
import ProductLine
import Symbols
import Syntax
import Syntax.Util
import Template
import Typechecker
import Types
import Types.Util

extendSymbolTable :: SymbolTable -> [LDefinition] -> Either Error SymbolTable
extendSymbolTable symTbl defs = flip execStateT symTbl $ do -- TODO: refactor
    forOf_ (traverse._ConstDef) defs $ \(Constant ct ident e l) ->
        ifNot containsSymbol ident l $ constants.at ident .=
            Just (ConstSymbol l (fromConstType ct e) ct e)

    forOf_ (traverse._FormulaDef) defs $ \f@(Formula ident _ _ l) ->
        ifNot containsSymbol ident l $ formulas.at ident .= Just f

    checkIfNonCyclicFormulas =<< use formulas

    forOf_ (traverse._LabelDef) defs $ \lbl@(Label ident _ l) ->
        ifNot containsSymbol ident l $ labels.at ident .= Just lbl

    findInitConfLabel

    forOf_ (traverse._ModuleDef) defs $ \m@(Module ident _ _) ->
        ifNot containsModule ident (modAnnot $ modBody m) $
            modules.at ident .= Just m

    forOf_ (traverse._FeatureDef) defs $ \f -> let ident = featIdent f in
        ifNot containsFeature ident (featAnnot f) $ features.at ident .= Just f

    checkIfNonCyclicFeatures =<< use features

    expandExprsOf $ constants.traverse.csExpr.traverse
    checkIfNonCyclicConstants =<< use constants
    evalConstValues

    symTbl' <- get
    fams <- lift (getFamily symTbl' defs)
    familySym .= fams


getFamily :: SymbolTable -> [LDefinition] -> Either Error (Maybe FamilySymbol)
getFamily symTbl defs = flip runReaderT (Env Global symTbl) $
    case defs^..traverse._FamilyDef of
        f1:f2:_ -> throw (famAnnot f2) $
                       MultipleDeclarations "family" (famAnnot f1)
        [Family{..}] -> do
            paramTbl <- execStateT (for famParameters addParamSymbol) Map.empty
            constrs  <- local (globals .~ paramSymsToGlobalSyms paramTbl) $
                for famConstraints checkConstraint
            feats <- traverse prepExprs famFeatures
            return . Just $ FamilySymbol paramTbl constrs feats
        [] -> return Nothing
  where
    addParamSymbol decl@(VarDecl ident _ _ l) = do
        tbl <- get
        case tbl^?at ident._Just.psDecl.to declAnnot of
            Just l'  -> throw l $ MultipleDeclarations ident l'
            Nothing -> do
                ps <- toParamSymbol decl
                at ident .= Just ps
    checkConstraint e = do
        e' <- prepExpr e
        checkIfType_ isBoolType e'
        return e'

updateSymbolTable :: SymbolTable -> [LDefinition] -> Either Error SymbolTable
updateSymbolTable symTbl defs = flip evalStateT symTbl $ do -- TODO: refactor
    addGlobals defs

    symTbl'' <- get
    forOf_ (traverse._ControllerDef) defs $ \(Controller body) ->
        ifNot containsController "controller" (modAnnot body) $ do
            body' <- runReaderT (prepModuleBody body) (Env LocalCtrlr symTbl'')
            controller .= Just (ControllerSymbol Map.empty body')
    symTbl''' <- get

    forOf_ (traverse._InitDef) defs $ \(Init e _) -> do
        e' <- runReaderT (prepExpr e) (Env Global symTbl''')
        initConfExpr %= Just . maybe e' (`lAnd` e')
    symTbl'''' <- get

    forOf_ (traverse._InvariantDef) defs $ \(Invariant e l) ->
        ifNot containsInvariant "invariant" l $ do
            e' <- runReaderT (prepExpr e) (Env Global symTbl'''')
            invariantExpr .= Just e'
    symTbl''''' <- get

    root <- rootFeatureSymbol symTbl'''''
    setControllerVarTypes

    symTbl'''''' <- get
    return $ symTbl'''''' & rootFeature .~ root

addGlobals :: (MonadState SymbolTable m, MonadError Error m)
           => [LDefinition]
           -> m ()
addGlobals defs = do
    symTbl <- get
    forOf_ (traverse._GlobalDef) defs $ \decl@(VarDecl ident vt _ l) ->
        ifNot containsSymbol ident l $
            flip runReaderT (Env Global symTbl) $ do
                decl' <- prepExprs decl
                t     <- fromVarType vt
                globals.at ident .= Just (GlobalSymbol t decl')

ifNot :: (MonadState SymbolTable m, MonadError Error m)
      => (SymbolTable -> Ident -> Maybe SrcLoc)
      -> Ident
      -> SrcLoc
      -> m ()
      -> m ()
ifNot contains ident loc m = do
    st <- get
    case st `contains` ident of
        Just l' -> throw loc $ MultipleDeclarations ident l'
        Nothing -> m

setControllerVarTypes :: (MonadState SymbolTable m, MonadError Error m) => m ()
setControllerVarTypes = do
    symTbl <- get
    let cts = symTbl^.controller
    void . flip runReaderT (Env LocalCtrlr symTbl) $
        for (cts^.._Just.ctsBody.to modVars.traverse) $ \decl -> do
            vs <- toVarSymbol False decl
            controller._Just.ctsVars.at (declIdent decl) .= Just vs

findInitConfLabel :: (MonadState SymbolTable m) => m ()
findInitConfLabel = use (labels.at initConfLabelIdent) >>= \case
    Just lbl -> initConfLabel .= Just (lblExpr lbl)
    Nothing  -> return ()

expandExprsOf :: (MonadState SymbolTable m, MonadError Error m)
              => Traversal' SymbolTable LExpr
              -> m ()
expandExprsOf t = do
    symTbl <- get
    put =<< t (expandFormulas (symTbl^.formulas)) symTbl

evalConstValues :: (MonadState SymbolTable m, MonadError Error m) => m ()
evalConstValues = do
    symTbl <- get
    void . flip runReaderT (Env Global symTbl) $
        for (symTbl^.constants.to keys) evalConstValue

evalConstValue :: ( MonadState SymbolTable m
                  , MonadReader Env m
                  , MonadError Error m
                  )
               => Ident
               -> m ()
evalConstValue ident = do
    val <- use constValues
    unless ((ident, 0) `member` val) $ do -- check if we already evaluated the constant
        symTbl <- ask
        case symTbl^.constants.at ident of
            Nothing -> return () -- there is no constant with the given identifier; ignore it, as undefined variables will be catched by the type checker
            Just cs -> case cs^.csExpr of
                Nothing -> return () -- constant is not initialized, skip it
                Just e -> do
                    forOf_ (traverse.identifiers) (universe e) evalConstValue -- make sure all referenced constants are evaluated
                    val' <- use constValues -- load new valuation

                    local (constValues .~ val') $ do
                        e' <- unrollLoopExprs e
                        t <- case cs^.csType of
                            CompoundType (ArrayType Nothing _) -> typeOf e'
                            t -> return t
                        constants.at ident._Just.csType .= t

                        checkInitialization t e'

                        e's <- case e' of
                            ArrayExpr es _ -> return (toList es)
                            CallExpr (FuncExpr FuncBinom _) [ep, en] _ -> do
                                p <- eval' val' ep >>= \case
                                    DblVal p' -> return p'
                                    _ -> error "evalConstValue: type error"
                                n <- eval' val' en >>= \case
                                    IntVal n' -> return n'
                                    _ -> error "evalConstValue: type error"
                                return . fmap (flip DecimalExpr noLoc) $
                                    binomialDist p n
                            _ -> return [e']

                        for_ (zip e's [0..]) $ \(e'', i) -> do
                            v <- eval' val' e''
                            constValues.at (ident, i)       .= Just v
                            constants.at ident._Just.csExpr .= Just e'

checkIfNonCyclicFormulas :: (MonadError Error m) => Table LFormula -> m ()
checkIfNonCyclicFormulas = checkIfNonCyclic post frmAnnot where
    post (Formula _ params e _) = universe e^..traverse.identifiers \\ params

checkIfNonCyclicConstants :: (MonadError Error m) => Table ConstSymbol -> m ()
checkIfNonCyclicConstants = checkIfNonCyclic post (view csLoc) where
    post cs = case cs^.csExpr of
        Just e  -> mapMaybe f (universe e)
        Nothing -> []

    f (NameExpr (Name ((ident, _) :| []) _) _) = Just ident
    f _                                        = Nothing

checkIfNonCyclicFeatures :: (MonadError Error m) => Table LFeature -> m ()
checkIfNonCyclicFeatures = checkIfNonCyclic post featAnnot where
    post f = case featDecomp f of
        Just (Decomposition _ refs _) -> map (instIdent . frInstance) refs
        Nothing                       -> []

checkIfNonCyclic :: (MonadError Error m)
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

