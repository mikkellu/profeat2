{-# LANGUAGE TupleSections #-}

module Analysis.InitialState
  ( QualifiedVar(..)
  , ModelState

  , initialState
  ) where

import Control.Lens

import Data.Map   ( assocs )

import Symbols
import Syntax
import Types

data QualifiedVar = QualifiedVar
  { qvScope :: !Scope
  , qvIdent :: !Ident
  , qvIndex :: Maybe Integer
  }

type ModelState = [(QualifiedVar, LExpr)]

initialState :: SymbolTable -> ModelState
initialState = concat . sequence [globalVars, localVars, controllerVars]

globalVars :: SymbolTable -> ModelState
globalVars = concatMap fromGlobalSymbol . toListOf (globals.traverse)
  where
    fromGlobalSymbol (GlobalSymbol t (VarDecl ident _ mInit _)) =
        modelState mInit t Global ident

localVars :: SymbolTable -> ModelState
localVars symTbl = concatMap fromContext . allContexts $ symTbl^.rootFeature
  where
    fromContext ctx = concatMap (fromVarSymbol (Local ctx)) $
        ctx^.this.fsVars.to assocs

controllerVars :: SymbolTable -> ModelState
controllerVars symTbl = case symTbl^.controller of
    Just cts ->
        concatMap (fromVarSymbol LocalCtrlr) $ cts^.ctsVars.to assocs
    Nothing -> []

fromVarSymbol :: Scope -> (Ident, VarSymbol) -> ModelState
fromVarSymbol sc (ident, vs) = modelState (vs^.vsInit) (vs^.vsType) sc ident

modelState :: Maybe LExpr -> Type -> Scope -> Ident -> ModelState
modelState mInit t sc ident = case mInit of
    Just i  -> fmap (,i) (qualifiedVars t sc ident)
    Nothing -> []

qualifiedVars :: Type -> Scope -> Ident -> [QualifiedVar]
qualifiedVars t sc ident = case t of
    CompoundType (ArrayType (Just (lower, upper)) _) ->
        fmap (QualifiedVar sc ident . Just) [lower..upper]
    CompoundType _ -> error "Analysis.InitialState.qualifiedVars: array without bounds"
    SimpleType _ -> [QualifiedVar sc ident Nothing]

