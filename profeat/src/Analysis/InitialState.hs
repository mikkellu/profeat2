{-# LANGUAGE TupleSections #-}

module Analysis.InitialState
  ( QualifiedVar(..)
  , ModelState

  , initialState
  ) where

import Control.Lens

import Data.Map   ( assocs )
import Data.Maybe ( fromMaybe )

import Symbols
import Syntax
import Types
import Types.Util

data QualifiedVar = QualifiedVar
  { qvScope :: !Scope
  , qvIdent :: !Ident
  , qvIndex :: Maybe Integer
  }

type ModelState = [(QualifiedVar, LExpr)]

initialState :: SymbolTable -> ModelState
initialState = concat . sequence [globalVars, localVars, controllerVars]

globalVars :: SymbolTable -> ModelState
globalVars symTbl = concatMap fromGlobalSymbol $ symTbl^..globals.traverse
  where
    fromGlobalSymbol (GlobalSymbol t (VarDecl ident _ mInit _)) =
        let i = fromMaybe (defaultInit t) mInit
        in fmap (,i) (qualifiedVars symTbl Global ident)

localVars :: SymbolTable -> ModelState
localVars symTbl = concatMap fromContext . allContexts $ symTbl^.rootFeature
  where
    fromContext ctx = concatMap (fromVarSymbol symTbl (Local ctx)) $
        ctx^.this.fsVars.to assocs

controllerVars :: SymbolTable -> ModelState
controllerVars symTbl = case symTbl^.controller of
    Just cts ->
        concatMap (fromVarSymbol symTbl LocalCtrlr) $ cts^.ctsVars.to assocs
    Nothing -> []

fromVarSymbol :: SymbolTable -> Scope -> (Ident, VarSymbol) -> ModelState
fromVarSymbol symTbl sc (ident, vs) =
    fmap (,vs^.vsInit) (qualifiedVars symTbl sc ident)

qualifiedVars :: SymbolTable -> Scope -> Ident -> [QualifiedVar]
qualifiedVars symTbl sc ident =
    let t = case sc of
                Global     -> symTbl^?!globals.at ident._Just.gsType
                Local ctx  -> ctx^?!this.fsVars.at ident._Just.vsType
                LocalCtrlr ->
                    symTbl^?!controller._Just.ctsVars.at ident._Just.vsType
    in case t of
        CompoundType (ArrayType (Just (lower, upper)) _) ->
            fmap (QualifiedVar sc ident . Just) [lower..upper]
        CompoundType _ -> error "Analysis.InitialState.qualifiedVars: array without bounds"
        SimpleType _ -> [QualifiedVar sc ident Nothing]

