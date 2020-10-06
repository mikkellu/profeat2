module VarOrder
  ( VarOrder(..)
  , Range(..)

  , VarMap
  , varMap
  ) where

import Control.Lens

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )
import Data.Text ( Text )
import Data.Text.Lazy ( toStrict )

import Text.PrettyPrint.Leijen.Text ( Pretty(..), Doc, dot, integer, brackets )

import Symbols
import Syntax hiding ( Range )
import Translator.Names ( fullyQualifiedIdent, activeIdent )
import Types


newtype VarOrder = VarOrder [(Doc, Range)]

data Range
  = RangeFeature !FeatureContext
  | RangeBool
  | Range !Int !Int


type VarMap = Map Text (Doc, Range)


varMap :: SymbolTable -> VarMap
varMap = mconcat . sequence [globalVars, moduleVars, controllerVars]

globalVars :: SymbolTable -> VarMap
globalVars = mconcat . fmap f . toListOf (globals.traverse)
  where
    f (GlobalSymbol t decl) = varToVarMap Global (declIdent decl) t

moduleVars :: SymbolTable -> VarMap
moduleVars symTbl =
    let ctxs = allContexts $ symTbl^.rootFeature
    in mconcat . flip concatMap ctxs $ \ctx ->
        fmap (localVars (ctx^.this.fsVars) (Local ctx))
             (ctx^..this.fsModules.traverse)

controllerVars :: SymbolTable -> VarMap
controllerVars symTbl =
    let vars =
            case symTbl^.controller of
                Just (ControllerSymbol vs body) -> localVars vs LocalCtrlr body
                Nothing -> mempty
        actVars = mconcat . fmap featVar . allContexts $ symTbl^.rootFeature
        attribVars = attributeVars symTbl
    in mconcat [actVars, attribVars, vars]
  where
    featVar ctx
        | ctx^.this.fsMandatory = mempty
        | otherwise =
            Map.singleton
                (toStrict (activeIdent ctx))
                (featureName ctx, RangeFeature ctx)
    featureName ctx = pretty (minimalPrefix (view rootFeature symTbl) ctx)

attributeVars :: SymbolTable -> VarMap
attributeVars symTbl = mconcat attribs
  where
    attribs =
        let ctxs = symTbl^.rootFeature.to allContexts
        in mconcat . flip fmap ctxs $ \ctx ->
            mapMaybe (attrib ctx) $ ctx^.this.fsVars.to Map.assocs
    attrib ctx (ident, vs)
      | vs^.vsIsAttrib =
          Just $ varToVarMap (Local ctx) ident (vs^.vsType)
      | otherwise = Nothing

localVars :: Table VarSymbol -> Scope -> ModuleBody a -> VarMap
localVars vs sc body =
    mconcat . flip fmap (modVars body) $ \decl ->
        let ident = declIdent decl
            t     = vs^?!at ident._Just.vsType
        in varToVarMap sc ident t

varToVarMap :: Scope -> Ident -> Type -> VarMap
varToVarMap sc ident t = case t of
    CompoundType (ArrayType (Just (lower, upper)) st) -> mconcat .
        fmap (uncurry (stToVarMap . Just)) . zip [lower..upper] $ repeat st
    CompoundType _ -> error "VarOrder.varToVarMap: array without range"
    SimpleType st -> stToVarMap Nothing st
  where
    name = toStrict . fullyQualifiedIdent sc ident
    stToVarMap idx st =
        let range = case st of
                BoolType                      -> RangeBool
                IntType (Just (lower, upper)) -> Range (fromInteger lower)
                                                       (fromInteger upper)
                IntType _                     -> error "VarOrder.varToVarMap: integer without range"
                DoubleType                    -> error "VarOrder.varToVarMap: illegal double type"
        in Map.singleton (name idx) (prettyName sc ident idx, range)


prettyName :: Scope -> Ident -> Maybe Integer -> Doc
prettyName sc ident idx = case sc of
    Local ctx  -> pretty ctx <> dot <> ident'
    LocalCtrlr -> ident'
    Global     -> ident'
  where
    ident' = pretty ident <> maybe mempty (brackets . integer) idx
