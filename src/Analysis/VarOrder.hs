{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Analysis.VarOrder
  ( VarOrder(..)
  , Range(..)

  , varOrder
  ) where

import Control.Lens

import Data.List          ( sortBy )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map ( assocs )
import Data.Maybe ( mapMaybe )
import Data.Ord ( comparing )
import Data.Semigroup

import Text.PrettyPrint.Leijen.Text ( Pretty(..), Doc )

import Symbols
import Syntax hiding ( Range )
import Syntax.Util
import Types

newtype VarOrder = VarOrder [(Doc, Range)] deriving (Show)

instance Monoid VarOrder where
    mempty = VarOrder []
    mappend (VarOrder x) (VarOrder y) = VarOrder (x <> y)

data LVarOrder = LVarOrder
  { stripLoc :: VarOrder
  , getLoc   :: !SrcLoc
  } deriving (Show)

instance Monoid LVarOrder where
    mempty = LVarOrder mempty noLoc
    mappend (LVarOrder x l) (LVarOrder y _) =
        LVarOrder (x `mappend` y) l

mkLVarOrder :: [(Doc, Range)] -> SrcLoc -> LVarOrder
mkLVarOrder vo = LVarOrder (VarOrder vo)

data Range
  = RangeFeature
  | RangeBool
  | Range !Int !Int
  deriving (Show)

varOrder :: SymbolTable -> VarOrder
varOrder symTbl =
    let gs = sortBy (comparing getLoc) $ globalVars symTbl
        ls = sortBy (comparing getLoc) . concat $
             sequence [moduleVars, controllerVars] symTbl
    in stripLoc . mconcat $ gs ++ ls

globalVars :: SymbolTable -> [LVarOrder]
globalVars = fmap globalToVarOrder . toListOf (globals.traverse)
  where
    globalToVarOrder (GlobalSymbol t decl) =
        toVarOrder Global (declIdent decl) t (declAnnot decl)

moduleVars :: SymbolTable -> [LVarOrder]
moduleVars symTbl =
    let ctxs = allContexts $ symTbl^.rootFeature
    in concat . flip fmap ctxs $ \ctx ->
        fmap (localVars (ctx^.this.fsVars) (Local ctx))
             (ctx^..this.fsModules.traverse)

controllerVars :: SymbolTable -> [LVarOrder]
controllerVars symTbl =
    let (VarOrder vars, l) =
            case symTbl^.controller of
                Just (ControllerSymbol vs body) ->
                    (stripLoc $ localVars vs LocalCtrlr body, modAnnot body)
                Nothing -> (mempty, noLoc)
        actVars = concatMap f . allContexts $ view rootFeature symTbl
        VarOrder attribVars = stripLoc $ attributeVars symTbl
    in [mkLVarOrder (actVars ++ attribVars ++ vars) l]
  where
    f ctx
      | ctx^.this.fsMandatory = []
      | otherwise = [(pretty (rootContext (ctx^.this)), RangeFeature)]

attributeVars :: SymbolTable -> LVarOrder
attributeVars symTbl = mconcat $ sortBy (comparing getLoc) attribs
  where
    attribs =
        let ctxs = symTbl^.rootFeature.to allContexts
        in concat . flip fmap ctxs $ \ctx ->
            mapMaybe (attrib ctx) $ ctx^.this.fsVars.to assocs
    attrib ctx (ident, vs)
      | vs^.vsIsAttrib =
          Just $ toVarOrder (Local ctx) ident (vs^.vsType) (vs^.vsLoc)
      | otherwise = Nothing

localVars :: Table VarSymbol -> Scope -> LModuleBody -> LVarOrder
localVars vs sc body =
    mconcat . flip fmap (sortVarDeclsByLoc (modVars body)) $ \decl ->
        let ident = declIdent decl
            t     = vs^?!at ident._Just.vsType
        in toVarOrder sc ident t (modAnnot body)

toVarOrder :: Scope -> Ident -> Type -> SrcLoc -> LVarOrder
toVarOrder sc ident t l = case t of
    CompoundType (ArrayType (Just (lower, upper)) st) -> mconcat .
        fmap (uncurry (stToVarOrder . Just)) . zip [lower..upper] $ repeat st
    CompoundType _ -> error "Analysis.VarOrder.toRange: array without range"
    SimpleType st -> stToVarOrder Nothing st
  where
    stToVarOrder idx st =
        let range = case st of
                BoolType                      -> RangeBool
                IntType (Just (lower, upper)) -> Range (fromInteger lower)
                                                       (fromInteger upper)
                IntType _                     -> error "Analysis.VarOrder.toRange: integer without range"
                DoubleType                    -> error "Analysis.VarOrder.toRange: illegal double type"
        in mkLVarOrder [(prettyName sc ident idx, range)] l

prettyName :: Scope -> Ident -> Maybe Integer -> Doc
prettyName sc ident idx = pretty (toName sc ident idx)

toName :: Scope -> Ident -> Maybe Integer -> LName
toName _ ident idx = Name ((ident, fmap intExpr idx) :| []) noLoc
