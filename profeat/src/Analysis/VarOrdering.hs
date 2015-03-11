{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Analysis.VarOrdering
  ( VarOrdering(..)
  , Range(..)

  , varOrdering
  ) where

import Control.Lens

import Data.List          ( sortBy )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N
import Data.Map ( assocs )
import Data.Maybe ( mapMaybe )
import Data.Ord ( comparing )
import Data.Semigroup

import Text.PrettyPrint.Leijen.Text ( Pretty(..), Doc )

import Symbols
import Syntax hiding ( Range )
import Syntax.Util
import Types

newtype VarOrdering = VarOrdering [(Doc, Range)] deriving (Show)

instance Monoid VarOrdering where
    mempty = VarOrdering []
    mappend (VarOrdering x) (VarOrdering y) = VarOrdering (x <> y)

data LVarOrdering = LVarOrdering
  { stripLoc :: VarOrdering
  , getLoc   :: !SrcLoc
  } deriving (Show)

instance Monoid LVarOrdering where
    mempty = LVarOrdering mempty noLoc
    mappend (LVarOrdering x l) (LVarOrdering y _) =
        LVarOrdering (x `mappend` y) l

mkLVarOrdering :: [(Doc, Range)] -> SrcLoc -> LVarOrdering
mkLVarOrdering vo = LVarOrdering (VarOrdering vo)

data Range
  = RangeFeature
  | RangeBool
  | Range !Int !Int
  | RangeInternal
  deriving (Show)

varOrdering :: SymbolTable -> VarOrdering
varOrdering = stripLoc .
              mconcat .
              sortBy (comparing getLoc) .
              concat .
              sequence [globalVars, moduleVars, controllerVars] -- TODO: globals are always first

globalVars :: SymbolTable -> [LVarOrdering]
globalVars = fmap globalToVarOrdering . toListOf (globals.traverse)
  where
    globalToVarOrdering (GlobalSymbol t decl) =
        toVarOrdering Global (declIdent decl) t (declAnnot decl)

moduleVars :: SymbolTable -> [LVarOrdering]
moduleVars symTbl =
    let ctxs = allContexts $ symTbl^.rootFeature
    in concat . flip fmap ctxs $ \ctx ->
        fmap (localVars (ctx^.this.fsVars) (Local ctx))
             (ctx^..this.fsModules.traverse)

controllerVars :: SymbolTable -> [LVarOrdering]
controllerVars symTbl =
    let (VarOrdering vars, l) =
            case symTbl^.controller of
                Just (ControllerSymbol vs body) ->
                    (stripLoc $ localVars vs LocalCtrlr body, modAnnot body)
                Nothing -> (mempty, noLoc)
        actVars = concatMap f . allContexts $ view rootFeature symTbl
        VarOrdering attribVars = stripLoc $ attributeVars symTbl
    in [mkLVarOrdering (actVars ++ attribVars ++ vars) l]
  where
    f ctx
      | ctx^.this.fsMandatory = []
      | otherwise             = [(pretty ctx, RangeFeature)]

attributeVars :: SymbolTable -> LVarOrdering
attributeVars symTbl = mconcat $ sortBy (comparing getLoc) attribs
  where
    attribs =
        let ctxs = symTbl^.rootFeature.to allContexts
        in concat . flip fmap ctxs $ \ctx ->
            mapMaybe (attrib ctx) $ ctx^.this.fsVars.to assocs
    attrib ctx (ident, vs)
      | vs^.vsIsAttrib =
          Just $ toVarOrdering (Local ctx) ident (vs^.vsType) (vs^.vsLoc)
      | otherwise = Nothing

localVars :: Table VarSymbol -> Scope -> LModuleBody -> LVarOrdering
localVars vs sc body =
    mconcat . flip fmap (sortVarDeclsByLoc (modVars body)) $ \decl ->
        let ident = declIdent decl
            t     = vs^?!at ident._Just.vsType
        in toVarOrdering sc ident t (modAnnot body)

toVarOrdering :: Scope -> Ident -> Type -> SrcLoc -> LVarOrdering
toVarOrdering sc ident t l = case t of
    CompoundType (ArrayType (Just (lower, upper)) st) -> mconcat .
        fmap (uncurry (stToVarOrdering . Just)) . zip [lower..upper] $ repeat st
    CompoundType _ -> error "Analysis.VarOrdering.toRange: array without range"
    SimpleType st -> stToVarOrdering Nothing st
  where
    stToVarOrdering idx st =
        let range = case st of
                BoolType                      -> RangeBool
                IntType (Just (lower, upper)) -> Range (fromInteger lower)
                                                       (fromInteger upper)
                IntType _                     -> error "Analysis.VarOrdering.toRange: integer without range"
                DoubleType                    -> error "Analysis.VarOrdering.toRange: illegal double type"
        in mkLVarOrdering [(prettyName sc ident idx, range)] l

prettyName :: Scope -> Ident -> Maybe Integer -> Doc
prettyName sc ident idx = pretty (toName sc ident idx)

toName :: Scope -> Ident -> Maybe Integer -> LName
toName sc ident idx =
    let localName = (ident, fmap intExpr idx)
    in case sc of
        Local ctx ->
            let qs = localName :|
                     fmap qualifier (N.init (getFeatureSymbols ctx))
            in Name (N.reverse qs) noLoc
        _         -> Name (localName :| []) noLoc
  where
    qualifier :: FeatureSymbol -> (Ident, Maybe LExpr)
    qualifier fs
      | fs^.fsIsMultiFeature = (fs^.fsIdent, Just (intExpr (fs^.fsIndex)))
      | otherwise            = (fs^.fsIdent, Nothing)

