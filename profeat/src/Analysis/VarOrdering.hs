{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

module Analysis.VarOrdering
  ( VarOrdering(..)
  , Range(..)

  , varOrdering
  ) where

import Control.Lens

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N
import Data.Semigroup

import Text.PrettyPrint.Leijen.Text ( Pretty(..), Doc, text )

import Symbols
import Syntax hiding ( Range )
import Translator.Names ( seedVarIdent )
import Types

newtype VarOrdering = VarOrdering [(Doc, Range)] deriving (Monoid, Show)

data Range
  = RangeFeature
  | RangeBool
  | Range !Int !Int
  | RangeInternal
  deriving (Show)

varOrdering :: SymbolTable -> VarOrdering
varOrdering = mconcat . sequence [globalVars, moduleVars, featureVars]

globalVars :: SymbolTable -> VarOrdering
globalVars symTbl =
    mconcat . flip fmap (symTbl^..globals.traverse) $ \(GlobalSymbol t decl) ->
        toVarOrdering Global (declIdent decl) t

moduleVars :: SymbolTable -> VarOrdering
moduleVars symTbl =
    let ctxs = allContexts $ symTbl^.rootFeature
    in mconcatFor ctxs $ \ctx ->
        mconcatFor (ctx^..this.fsModules.traverse) $ \body ->
            mconcatFor (modVars body) $ \decl ->
                let ident = declIdent decl
                    t     = ctx^?!this.fsVars.at ident._Just.vsType
                in toVarOrdering (Local ctx) ident t
  where
    mconcatFor xs f = mconcat (fmap f xs)

featureVars :: SymbolTable -> VarOrdering
featureVars symTbl =
    let fvars   = concatMap f . allContexts $ view rootFeature symTbl
        seedVar = [(text seedVarIdent, RangeInternal)]
    in VarOrdering $ seedVar ++ fvars
  where
    f ctx
      | ctx^.this.fsMandatory = []
      | otherwise             = [(pretty ctx, RangeFeature)]

toVarOrdering :: Scope -> Ident -> Type -> VarOrdering
toVarOrdering sc ident = \case
    CompoundType (ArrayType (Just (lower, upper)) st) -> mconcat .
        fmap (uncurry (stToVarOrdering . Just)) . zip [lower..upper] $ repeat st
    CompoundType _ -> error "Analysis.VarOrdering.toRange: array without range"
    SimpleType st -> stToVarOrdering Nothing st
  where
    stToVarOrdering idx =
        VarOrdering . (:[]) . (prettyName sc ident idx,) . \case
            BoolType                      -> RangeBool
            IntType (Just (lower, upper)) -> Range (fromInteger lower)
                                                   (fromInteger upper)
            IntType _                     -> error "Analysis.VarOrdering.toRange: integer without range"
            DoubleType                    -> error "Analysis.VarOrdering.toRange: illegal double type"

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

