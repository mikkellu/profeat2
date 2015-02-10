{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Translator.Names
  ( fullyQualifiedName
  , fullyQualifiedIdent

  , activeFormulaName
  , activeFormulaIdent
  , activeName
  , activeIdent

  , controllerIdent
  , moduleIdent

  , labelInfoIdent

  , reconfIdent

  , seedVarName
  , seedVarIdent

  , operatingName
  , operatingIdent
  ) where

import Data.Monoid

import Control.Lens

import Symbols
import Syntax
import Syntax.Util
import Typechecker ( LabelInfo(..) )
import Types

fullyQualifiedName :: Scope -> Ident -> Maybe Integer -> SrcLoc -> LName
fullyQualifiedName sc ident idx l =
    review _Ident (fullyQualifiedIdent sc ident idx, l)

fullyQualifiedIdent :: Scope -> Ident -> Maybe Integer -> Ident
fullyQualifiedIdent sc ident idx =
    let prefix = case sc of
            Local ctx  -> contextIdent ctx `snoc` '_'
            LocalCtrlr -> "__"
            Global     -> ""
        ident' = prefix <> ident
    in case idx of
           Just i  -> indexedIdent ident' i
           Nothing -> ident'

labelInfoIdent :: LabelInfo -> Ident
labelInfoIdent (LabelInfo sc ident idx) = fullyQualifiedIdent sc ident idx

activeFormulaName :: FeatureContext -> LName
activeFormulaName = toName . activeFormulaIdent

activeFormulaIdent :: FeatureContext -> Ident
activeFormulaIdent ctx = activeIdent ctx <> "_active"

activeName :: FeatureContext -> LName
activeName = toName . activeIdent

activeIdent :: FeatureContext -> Ident
activeIdent = contextIdent

controllerIdent :: Ident
controllerIdent = "_controller"

moduleIdent :: FeatureContext -> Ident -> Ident
moduleIdent ctx ident = contextIdent ctx <> ('_' `cons` ident)

reconfIdent :: ReconfType -> Ident
reconfIdent ReconfActivate   = "activate"
reconfIdent ReconfDeactivate = "deactivate"

seedVarName :: LName
seedVarName = toName seedVarIdent

seedVarIdent :: Ident
seedVarIdent = "__loc"

operatingName :: LName
operatingName = toName operatingIdent

operatingIdent :: Ident
operatingIdent = "_operating"

toName :: Ident -> LName
toName = review _Ident . (,noLoc)

