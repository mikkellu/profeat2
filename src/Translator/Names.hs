{-# LANGUAGE OverloadedStrings #-}

module Translator.Names
  ( fullyQualifiedName
  , fullyQualifiedIdent

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

activeName :: FeatureContext -> LName
activeName ctx = review _Ident (activeIdent ctx, noLoc)

labelInfoIdent :: LabelInfo -> Ident
labelInfoIdent (LabelInfo sc ident idx) = fullyQualifiedIdent sc ident idx

activeIdent :: FeatureContext -> Ident
activeIdent = contextIdent

controllerIdent :: Ident
controllerIdent = "_ident"

moduleIdent :: FeatureContext -> Ident -> Ident
moduleIdent ctx ident = contextIdent ctx <> ('_' `cons` ident)

reconfIdent :: ReconfType -> Ident
reconfIdent ReconfActivate   = "activate"
reconfIdent ReconfDeactivate = "deactivate"

seedVarName :: LName
seedVarName = review _Ident (seedVarIdent, noLoc)

seedVarIdent :: Ident
seedVarIdent = "__loc"

operatingName :: LName
operatingName = review _Ident (operatingIdent, noLoc)

operatingIdent :: Ident
operatingIdent = "_operating"

