{-# LANGUAGE OverloadedStrings #-}

module Translator.Names
  ( fullyQualifiedName
  , fullyQualifiedIdent

  , activeName
  , activeIdent

  , moduleIdent

  , seedVarName
  , seedVarIdent
  ) where

import Data.Monoid

import Control.Lens

import Symbols
import Syntax

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

activeIdent :: FeatureContext -> Ident
activeIdent = contextIdent

moduleIdent :: FeatureContext -> Ident -> Ident
moduleIdent ctx ident = contextIdent ctx <> ('_' `cons` ident)

seedVarName :: LName
seedVarName = review _Ident (seedVarIdent, noLoc)

seedVarIdent :: Ident
seedVarIdent = "__loc"

