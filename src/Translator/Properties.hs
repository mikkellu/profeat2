{-# LANGUAGE OverloadedStrings #-}

module Translator.Properties
  ( trnsProperty
  ) where

import Control.Lens
import Control.Monad

import Data.Maybe

import Symbols
import Syntax
import Template

import Translator.Common

trnsProperty :: Translator LProperty
trnsProperty = prepExprs >=> genFilterProperty

genFilterProperty :: Translator LProperty
genFilterProperty (Property ident e l) = do
    e'  <- trnsExpr (const True) e
    lbl <- view initConfLabel
    let li         = if isJust lbl
                         then initConfLabelIdent
                         else "init"
        labelExpr  = LabelExpr li noLoc
        filterExpr = FilterExpr FilterPrint e' (Just labelExpr) noLoc
    return $ Property ident filterExpr l
