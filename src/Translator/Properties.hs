module Translator.Properties
  ( trnsProperty
  ) where

import Control.Lens

import Symbols
import Syntax

import Translator.Common

trnsProperty :: Translator LProperty
trnsProperty prop = do
    root <- view rootFeature
    if hasSingleConfiguration root
        then exprs (trnsExpr (const True)) prop
        else genFilterProperty prop

genFilterProperty :: Translator LProperty
genFilterProperty (Property ident e l) = do
    e' <- trnsExpr (const True) e
    let labelExpr  = LabelExpr initConfLabelIdent noLoc
        filterExpr = FilterExpr FilterPrint e' (Just labelExpr) noLoc
    return $ Property ident filterExpr l

