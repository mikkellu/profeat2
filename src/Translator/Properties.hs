{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Translator.Properties
  ( trnsProperty
  ) where

import Control.Lens
import Control.Monad

import Data.Maybe

import Syntax
import Symbols
import Template

import Translator.Common

trnsProperty :: Translator LProperty
trnsProperty = prepExprs >=> genFilterProperty

genFilterProperty :: Translator LProperty
genFilterProperty (Property ident es l) = do
    es' <- traverse trnsPropertyElem es
    lbl <- view initConfLabel
    let li = if isJust lbl
                 then initConfLabelIdent
                 else "init"
        filterPrefix = PropElemString "filter(printall, "
        filterSuffix = PropElemString (", \"" <> li <> "\")")
    return (Property ident (filterPrefix : es' ++ [filterSuffix]) l)
  where
    trnsPropertyElem = \case
        PropElemString s -> pure (PropElemString s)
        PropElemExpr e   -> PropElemExpr <$> trnsExpr (const True) e
