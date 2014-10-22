module Translator.Properties
  ( trnsProperty
  ) where

import Syntax

import Translator.Common

trnsProperty :: Translator LProperty
trnsProperty = exprs (trnsExpr (const True))

