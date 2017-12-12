module FeatureVars
  ( toCsv
  ) where

import Control.Lens

import Data.Maybe ( mapMaybe )

import Text.PrettyPrint.Leijen.Text

import Symbols
import Translator.Names


toCsv :: FeatureSymbol -> Doc
toCsv root = vsep . mapMaybe (featureMapping root) . allContexts $ root

featureMapping :: FeatureSymbol -> FeatureContext -> Maybe Doc
featureMapping root ctx
  | ctx^.this.fsMandatory = Nothing
  | otherwise = Just $ featureName <> comma <> varName
  where
    featureName = pretty (minimalPrefix root ctx)
    varName     = text (activeIdent ctx)
