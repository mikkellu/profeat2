{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module FeatureDiagram
  ( writeFeatureDiagram
  ) where

import Symbols

import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy.IO as LIO

import Text.PrettyPrint.Leijen.Text


type FeatureIds = Map FeatureSymbol Doc


writeFeatureDiagram :: FilePath -> FeatureSymbol -> IO ()
writeFeatureDiagram fileName root =
    LIO.writeFile fileName (renderFeatureDiagram root)

renderFeatureDiagram :: FeatureSymbol -> Text
renderFeatureDiagram = displayT . renderPretty 0.4 80 . featureDiagram

featureDiagram :: FeatureSymbol -> Doc
featureDiagram root = featureNodes (featureIds root) root

featureIds :: FeatureSymbol -> FeatureIds
featureIds root = Map.fromList . fmap mkId . allContexts $ root where
    mkId ctx = (thisFeature ctx, pretty (minimalPrefix root ctx))

featureNodes :: FeatureIds -> FeatureSymbol -> Doc
featureNodes ids (rootContext -> root) = go empty [root] where
    go parentName ctxs = "subgraph" <+> dquotes parentName <+> lbrace <> line <>
        indent 4 ("rank = same;" <> line <> vsep (fmap featureNode ctxs)) <>
        line <> rbrace <> line <>
        vsep (fmap (\ctx -> go (pretty ctx) (childContexts ctx)) ctxs)
    featureNode ctx = dquotes (ids ! thisFeature ctx) <> semi
