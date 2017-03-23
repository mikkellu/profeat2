{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module FeatureDiagram
  ( writeFeatureDiagram
  ) where

import Control.Lens

import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy.IO as LIO

import Text.PrettyPrint.Leijen.Text

import Symbols
import Syntax
import Syntax.Util


type FeatureIds = Map FeatureSymbol Doc


writeFeatureDiagram :: FilePath -> FeatureSymbol -> IO ()
writeFeatureDiagram fileName root =
    LIO.writeFile fileName (renderFeatureDiagram root)

renderFeatureDiagram :: FeatureSymbol -> Text
renderFeatureDiagram = displayT . renderPretty 0.4 80 . featureDiagram

featureDiagram :: FeatureSymbol -> Doc
featureDiagram root =
    "digraph" <+> dquotes "feature-diagram" <+> lbrace <> line <>
    indent 4 body <> line <> rbrace
  where
    ids = featureIds root
    body =
        attributes <> line <>
        featureNodes ids root <>
        constraintNode root <> line <>
        decompositionEdges ids root
    attributes =
        "node [shape = plaintext];" <> line <>
        "splines = line;"

featureIds :: FeatureSymbol -> FeatureIds
featureIds root = Map.fromList . fmap mkId . allContexts $ root where
    mkId ctx = (thisFeature ctx, pretty (minimalPrefix root ctx))

featureNodes :: FeatureIds -> FeatureSymbol -> Doc
featureNodes ids (rootContext -> root) = go empty [root] where
    go _          []   = empty
    go parentName ctxs = "subgraph" <+> dquotes parentName <+> lbrace <> line <>
        indent 4 ("rank = same;" <> line <> vsep (fmap featureNode ctxs)) <>
        line <> rbrace <> line <>
        vsep (fmap (\ctx -> go (pretty ctx) (childContexts ctx)) ctxs)
    featureNode ctx = dquotes (getId ctx ids) <+>
        brackets ("label =" <+> label ctx) <> semi
    label ctx =
        "<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
        "<tr><td cellpadding=\"3\">" <> getId ctx ids <> "</td></tr>" <>
        groupCard ctx <>
        "</table>>"
    groupCard ctx =
        let (lower, upper) = ctx^.this.fsGroupCard
        in if lower == 0 && upper == 0
               then empty
               else "<tr><td border=\"0\">" <>
                    brackets (integer lower <> ".." <> integer upper) <>
                    "</td></tr>"

decompositionEdges :: FeatureIds -> FeatureSymbol -> Doc
decompositionEdges ids = vsep . fmap mkEdges . allContexts
  where
    mkEdges ctx = vsep (fmap (mkEdge ctx) (childContexts ctx))
    mkEdge parent child =
        dquotes (getId parent ids) <> colon <> "s" <+> "->" <+>
        dquotes (getId child ids) <> colon <> "n" <+>
        brackets ("arrowhead =" <+> arrowhead) <> semi
      where
        arrowhead | child^.this.fsOptional = "odot"
                  | otherwise              = "none"

constraintNode :: FeatureSymbol -> Doc
constraintNode root = dquotes "constraints" <+>
    brackets ("label =" <+> dquotes rows) <> semi
  where
    rows = hsep . punctuate "\\n" . concatMap constraints . allContexts $ root
    constraints = fmap (pretty . removeActive) . toListOf
        (this.fsConstraints.traverse.to constrExpr.from conjunction.traverse)

removeActive :: Expr a -> Expr a
removeActive = transform $ \case
    CallExpr (FuncExpr FuncActive _) [e] _ -> e
    e -> e

getId :: FeatureContext -> FeatureIds -> Doc
getId ctx ids = ids ! thisFeature ctx
