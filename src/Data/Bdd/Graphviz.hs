{-# LANGUAGE OverloadedStrings #-}

module Data.Bdd.Graphviz
  ( renderSobdd
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import Data.Bdd
import Data.Bdd.Internal

renderSobdd :: Text -> Sobdd -> Doc
renderSobdd name (Sobdd ns) = "digraph" <+> text name <+> lbrace <$>
    indent 4 (nodes ns) <$>
    rbrace

nodes :: [Bdd] -> Doc
nodes = vsep . map node

node :: Bdd -> Doc
node n = case view n of
    Terminal _   -> terminal nid
    Decision (Variable v) t e ->
        nid <+> brackets ("label=" <> dquotes (int v)) <$>
        nid <+> edge <+> int (nodeId t) <> semi <$>
        nid <+> edge <+> int (nodeId e) <+> brackets "style=dashed" <> semi
  where
    nid = int (nodeId n)

terminal :: Doc -> Doc
terminal nid = nid <+> brackets "shape=box" <> semi

edge :: Doc
edge = "->"

