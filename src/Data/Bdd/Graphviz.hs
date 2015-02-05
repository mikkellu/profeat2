{-# LANGUAGE OverloadedStrings #-}

-- | Functions for rendering 'Bdd's and 'Sobdd's to Graphviz dot format.
module Data.Bdd.Graphviz
  ( renderSobdd
  , renderBdd
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import Data.Bdd
import Data.Bdd.Internal

-- | Render all nodes of an 'Sobdd'.
renderSobdd :: Text -> Sobdd -> Doc
renderSobdd name (Sobdd ns) = bdd name ns

-- | Render all nodes of a 'Bdd'.
renderBdd :: Text -> Bdd -> Doc
renderBdd name n = bdd name (allNodes n)

bdd :: Text -> [Bdd] -> Doc
bdd name ns = "digraph" <+> text name <+> lbrace <$>
    indent 4 (nodes ns) <$> rbrace

nodes :: [Bdd] -> Doc
nodes = vsep . map node

node :: Bdd -> Doc
node n = case viewNode n of
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

