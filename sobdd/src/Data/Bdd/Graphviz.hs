{-# LANGUAGE OverloadedStrings #-}

-- | Functions for rendering 'Bdd's and 'Sobdd's to Graphviz dot format.
module Data.Bdd.Graphviz
  ( renderSobdd
  , renderSobddWithNames
  , renderBdd
  , renderBddWithNames
  ) where

import Data.Map                     ( Map )
import qualified Data.Map as Map
import Data.Text.Lazy               ( Text )

import Text.PrettyPrint.Leijen.Text

import Data.Bdd
import Data.Bdd.Internal

-- | Render all nodes of an 'Sobdd'.
renderSobdd :: Text -> Sobdd -> Doc
renderSobdd = renderSobddWithNames (Map.empty :: Map Variable ())

-- | Render all nodes of an 'Sobdd' and label them with their variable
-- name. If a 'Variable' is not contained in the given 'Map', its index is
-- used to label the corresponding nodes.
renderSobddWithNames :: Pretty a => Map Variable a -> Text -> Sobdd -> Doc
renderSobddWithNames names name (Sobdd ns) = bdd names name ns

-- | Render all nodes of a 'Bdd'.
renderBdd :: Text -> Bdd -> Doc
renderBdd = renderBddWithNames (Map.empty :: Map Variable ())

-- | Render all nodes of a 'Bdd' and label them with their variable
-- name. If a 'Variable' is not contained in the given 'Map', its index is
-- used to label the corresponding nodes.
renderBddWithNames :: Pretty a => Map Variable a -> Text -> Bdd -> Doc
renderBddWithNames names name n = bdd names name (allNodes n)

bdd :: Pretty a => Map Variable a -> Text -> [Bdd] -> Doc
bdd names name ns = "digraph" <+> text name <+> lbrace <$>
    indent 4 (nodes names ns) <$> rbrace

nodes :: Pretty a => Map Variable a -> [Bdd] -> Doc
nodes names = vsep . map (node names)

node :: Pretty a => Map Variable a -> Bdd -> Doc
node names n = case viewNode n of
    Terminal _       -> terminal nid
    Decision var t e ->
        let label = case Map.lookup var names of
                        Just x  -> pretty x
                        Nothing -> int (getVariable var)
        in nid <+> brackets ("label=" <> dquotes label) <$>
           nid <+> edge <+> int (nodeId t) <> semi <$>
           nid <+> edge <+> int (nodeId e) <+> brackets "style=dashed" <> semi
  where
    nid = int (nodeId n)

terminal :: Doc -> Doc
terminal nid = nid <+> brackets "shape=box" <> semi

edge :: Doc
edge = "->"

