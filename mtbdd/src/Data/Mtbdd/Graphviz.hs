{-# LANGUAGE OverloadedStrings #-}

module Data.Mtbdd.Graphviz
  ( RenderOpts(..)
  , defaultOpts

  , NodePred
  , EdgePred

  , TerminalLabeling
  , defaultTerminalLabeling

  , NodeLabeling
  , defaultNodeLabeling

  , renderMtbddToFile
  , renderMtbdd
  , prettyMtbdd
  ) where


import Control.Applicative (liftA2)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

import Data.List (sortOn)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LIO

import Text.PrettyPrint.Leijen.Text

import Data.Mtbdd


data RenderOpts t = RenderOpts
  { compact          :: !Bool
  , nodePred         :: NodePred t
  , edgePred         :: EdgePred t
  , terminalLabeling :: TerminalLabeling t
  , nodeLabeling     :: NodeLabeling t
  }

defaultOpts :: Pretty t => RenderOpts t
defaultOpts = RenderOpts
  { compact          = False
  , nodePred         = const True
  , edgePred         = const True
  , terminalLabeling = defaultTerminalLabeling
  , nodeLabeling     = defaultNodeLabeling
  }


type NodePred t = Node t -> Bool

type EdgePred t = Node t -> Bool


type TerminalLabeling t = Id -> t -> Doc

defaultTerminalLabeling :: Pretty t => TerminalLabeling t
defaultTerminalLabeling _ = pretty

type NodeLabeling t = Id -> Var -> Node t -> Node t -> Doc

defaultNodeLabeling :: NodeLabeling t
defaultNodeLabeling _ (Var var) _ _ = int var


renderMtbddToFile :: RenderOpts t -> Text -> FilePath -> Mtbdd t -> IO ()
renderMtbddToFile opts name fileName m =
    LIO.writeFile fileName (renderMtbdd opts name m)


renderMtbdd :: RenderOpts t -> Text -> Mtbdd t -> Text
renderMtbdd opts name = displayT . renderPretty 0.4 80 . prettyMtbdd opts name


prettyMtbdd :: RenderOpts t -> Text -> Mtbdd t -> Doc
prettyMtbdd opts name m = "digraph" <+> text name <+> lbrace <> line <>
    indent 4 body <> line <> rbrace
  where
    body = vsep [mtbddNodes opts lvls, mtbddEdges opts lvls]
    lvls = fmap (fmap (filter (nodePred opts))) (levels m)


mtbddNodes :: RenderOpts t -> [(Var, [Node t])] -> Doc
mtbddNodes opts = vsep . fmap subgraph where
    subgraph (Var var, ms) = "subgraph" <+> name <+> lbrace <> line <>
        indent 4 body <> line <> rbrace
      where
        name | var < maxBound = int var
             | otherwise      = "terminals"
        body = (if compact opts then empty else "rank=same" <> semi <> line) <>
               vsep (fmap (mtbddNode opts) ms) <> line


mtbddNode :: RenderOpts t -> Node t -> Doc
mtbddNode opts (Node nid ty) = case ty of
    Terminal v -> int nid <+>
        brackets ("style=filled, shape=box" <> comma <>
        "label=" <> dquotes (terminalLabeling opts nid v)) <> semi
    Decision var one zero ->
        let nodeLabel = nodeLabeling opts nid var one zero
        in int nid <+> brackets ("label=" <> dquotes nodeLabel) <> semi


mtbddEdges :: RenderOpts t -> [(Var, [Node t])] -> Doc
mtbddEdges opts = vsep . fmap edges . concatMap snd where
    edges (Node nid ty) = case ty of
        Terminal _          -> empty
        Decision _ one zero -> vsep
          [ if p one then i <+> "->" <+> int (nodeId one) <> semi else empty
          , if p zero
                then i <+> "->" <+> int (nodeId zero) <+>
                     brackets "style=dashed" <> semi
                else empty
          ]
      where
        i = int nid
        p = liftA2 (&&) (edgePred opts) (nodePred opts)


levels :: Mtbdd t -> [(Var, [Node t])]
levels = sortOn fst . toLists . foldr insert Map.empty . allNodes . rootNode
  where
    insert m = Map.insertWith Set.union (variable m) (Set.singleton m)
    toLists = Map.toList . Map.map Set.toList
