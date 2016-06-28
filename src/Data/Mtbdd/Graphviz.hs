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

  , renderMtbddsToFile
  , renderMtbddToFile

  , renderMtbdds
  , renderMtbdd

  , prettyMtbdds
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


type NodePred t = Mtbdd t -> Bool

type EdgePred t = Mtbdd t -> Bool


type TerminalLabeling t = Id -> t -> Doc

defaultTerminalLabeling :: Pretty t => TerminalLabeling t
defaultTerminalLabeling _ = pretty

type NodeLabeling t = Id -> Var -> Mtbdd t -> Mtbdd t -> Doc

defaultNodeLabeling :: NodeLabeling t
defaultNodeLabeling _ (Var var) _ _ = int var


renderMtbddsToFile
    :: (Eq t, Pretty t) => RenderOpts t -> Text -> FilePath -> [Mtbdd t] -> IO ()
renderMtbddsToFile opts name fileName ms =
    LIO.writeFile fileName (renderMtbdds opts name ms)

renderMtbddToFile
    :: (Eq t, Pretty t) => RenderOpts t -> Text -> FilePath -> Mtbdd t -> IO ()
renderMtbddToFile opts name fileName m =
    renderMtbddsToFile opts name fileName [m]


renderMtbdds :: (Eq t, Pretty t) => RenderOpts t -> Text -> [Mtbdd t] -> Text
renderMtbdds opts name = displayT . renderPretty 0.4 80 . prettyMtbdds opts name

renderMtbdd :: (Eq t, Pretty t) => RenderOpts t -> Text -> Mtbdd t -> Text
renderMtbdd opts name m = renderMtbdds opts name [m]


prettyMtbdds :: Eq t => RenderOpts t -> Text -> [Mtbdd t] -> Doc
prettyMtbdds opts name ms = "digraph" <+> text name <+> lbrace <> line <>
    indent 4 body <> line <> rbrace
  where
    body = vsep [mtbddNodes opts lvls, mtbddEdges opts lvls]
    lvls = fmap (fmap (filter (nodePred opts))) (levels ms)


prettyMtbdd :: (Eq t, Pretty t) => RenderOpts t -> Text -> Mtbdd t -> Doc
prettyMtbdd opts name m = prettyMtbdds opts name [m]


mtbddNodes :: RenderOpts t -> [(Var, [Mtbdd t])] -> Doc
mtbddNodes opts = vsep . fmap subgraph where
    subgraph (Var var, ms) = "subgraph" <+> name <+> lbrace <> line <>
        indent 4 body <> line <> rbrace
      where
        name | var < maxBound = int var
             | otherwise      = "terminals"
        body = (if compact opts then empty else "rank=same" <> semi <> line) <>
               vsep (fmap (mtbddNode opts) ms) <> line


mtbddNode :: RenderOpts t -> Mtbdd t -> Doc
mtbddNode opts (Mtbdd nid n) = case n of
    Terminal v -> int nid <+>
        brackets ("style=filled, shape=box" <> comma <>
        "label=" <> dquotes (terminalLabeling opts nid v)) <> semi
    Node var one zero -> int nid <+>
        brackets ("label=" <> dquotes (nodeLabeling opts nid var one zero)) <>
        semi


mtbddEdges :: RenderOpts t -> [(Var, [Mtbdd t])] -> Doc
mtbddEdges opts = vsep . fmap edges . concatMap snd where
    edges (Mtbdd nid n) = case n of
        Terminal _      -> empty
        Node _ one zero -> vsep
          [ if p one then i <+> "->" <+> int (nodeId one) <> semi else empty
          , if p zero
                then i <+> "->" <+> int (nodeId zero) <+>
                     brackets "style=dashed" <> semi
                else empty
          ]
      where
        i = int nid
        p = liftA2 (&&) (edgePred opts) (nodePred opts)


levels :: Eq t => [Mtbdd t] -> [(Var, [Mtbdd t])]
levels = sortOn fst . toLists . foldr insert Map.empty . concatMap allNodes
  where
    insert m = Map.insertWith Set.union (variable m) (Set.singleton m)
    toLists = Map.toList . Map.map Set.toList
