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

import Control.Monad.State

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LIO

import Text.PrettyPrint.Leijen.Text

import Data.Mtbdd


data RenderOpts t = RenderOpts
  { nodePred         :: NodePred t
  , edgePred         :: EdgePred t
  , terminalLabeling :: TerminalLabeling t
  , nodeLabeling     :: NodeLabeling t
  }

defaultOpts :: Pretty t => RenderOpts t
defaultOpts = RenderOpts
  { nodePred         = const True
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


prettyMtbdds :: (Eq t, Pretty t) => RenderOpts t -> Text -> [Mtbdd t] -> Doc
prettyMtbdds opts name ms = "digraph" <+> text name <+> lbrace <> line <>
    indent 4 (mtbdds opts (concatMap allNodes ms)) <> line <> rbrace

prettyMtbdd :: (Eq t, Pretty t) => RenderOpts t -> Text -> Mtbdd t -> Doc
prettyMtbdd opts name m = prettyMtbdds opts name [m]


mtbdds :: Pretty t => RenderOpts t -> [Mtbdd t] -> Doc
mtbdds opts ms = vsep (evalState (traverse (mtbdd opts) ms) Set.empty)


mtbdd :: Pretty t => RenderOpts t -> Mtbdd t -> State (HashSet Id) Doc
mtbdd opts m@(Mtbdd nid n)
  | nodePred opts m = once nid $ case n of
        Terminal v        -> terminal opts v
        Node var one zero -> node opts var one zero
  | otherwise = return empty


node :: RenderOpts t -> Var -> Mtbdd t -> Mtbdd t -> Id -> Doc
node opts var one zero nid = vsep
    [ i <+> brackets ("label=" <> dquotes label) <> semi
    , if p one then i <+> edge <+> int (nodeId one) <> semi else empty
    , if p zero
          then i <+> edge <+> int (nodeId zero) <+>
               brackets "style=dashed" <> semi
          else empty
    ]
  where
    i = int nid
    p = liftA2 (&&) (edgePred opts) (nodePred opts)
    label = nodeLabeling opts nid var one zero


terminal :: RenderOpts t -> t -> Id -> Doc
terminal opts v nid = int nid <+>
    brackets ("shape=box" <> comma <> "label=" <> dquotes label) <> semi
  where
    label = terminalLabeling opts nid v


edge :: Doc
edge = "->"


once :: Id -> (Id -> Doc) -> State (HashSet Id) Doc
once nid f = do
    nids <- get
    if Set.member nid nids
    then return empty
    else do
        modify (Set.insert nid)
        return (f nid)
