{-# LANGUAGE OverloadedStrings #-}

module Data.Mtbdd.Graphviz
  ( renderMtbddsToFile
  , renderMtbddToFile

  , renderMtbdds
  , renderMtbdd

  , prettyMtbdds
  , prettyMtbdd
  ) where


import Control.Monad.State

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LIO

import Text.PrettyPrint.Leijen.Text

import Data.Mtbdd


renderMtbddsToFile :: (Eq t, Pretty t) => Text -> FilePath -> [Mtbdd t] -> IO ()
renderMtbddsToFile name fileName ms =
    LIO.writeFile fileName (renderMtbdds name ms)

renderMtbddToFile :: (Eq t, Pretty t) => Text -> FilePath -> Mtbdd t -> IO ()
renderMtbddToFile name fileName m = renderMtbddsToFile name fileName [m]


renderMtbdds :: (Eq t, Pretty t) => Text -> [Mtbdd t] -> Text
renderMtbdds name = displayT . renderPretty 0.4 80 . prettyMtbdds name

renderMtbdd :: (Eq t, Pretty t) => Text -> Mtbdd t -> Text
renderMtbdd name m = renderMtbdds name [m]


prettyMtbdds :: (Eq t, Pretty t) => Text -> [Mtbdd t] -> Doc
prettyMtbdds name ms = "digraph" <+> text name <+> lbrace <> line <>
    indent 4 (mtbdds (concatMap allNodes ms)) <> line <> rbrace

prettyMtbdd :: (Eq t, Pretty t) => Text -> Mtbdd t -> Doc
prettyMtbdd name m = prettyMtbdds name [m]


mtbdds :: Pretty t => [Mtbdd t] -> Doc
mtbdds ms = vsep (evalState (traverse mtbdd ms) Set.empty)


mtbdd :: Pretty t => Mtbdd t -> State (HashSet Id) Doc
mtbdd (Mtbdd nid n) = once nid $ case n of
    Terminal v        -> terminal v
    Node var one zero -> node var (nodeId one) (nodeId zero)


node :: Var -> Id -> Id -> Id -> Doc
node (Var var) oneId zeroId nid =
    i <+> brackets ("label=" <> dquotes label) <> line <>
    i <+> edge <+> int oneId <> semi <> line <>
    i <+> edge <+> int zeroId <+> brackets "style=dashed" <> semi
  where
    i = int nid
    label = int var


terminal :: Pretty t => t -> Id -> Doc
terminal v nid = int nid <+>
    brackets ("shape=box" <> comma <> "label=" <> dquotes (pretty v)) <> semi


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
