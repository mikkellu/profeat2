{-# LANGUAGE OverloadedStrings #-}

module Data.Mtbdd.Graphviz
  ( EdgePred
  , allEdges

  , renderMtbddsToFile
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


type EdgePred t = Mtbdd t -> Bool


allEdges :: EdgePred t
allEdges = const True


renderMtbddsToFile
    :: (Eq t, Pretty t) => EdgePred t -> Text -> FilePath -> [Mtbdd t] -> IO ()
renderMtbddsToFile p name fileName ms =
    LIO.writeFile fileName (renderMtbdds p name ms)

renderMtbddToFile
    :: (Eq t, Pretty t) => EdgePred t -> Text -> FilePath -> Mtbdd t -> IO ()
renderMtbddToFile p name fileName m = renderMtbddsToFile p name fileName [m]


renderMtbdds :: (Eq t, Pretty t) => EdgePred t -> Text -> [Mtbdd t] -> Text
renderMtbdds p name = displayT . renderPretty 0.4 80 . prettyMtbdds p name

renderMtbdd :: (Eq t, Pretty t) => EdgePred t -> Text -> Mtbdd t -> Text
renderMtbdd p name m = renderMtbdds p name [m]


prettyMtbdds :: (Eq t, Pretty t) => EdgePred t -> Text -> [Mtbdd t] -> Doc
prettyMtbdds p name ms = "digraph" <+> text name <+> lbrace <> line <>
    indent 4 (mtbdds p (concatMap allNodes ms)) <> line <> rbrace

prettyMtbdd :: (Eq t, Pretty t) => EdgePred t -> Text -> Mtbdd t -> Doc
prettyMtbdd p name m = prettyMtbdds p name [m]


mtbdds :: Pretty t => EdgePred t -> [Mtbdd t] -> Doc
mtbdds p ms = vsep (evalState (traverse (mtbdd p) ms) Set.empty)


mtbdd :: Pretty t => EdgePred t -> Mtbdd t -> State (HashSet Id) Doc
mtbdd p (Mtbdd nid n) = once nid $ case n of
    Terminal v        -> terminal v
    Node var one zero -> node p var one zero


node :: EdgePred t -> Var -> Mtbdd t -> Mtbdd t -> Id -> Doc
node p (Var var) one zero nid = vsep
    [ i <+> brackets ("label=" <> dquotes label) <> semi
    , if p one then i <+> edge <+> int (nodeId one) <> semi else empty
    , if p zero
          then i <+> edge <+> int (nodeId zero) <+>
               brackets "style=dashed" <> semi
          else empty
    ]
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
