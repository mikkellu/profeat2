{-# LANGUAGE OverloadedStrings #-}

module Data.Mtbdd.Graphviz
  ( renderMtbddToFile
  , renderMtbdd
  , prettyMtbdd
  ) where


import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LIO

import Text.PrettyPrint.Leijen.Text

import Data.Mtbdd


renderMtbddToFile :: (Eq t, Pretty t) => Text -> FilePath -> Mtbdd t -> IO ()
renderMtbddToFile name fileName m =
    LIO.writeFile fileName (renderMtbdd name m)


renderMtbdd :: (Eq t, Pretty t) => Text -> Mtbdd t -> Text
renderMtbdd name = displayT . renderPretty 0.4 80 . prettyMtbdd name


prettyMtbdd :: (Eq t, Pretty t) => Text -> Mtbdd t -> Doc
prettyMtbdd name m = "digraph" <+> text name <+> lbrace <> line <>
    indent 4 (mtbdds (allNodes m)) <> line <> rbrace


mtbdds :: Pretty t => [Mtbdd t] -> Doc
mtbdds = vsep . fmap mtbdd


mtbdd :: Pretty t => Mtbdd t -> Doc
mtbdd (Mtbdd nid n) = case n of
    Terminal v        -> terminal nid v
    Node var one zero -> node nid var (nodeId one) (nodeId zero)


node :: Id -> Var -> Id -> Id -> Doc
node nid (Var var) oneId zeroId =
    i <+> brackets ("label=" <> dquotes label) <> line <>
    i <+> edge <+> int oneId <> semi <> line <>
    i <+> edge <+> int zeroId <+> brackets "style=dashed" <> semi
  where
    i = int nid
    label = int var


terminal :: Pretty t => Id -> t -> Doc
terminal nid v = int nid <+>
    brackets ("shape=box" <> comma <> "label=" <> dquotes (pretty v)) <> semi


edge :: Doc
edge = "->"
