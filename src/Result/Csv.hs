{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Result.Csv
  ( toCsv
  ) where

import Data.Foldable ( toList )
import Data.Maybe ( mapMaybe)
import Data.Sequence ( Seq )
import Data.Vector.Generic ( Vector )
import qualified Data.Vector.Generic as V

import Data.Text.Lazy ( Text, replace )
import Text.PrettyPrint.Leijen.Text

import Result
import Syntax
import VarOrder

toCsv :: VarMap -> Property a -> ResultCollection -> Doc
toCsv vm prop ResultCollection{..} =
    header prop <> line <>
    stateResults (toVarOrder vm _rcVariables) _rcStateResults

header :: Property a -> Doc
header prop = "id" <> comma <> "configuration" <> comma <> dquotes propName
  where
    propName = case propIdent prop of
        Just name -> text name
        Nothing   -> text (quoteProp prop)

stateResults :: VarOrder -> Seq StateResult -> Doc
stateResults vo = vsep . fmap (stateResult vo) . toList

stateResult :: VarOrder -> StateResult -> Doc
stateResult vo (StateResult i sv r) =
    int i <> comma <> stateVec vo sv <> comma <> pretty r

stateVec :: Vector v Int => VarOrder -> v Int -> Doc
stateVec (VarOrder vo) = hsep . mapMaybe (uncurry prettyVal) . zip vo . V.toList


quoteProp :: Property a -> Text
quoteProp = replace "\"" "\"\"" . displayT . renderPretty 1.0 300 . pretty
