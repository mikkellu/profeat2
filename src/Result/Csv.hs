{-# LANGUAGE FlexibleContexts #-}
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

import Text.PrettyPrint.Leijen.Text

import Result
import VarOrder

toCsv :: VarMap -> ResultCollection -> Doc
toCsv vm ResultCollection{..} =
    stateResults (toVarOrder vm _rcVariables) _rcStateResults

stateResults :: VarOrder -> Seq StateResult -> Doc
stateResults vo = vsep . fmap (stateResult vo) . toList

stateResult :: VarOrder -> StateResult -> Doc
stateResult vo (StateResult i sv r) =
    int i <> char ',' <+> stateVec vo sv <> char ',' <+> pretty r

stateVec :: Vector v Int => VarOrder -> v Int -> Doc
stateVec (VarOrder vo) = hsep . mapMaybe (uncurry prettyVal) . zip vo . V.toList
