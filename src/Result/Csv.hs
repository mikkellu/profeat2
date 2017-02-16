{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Result.Csv
  ( toCsv
  ) where

import Data.Foldable ( toList )
import Data.Maybe ( mapMaybe)
import qualified Data.Strict.Tuple as ST
import Data.Strict.Tuple ( (:!:) )
import Data.Sequence ( Seq )
import Data.Vector.Generic ( Vector )
import qualified Data.Vector.Generic as V

import Text.PrettyPrint.Leijen.Text

import Analysis.VarOrder
import Result

toCsv :: ResultCollection -> Doc
toCsv ResultCollection{..} = stateResults _rcVarOrder _rcStateResults

stateResults :: Vector v Int => VarOrder -> Seq (v Int :!: Result) -> Doc
stateResults vo = vsep . fmap (ST.uncurry $ stateResult vo) . toList

stateResult :: Vector v Int => VarOrder -> v Int -> Result -> Doc
stateResult vo sv r = stateVec vo sv <> char ',' <> pretty r

stateVec :: Vector v Int => VarOrder -> v Int -> Doc
stateVec (VarOrder vo) = hsep . mapMaybe (uncurry prettyVal) . zip vo . V.toList
