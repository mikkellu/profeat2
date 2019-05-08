{-# LANGUAGE OverloadedStrings #-}


module Result.Time
  ( timeToCsv
  ) where

import Control.Lens

import Data.List (transpose)
import qualified Data.Map as Map
import Data.Text.Lazy (fromStrict, pack)

import Numeric

import Text.PrettyPrint.Leijen.Text

import Result


timeToCsv :: ResultCollection -> Doc
timeToCsv rc = vsep (fmap prettyRow rows)
  where
    prettyRow = hsep . punctuate comma
    rows = transpose (addInstanceRow (timeRows rc))


addInstanceRow :: [[Doc]] -> [[Doc]]
addInstanceRow rows =
    (text "instance" : fmap pretty [0 .. numInstances - 1]) : rows
  where
    numInstances = maximum (fmap length rows) - 1


timeRows :: ResultCollection -> [[Doc]]
timeRows = fmap convert . Map.assocs . view rcTime
  where
    convert (name, times) = text (fromStrict name) : fmap time times


time :: Double -> Doc
time = text . pack . ($ "") . showFFloat Nothing
