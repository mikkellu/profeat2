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
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Vector.Generic ( Vector )
import qualified Data.Vector.Generic as V

import Data.Text.Lazy ( Text, replace )
import Text.PrettyPrint.Leijen.Text

import Result
import Symbols
import Syntax
import VarOrder


toCsv :: FeatureSymbol -> VarMap -> Property a -> ResultCollection -> Doc
toCsv root vm prop ResultCollection{..} =
    header root innerFeatures prop <> line <>
    stateResults root innerFeatures (toVarOrder vm _rcVariables) _rcStateResults
  where
    innerFeatures =
        filter (not . isLeafFeature . thisFeature) (allContexts root)

header :: FeatureSymbol -> [FeatureContext] -> Property a -> Doc
header root ctxs prop =
    "id" <> comma <> "configuration" <> comma <> dquotes propName <> comma <>
    hcat (punctuate comma (fmap featureName ctxs))
  where
    propName = case propIdent prop of
        Just name -> text name
        Nothing   -> text (quoteProp prop)
    featureName ctx = pretty (minimalPrefix root ctx)

stateResults
    :: FeatureSymbol -> [FeatureContext] -> VarOrder -> Seq StateResult -> Doc
stateResults root innerFeatures vo =
    vsep . fmap (stateResult root innerFeatures vo) . toList

stateResult
    :: FeatureSymbol -> [FeatureContext] -> VarOrder -> StateResult -> Doc
stateResult root innerFeatures vo (StateResult i sv r) =
    int i <> comma <> stateVec vo sv <> comma <> pretty r <> comma <>
    hcat (punctuate comma (featureSets root innerFeatures vo sv))

stateVec :: Vector v Int => VarOrder -> v Int -> Doc
stateVec (VarOrder vo) = hsep . mapMaybe (uncurry prettyVal) . zip vo . V.toList

featureSets
    :: Vector v Int
    => FeatureSymbol
    -> [FeatureContext]
    -> VarOrder
    -> v Int
    -> [Doc]
featureSets root innerFeatures vo sv =
    fmap (hsep . fmap (pretty . minimalPrefix root) . Set.toList . featureSet)
    innerFeatures
  where
    featureSet ctx =
        Set.fromList (childContexts ctx) `Set.intersection` selected
    selected = selectedFeatures vo sv


selectedFeatures :: Vector v Int => VarOrder -> v Int -> Set FeatureContext
selectedFeatures (VarOrder vo) = Set.fromList . mapMaybe f . zip vo . V.toList
  where
    f ((_, r), v) = case r of
        RangeFeature ctx | v == 1 -> Just ctx
        _ -> Nothing


quoteProp :: Property a -> Text
quoteProp = replace "\"" "\"\"" . displayT . renderPretty 1.0 300 . pretty
