{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Result.Constraint
  ( constraintsFor
  ) where

import Control.Lens
import Control.Monad.State

import Data.Bits
import Data.Foldable
import Data.Set ( (\\) )
import qualified Data.Set as Set
import Data.Strict.Tuple ( (:!:) )
import qualified Data.Vector.Generic as V

import Qm

import Text.PrettyPrint.Leijen.Text

import Result
import VarOrder

constraintsFor :: VarOrder -> ResultCollection -> (Result -> Bool) -> Doc
constraintsFor vo rc p =
    toConstraints vo . qm' . toBitVectors vo .
    filter (p . view _2) $ rc^..rcStateResults.traverse
  where
    qm' ones  = qm ones [] dontCares
    dontCares = invalidFeatureCombinations vo rc

toConstraints :: VarOrder -> [QmTerm] -> Doc
toConstraints vo =
    hsep . punctuate " |" .
    fmap (parens . hsep . punctuate " &"  . toConstraint vo)

toConstraint :: VarOrder -> QmTerm -> [Doc]
toConstraint (VarOrder vo) (QmTerm (vars, mask)) =
    fst . flip execState ([], 0) $ for_ vo $ \(name, r) -> case r of
        RangeFeature -> do
            i <- use _2
            unless (mask `testBit` i) $ -- it is not a "don't care" variable
                _1 %= (++ [literal name (vars `testBit` i)])
            _2 += 1
        _ -> return ()
  where
    literal name active
        | active    = name
        | otherwise = "!" <> name

toBitVectors :: VarOrder -> [StateVec :!: Result] -> [BitVector]
toBitVectors vo = fmap (toBitVector vo . view _1)

toBitVector :: VarOrder -> StateVec -> BitVector
toBitVector (VarOrder vo) sv =
    fst . flip execState (zeroBits, 0) $ for_ vars $ \((_, r), v) -> case r of
        RangeFeature -> do
            when (v == 1) $ do
                i <- use _2
                _1 %= (`setBit` i)
            _2 += 1
        _ -> return ()
  where
    vars = zip vo (V.toList sv)

invalidFeatureCombinations :: VarOrder -> ResultCollection -> [BitVector]
invalidFeatureCombinations vo rc =
    Set.toList
        (Set.fromList (allFeatureCombinations vo) \\
         Set.fromList (validFeatureCombinations vo rc))

validFeatureCombinations :: VarOrder -> ResultCollection -> [BitVector]
validFeatureCombinations vo rc =
    toBitVectors vo $ rc^..rcStateResults.traverse

allFeatureCombinations :: VarOrder -> [BitVector]
allFeatureCombinations (VarOrder vo) = [0 .. 2 ^ numBits - 1]
  where
    numBits = length (filter isFeature vo)
    isFeature (_, r) = case r of
        RangeFeature -> True
        _            -> False
