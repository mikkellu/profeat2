{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Result.Mtbdd
  ( MtbddOpts(..)
  , ReduceOpts(..)
  , ReorderOpts(..)
  , writeMtbdd
  ) where

import Control.Applicative

import Data.Bits ((.&.))
import Data.Foldable (foldl')
import Data.Mtbdd hiding ( VarOrder )
import Data.Mtbdd.Builder
import Data.Mtbdd.Builder.Internal
import Data.Mtbdd.Graphviz
import Data.Mtbdd.Reorder
import Data.Sequence ( Seq )
import Data.Strict.Tuple ( (:!:), Pair(..) )
import Data.Text.Lazy ( pack )
import Data.Vector.Generic ( Vector, toList )

import Text.PrettyPrint.Leijen.Text ( Doc, int )

import System.FilePath ( dropExtension )

import Result
import VarOrder ( VarOrder(..), Range(..) )


-- Rendering -------------------------------------------------------------------

data MtbddOpts = MtbddOpts
  { reduceOpts  :: ReduceOpts
  , reorderOpts :: ReorderOpts
  , mappingFunc :: Maybe Double -> Maybe Double
  }


writeMtbdd
    :: MtbddOpts
    -> VarOrder
    -> FilePath
    -> Seq StateResult
    -> IO ()
writeMtbdd opts vo name stateResults =
    renderMtbddToFile (featureRenderOpts vo)
                      (pack (dropExtension name))
                      name
                      mtbdd
  where
    mtbdd = reorder (toMtbdd (reduceOpts opts) (mappingFunc opts) vo stateVecs)
    stateVecs = fmap (\(StateResult _ sv r) -> sv :!: r) stateResults
    reorder = case reorderOpts opts of
        Reorder      -> sift
        NoReordering -> id


featureRenderOpts :: VarOrder -> RenderOpts (Maybe Double)
featureRenderOpts vo =
    defaultOpts { nodeLabeling = label (variableLabels vo) }
  where
    label labels _ (Var var) _ _ = labels !! var


variableLabels :: VarOrder -> [Doc]
variableLabels (VarOrder vo) = concatMap labels vo where
    labels (name, range) = case range of
        Range _ upper -> attributeLabels name (numberOfBits upper)
        _             -> [name]

    attributeLabels _    0       = []
    attributeLabels name numBits = label : attributeLabels name (numBits - 1)
      where
        label = name <> "+" <> int (2 ^ (numBits - 1))


numberOfBits :: Int -> Int
numberOfBits upper = ceiling (logBase 2 (fromIntegral (upper + 1) :: Double))



-- Conversion ------------------------------------------------------------------


data ReorderOpts = Reorder | NoReordering

data ReduceOpts = FullMtbdd | ReducedMtbdd

type ResultRef s = Builder (Maybe Double) s (Ref (Maybe Double) s)


toMtbdd
    :: Vector v Int
    => ReduceOpts
    -> (Maybe Double -> Maybe Double)
    -> VarOrder
    -> Seq (v Int :!: Result)
    -> Mtbdd (Maybe Double)
toMtbdd opts mf vo (fmap coerceStateVec -> svs) = runBuilder $ do
    m <- combine (fmap (convertStateVec vo) svs)
    result <- f m
    result' <- Data.Mtbdd.Builder.map mf result
    deref result'
  where
    f = case opts of
        FullMtbdd    -> return
        ReducedMtbdd -> reduce


coerceStateVec :: Vector v Int => v Int :!: Result -> v Int :!: Double
coerceStateVec (sv :!: r) = sv :!: toDouble r where
    toDouble = \case
        ResultBool True  -> 1
        ResultBool False -> 0
        ResultDouble x   -> x
        _                -> error "Result.Diagram.coerceStateVec: illegal ResultRange"


convertStateVec :: Vector v Int => VarOrder -> (v Int :!: Double) -> ResultRef s
convertStateVec (VarOrder vo) (sv :!: r) =
    fst (foldl' toNode (constant (Just r), 0) (zip (toList sv) (fmap snd vo)))
  where
    toNode (node, idx) (v, range) =
        let node' = case range of
                        Range _ upper -> attr (numberOfBits upper) v idx
                        _ | v == 0    -> mkNotVar idx
                          | otherwise -> mkVar idx
            idxDelta = case range of
                           Range _ upper -> numberOfBits upper
                           _             -> 1
        in (bindApply mul node node', idx + idxDelta)


-- | Removes the 'Nothing' terminal node and all edges pointing to it.
-- Additionally, all inner nodes with only one outgoing edge are removed
-- (recursively).
reduce
    :: (Eq t, Monad m)
    => Ref (Maybe t) s -> BuilderT (Maybe t) s m (Ref (Maybe t) s)
reduce (Ref m) = Ref <$> reduce' m

reduce'
    :: (Eq t, Monad m)
    => Node (Maybe t) -> BuilderT (Maybe t) s m (Node (Maybe t))
reduce' node@(Node _ ty) = case ty of
    Terminal _ -> return node
    Decision lvl one zero -> case (one, zero) of
        (Node _ (Terminal Nothing), _) -> reduce' zero
        (_, Node _ (Terminal Nothing)) -> reduce' one
        _ -> do
            zero' <- reduce' zero
            one'  <- reduce' one

            if zero' == one'
                then return one'
                else findOrAddNode lvl one' zero'


attr :: Int -> Int -> Int -> ResultRef s
attr numBits v idx
  | numBits == 0 = constant (Just 1.0)
  | numBits == 1 = var
  | otherwise    = bindApply mul var (attr (numBits - 1) v (idx + 1))
  where
    bit = 2 ^ (numBits - 1)
    var | v .&. bit /= 0 = mkVar idx
        | otherwise      = mkNotVar idx


mkVar :: Int -> ResultRef s
mkVar idx = projection (Var idx) (Just 1) Nothing

mkNotVar :: Int -> ResultRef s
mkNotVar idx = projection (Var idx) Nothing (Just 1)


combine :: Foldable f => f (ResultRef s) -> ResultRef s
combine = foldr (bindApply (<|>)) (constant Nothing)

mul :: Maybe Double -> Maybe Double -> Maybe Double
mul = liftA2 (*)
