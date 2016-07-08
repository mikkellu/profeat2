{-# LANGUAGE ViewPatterns #-}

module Data.Mtbdd.ReorderSpec (main, spec) where


import Control.Monad ((<=<))

import Data.Hashable

import Test.Hspec
import Test.QuickCheck

import Data.Mtbdd
import Data.Mtbdd.Builder
import Data.Mtbdd.Builder.Internal (Ref(..))
import Data.Mtbdd.Reorder

import Data.Proposition


spec :: Spec
spec = do
    describe "swap" $ do
        it "preserves the represented function" $ property $ withBoundedLevel $
            \f lvl -> sat id f == sat id (swap' lvl f)
        it "returns the original Mtbdd if applied twice" $ property $
            withBoundedLevel $ \f lvl ->
                let f' = runBuilderWith f $ \(Ref node) -> do
                             result <- Ref <$> (swap lvl <=< swap lvl) node
                             deref result
                in allNodes (rootNode f) == allNodes (rootNode f')

    describe "sift" $ do
        it "preserves the number of variables" $ property $
            \(toBdd -> f) -> numberOfVars f == numberOfVars (sift f)
        it "returns a BDD of equal or smaller size" $ property $
            \(toBdd -> f) -> size (rootNode f) >= size (rootNode (sift f))


withBoundedLevel
    :: Testable prop
    => (Mtbdd Bool -> Level -> prop)
    -> Proposition -> NonNegative Int
    -> Property
withBoundedLevel prop (toBdd -> f) (getNonNegative -> i) =
    let numVars = numberOfVars f
        i'      = i `mod` max 1 numVars
        lvl     = Level i'
    in i' < numVars - 1 ==> prop f lvl


swap' :: (Eq t, Hashable t) => Level -> Mtbdd t -> Mtbdd t
swap' lvl m = runBuilderWith m $ \(Ref node) -> do
    result <- Ref <$> swap lvl node
    deref result


main :: IO ()
main = hspec spec
