{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Mtbdd.BuilderSpec (main, spec) where


import Data.Vector (fromList)

import Test.Hspec
import Test.QuickCheck

import Data.Mtbdd as M
import Data.Mtbdd.Builder

import Data.Proposition hiding (eval)
import qualified Data.Proposition as P


spec :: Spec
spec = do
    describe "Mtbdd" $
        it "is equivalent to represented function" $
            property $ \f (fromList -> ds) -> eval (toBdd f) ds == P.eval f ds

    describe "projection" $
        it "evaluates to the given values" $ do
            let f = runBuilder $ do
                        x <- projection (Var 0) "True" "False"
                        deref x
            eval f [True] `shouldBe` "True"
            eval f [False] `shouldBe` "False"

    describe "eval" $
        it "works for binary encoding" $ do
            let f = runBuilder $ do
                        result <- binaryEncoding 3
                        deref result
            eval f [False, True, True] `shouldBe` (3 :: Int)
            eval f [True, True, False] `shouldBe` 6

    describe "map" $ do
        it "works for incrementation" $ do
            let f = runBuilder $ do
                        bin <- binaryEncoding 3
                        result <- Data.Mtbdd.Builder.map (+1) bin
                        deref result
            eval f [False, True, False] `shouldBe` (3 :: Int)
            eval f [True, True, True] `shouldBe` 8

        it "works for signum" $ do
            let f = runBuilder $ do
                        bin <- binaryEncoding 3
                        result <- Data.Mtbdd.Builder.map signum bin
                        deref result
            eval f [False, False, False] `shouldBe` (0 :: Int)
            eval f [True, False, True] `shouldBe` 1

    describe "apply" $ do
        it "works for multiplication" $ do
            let f = runBuilder $ do
                        result <- 5 * projection (Var 0) 2 (1 :: Int)
                        deref result
            eval f [True]  `shouldBe` 10
            eval f [False] `shouldBe` 5

        it "works for subtraction" $ do
            let f = runBuilder $ do
                        result <- projection (Var 0) 3 (1 :: Int) - 1
                        deref result
            eval f [True]  `shouldBe` 2
            eval f [False] `shouldBe` 0

    describe "runBuilderWith" $
        it "preserves structure" $
            property $ \(toBdd -> f) ->
                allNodes f == allNodes (runBuilderWith f deref)


binaryEncoding :: Monad m => Int -> BuilderT Int s m (Ref Int s)
binaryEncoding numBits = sum (fmap ithVar ([0 .. numBits - 1] :: [Int]))
  where
    ithVar i = projection (Var (numBits - 1 - i)) (2 ^ i) 0


main :: IO ()
main = hspec spec
