module Data.Mtbdd.BuilderSpec (main, spec) where

import Test.Hspec

import Control.Monad

import Data.Mtbdd
import Data.Mtbdd.Builder

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "projection" $
        it "evaluates to the given values" $ do
            let f = runBuilder $ do
                        x <- projection (Var 0) "True" "False"
                        returnDeref x
            eval f [True] `shouldBe` "True"
            eval f [False] `shouldBe` "False"

    describe "eval" $
        it "works for binary encoding" $ do
            let f = runBuilder $ do
                        result <- binaryEncoding 3
                        returnDeref result
            eval f [False, True, True] `shouldBe` (3 :: Int)
            eval f [True, True, False] `shouldBe` 6

    describe "apply" $ do
        it "works for multiplication" $ do
            let f = runBuilder $ do
                        x <- projection (Var 0) 2 (1 :: Int)
                        y <- constant 5
                        result <- apply (*) x y
                        returnDeref result
            eval f [True]  `shouldBe` 10
            eval f [False] `shouldBe` 5

        it "works for subtraction" $ do
            let f = runBuilder $ do
                        x <- projection (Var 0) 3 (1 :: Int)
                        y <- constant 1
                        result <- apply (-) x y
                        returnDeref result
            eval f [True]  `shouldBe` 2
            eval f [False] `shouldBe` 0

binaryEncoding :: Monad m => Int -> BuilderT Int s m (Ref Int s)
binaryEncoding numBits = do
    c <- constant 0
    foldM powAdd c [0 .. numBits - 1]
  where
    powAdd x i = do
        y <- projection (Var (numBits - 1 - i)) (2 ^ i) 0
        apply (+) x y

