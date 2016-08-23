{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.VarOrderSpec (main, spec) where


import Test.Hspec
import Test.QuickCheck

import Data.VarOrder


spec :: Spec
spec =
    describe "swapVars" $
        it "is consistent" $
            property $ \vo lvls ->
                all (\lvl -> lookupLevel vo (lookupVar vo lvl) == lvl)
                    (lvls :: [Level])


instance Arbitrary VarOrder where
    arbitrary = do
        swaps <- arbitrary :: Gen [NonNegative Int]
        let swapLvls = fmap (Level . getNonNegative) swaps
        return (foldr swapVars initialOrder swapLvls)

deriving instance Arbitrary Level


main :: IO ()
main = hspec spec
