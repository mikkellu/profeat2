module Translator.Seeding.Common
  ( incSeedVar
  , configurations
  ) where

import Control.Lens

import Data.Array
import Data.List  ( genericLength, genericTake )

import Syntax
import Translator.Names

incSeedVar :: Integer -> LAssign
incSeedVar i = Assign seedVarName (intExpr $ i + 1) noLoc

configurations :: (a -> Bool)
               -> (Integer, Integer)
               -> [Array Integer a]
               -> [[a]]
configurations isOptional (lower, upper) as = filter valid . subsequences $ as
  where
    opt = genericLength . filter isOptional $ as^..traverse.traverse
    valid xs = let cnt  = genericLength xs
                   mand = genericLength . filter (not . isOptional) $ xs
               in lower - opt <= mand && cnt <= upper

subsequences :: [Array Integer a] -> [[a]]
subsequences (a:as) = let (lower, upper) = bounds a in do
    ss <- subsequences as
    i  <- enumFromTo lower (upper + 1)
    return $ genericTake i (elems a) ++ ss
subsequences [] = return []

