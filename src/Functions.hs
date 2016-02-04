{-# LANGUAGE ViewPatterns #-}

module Functions
  ( binomialDist
  ) where

binomialDist :: Fractional a => a -> Integer -> [a]
binomialDist p n = fmap binom [0..n]
  where
    binom k = fromInteger (choose n k) * p ^ k * (1.0 - p) ^ (n - k)

choose :: Integral a => a -> a -> a
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

