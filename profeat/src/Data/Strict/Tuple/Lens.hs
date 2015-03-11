{-# LANGUAGE TypeOperators #-}

module Data.Strict.Tuple.Lens
  ( _1'
  , _2'
  ) where

import Data.Strict.Tuple

_1' :: Functor f => (a -> f a') -> (a :!: b) -> f (a' :!: b)
_1' f (x :!: y) = fmap (:!: y) (f x)

_2' :: Functor f => (b -> f b') -> (a :!: b) -> f (a :!: b')
_2' f (x :!: y) = fmap (x :!:) (f y)

