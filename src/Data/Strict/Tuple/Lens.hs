{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Strict.Tuple.Lens where

import Control.Lens.Tuple
import Data.Strict.Tuple

instance Field1 (Pair a b) (Pair a' b) a a' where
    _1 k (x :!: y) = fmap (:!: y) (k x)

instance Field2 (Pair a b) (Pair a b') b b' where
    _2 k (x :!: y) = fmap (x :!:) (k y)

