{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Mtbdd.Builder
  ( Ref
  , deref
  , returnDeref

  , BuilderT
  , runBuilderT

  , Builder
  , runBuilder

  , constant
  , projection
  , bindMap
  , map
  , bindApply
  , apply
  ) where

import Prelude hiding (map)

import Data.Hashable

import Data.Mtbdd
import Data.Mtbdd.Builder.Internal


instance (Monad m, Eq t, Hashable t, Num t) =>
         Num (BuilderT t s m (Ref t s)) where
    x + y       = bindApply (+) x y
    x - y       = bindApply (-) x y
    x * y       = bindApply (*) x y
    abs         = bindMap abs
    signum      = bindMap signum
    fromInteger = constant . fromInteger
    negate      = bindMap negate


deref :: Ref t s -> Mtbdd t
deref (Ref x) = x

returnDeref :: Monad m => Ref t s -> BuilderT t s m (Mtbdd t)
returnDeref = return . deref


constant :: (Eq t, Hashable t, Monad m) => t -> BuilderT t s m (Ref t s)
constant v = Ref <$> findOrAddTerminal v


projection
    :: (Eq t, Hashable t, Monad m) => Var -> t -> t -> BuilderT t s m (Ref t s)
projection var one zero = do
    one'  <- findOrAddTerminal one
    zero' <- findOrAddTerminal zero
    Ref <$> findOrAddNode var one' zero'


bindMap
    :: (Eq t, Hashable t, Monad m)
    => (t -> t) -> BuilderT t s m (Ref t s) -> BuilderT t s m (Ref t s)
bindMap f = (map f =<<)

map
    :: (Eq t, Hashable t, Monad m)
    => (t -> t) -> Ref t s -> BuilderT t s m (Ref t s)
map f (Ref m) = Ref <$> map' f m

map'
    :: (Eq t, Hashable t, Monad m)
    => (t -> t) -> Mtbdd t -> BuilderT t s m (Mtbdd t)
map' f = go where
    go (Mtbdd _ node) = case node of
        Terminal v        -> findOrAddTerminal (f v)
        Node var one zero -> do
            zero' <- go zero
            one'  <- go one

            if zero' == one'
                then return one'
                else findOrAddNode var one' zero'


bindApply
    :: (Eq t, Hashable t, Monad m)
    => (t -> t -> t)
    -> BuilderT t s m (Ref t s)
    -> BuilderT t s m (Ref t s)
    -> BuilderT t s m (Ref t s)
bindApply op = bindAp2 (apply op)

apply
    :: (Eq t, Hashable t, Monad m)
    => (t -> t -> t) -> Ref t s -> Ref t s -> BuilderT t s m (Ref t s)
apply op (Ref l) (Ref r) = Ref <$> apply' op l r

apply'
    :: (Eq t, Hashable t, Monad m)
    => (t -> t -> t) -> Mtbdd t -> Mtbdd t -> BuilderT t s m (Mtbdd t)
apply' op = go where
    go l r = case (l, r) of
        (Mtbdd _ (Terminal vl), Mtbdd _ (Terminal vr)) ->
            findOrAddTerminal (vl `op` vr)
        _ -> do
            let var = min (variable l) (variable r)

            zero <- go (child l var False) (child r var False)
            one  <- go (child l var True)  (child r var True)

            if zero == one
                then return one
                else findOrAddNode var one zero

    child this@(Mtbdd _ node) var b = case node of
        Terminal _ -> this
        Node nodeVar one zero
          | var < nodeVar -> this
          | otherwise     -> if b then one else zero


bindAp2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindAp2 f mx my = do
    x <- mx
    y <- my
    f x y
