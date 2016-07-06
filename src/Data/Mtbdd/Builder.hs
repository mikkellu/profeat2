{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Mtbdd.Builder
  ( Ref
  , deref

  , BuilderT
  , runBuilderT
  , runBuilderTWith

  , Builder
  , runBuilder
  , runBuilderWith

  , constant
  , projection
  , bindMap
  , map
  , bindApply
  , apply
  ) where

import Prelude hiding (map)

import Data.Hashable

import Data.Mtbdd.Internal
import Data.Mtbdd.Builder.Internal
import Data.VarOrder


instance (Monad m, Eq t, Hashable t, Num t) =>
         Num (BuilderT t s m (Ref t s)) where
    x + y       = bindApply (+) x y
    x - y       = bindApply (-) x y
    x * y       = bindApply (*) x y
    abs         = bindMap abs
    signum      = bindMap signum
    fromInteger = constant . fromInteger
    negate      = bindMap negate


deref :: Monad m => Ref t s -> BuilderT t s m (Mtbdd t)
deref (Ref node) = Mtbdd <$> getNumberOfVars <*> getVarOrder <*> pure node


constant :: (Eq t, Hashable t, Monad m) => t -> BuilderT t s m (Ref t s)
constant v = Ref <$> findOrAddTerminal v


projection
    :: (Eq t, Hashable t, Monad m) => Var -> t -> t -> BuilderT t s m (Ref t s)
projection var one zero = do
    adjustNumberOfVars var
    vo <- getVarOrder

    one'  <- findOrAddTerminal one
    zero' <- findOrAddTerminal zero

    Ref <$> findOrAddNode (lookupLevel vo var) one' zero'


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
    => (t -> t) -> Node t -> BuilderT t s m (Node t)
map' f = go where
    go (Node _ ty) = case ty of
        Terminal v            -> findOrAddTerminal (f v)
        Decision lvl one zero -> do
            zero' <- go zero
            one'  <- go one

            if zero' == one'
                then return one'
                else findOrAddNode lvl one' zero'


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
    => (t -> t -> t) -> Node t -> Node t -> BuilderT t s m (Node t)
apply' op = go where
    go l r = case (l, r) of
        (Node _ (Terminal vl), Node _ (Terminal vr)) ->
            findOrAddTerminal (vl `op` vr)
        _ -> do
            let lvl = min (level l) (level r)

            zero <- go (child l lvl False) (child r lvl False)
            one  <- go (child l lvl True)  (child r lvl True)

            if zero == one
                then return one
                else findOrAddNode lvl one zero


bindAp2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindAp2 f mx my = do
    x <- mx
    y <- my
    f x y
