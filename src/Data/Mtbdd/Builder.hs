{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Mtbdd.Builder
  ( BuilderT
  , runBuilderT

  , Builder
  , runBuilder

  , Ref
  , deref
  , returnDeref

  , constant
  , projection
  , mapB
  , map
  , applyB
  , apply
  ) where

import Prelude hiding (map)

import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.Mtbdd


type BinOp t = t -> t -> t


type UniqueTable t = HashMap (Var, Id, Id) (Mtbdd t)


type TerminalTable t = HashMap t (Mtbdd t)


data BuilderState t = BuilderState
  { nextId    :: !Id
  , unique    :: UniqueTable t
  , terminals :: TerminalTable t
  }

initialState :: BuilderState t
initialState = BuilderState
  { nextId    = 0
  , unique    = Map.empty
  , terminals = Map.empty
  }

freshNodeId :: MonadState (BuilderState t) m => m Id
freshNodeId = do
    nid <- gets nextId
    modify $ \s -> s { nextId = nid + 1 }
    return nid

createTerminal
    :: (Eq t, Hashable t, MonadState (BuilderState t) m) => t -> m (Mtbdd t)
createTerminal v = do
    nid <- freshNodeId
    let term = Mtbdd nid (Terminal v)
    modify $ \s -> s { terminals = Map.insert v term (terminals s) }
    return term

createNode
    :: MonadState (BuilderState t) m => Var -> Mtbdd t -> Mtbdd t -> m (Mtbdd t)
createNode var one zero = do
    nid <- freshNodeId
    let node = Mtbdd nid (Node var one zero)
    modify $ \s ->
        s { unique = Map.insert (var, nodeId one, nodeId zero) node (unique s) }
    return node


newtype BuilderT t s m a = BuilderT (StateT (BuilderState t) m a)
                           deriving (Functor, Applicative, Monad, MonadTrans)

runBuilderT :: Monad m => (forall s. BuilderT t s m a) -> m a
runBuilderT (BuilderT m) = evalStateT m initialState


type Builder t s a = BuilderT t s Identity a

runBuilder :: (forall s. Builder t s a) -> a
runBuilder m = runIdentity (runBuilderT m)


newtype Ref a s = Ref (Mtbdd a)

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


mapB
    :: (Eq t, Hashable t, Monad m)
    => (t -> t) -> BuilderT t s m (Ref t s) -> BuilderT t s m (Ref t s)
mapB f = (map f =<<)

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


applyB
    :: (Eq t, Hashable t, Monad m)
    => BinOp t
    -> BuilderT t s m (Ref t s)
    -> BuilderT t s m (Ref t s)
    -> BuilderT t s m (Ref t s)
applyB op = bindAp2 (apply op)

apply
    :: (Eq t, Hashable t, Monad m)
    => BinOp t -> Ref t s -> Ref t s -> BuilderT t s m (Ref t s)
apply op (Ref l) (Ref r) = Ref <$> apply' op l r

apply'
    :: (Eq t, Hashable t, Monad m)
    => BinOp t -> Mtbdd t -> Mtbdd t -> BuilderT t s m (Mtbdd t)
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


findOrAddTerminal :: (Eq t, Hashable t, Monad m) => t -> BuilderT t s m (Mtbdd t)
findOrAddTerminal v = BuilderT $ do
    ts <- gets terminals
    case Map.lookup v ts of
        Just term -> return term
        Nothing   -> createTerminal v

findOrAddNode :: Monad m => Var -> Mtbdd t -> Mtbdd t -> BuilderT t s m (Mtbdd t)
findOrAddNode var one zero = BuilderT $ do
    ut <- gets unique
    case Map.lookup (var, nodeId one, nodeId zero) ut of
        Just node -> return node
        Nothing   -> createNode var one zero


bindAp2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindAp2 f mx my = do
    x <- mx
    y <- my
    f x y
