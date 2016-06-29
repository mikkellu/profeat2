{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Mtbdd.Builder.Internal
  ( Ref(..)

  , BuilderT(..)
  , runBuilderT

  , Builder
  , runBuilder

  , getVarOrder

  , findOrAddTerminal
  , findOrAddNode
  ) where


import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.Mtbdd.Internal
import Data.VarOrder


newtype Ref t s = Ref (Node t)


newtype BuilderT t s m a = BuilderT (StateT (BuilderState t) m a)
                           deriving (Functor, Applicative, Monad, MonadTrans)

runBuilderT :: Monad m => (forall s. BuilderT t s m a) -> m a
runBuilderT (BuilderT m) = evalStateT m initialState


type Builder t s a = BuilderT t s Identity a

runBuilder :: (forall s. Builder t s a) -> a
runBuilder m = runIdentity (runBuilderT m)


type UniqueTable t = HashMap (Level, Id, Id) (Node t)


type TerminalTable t = HashMap t (Node t)


data BuilderState t = BuilderState
  { nextId    :: !Id
  , order     :: !VarOrder
  , unique    :: UniqueTable t
  , terminals :: TerminalTable t
  }

initialState :: BuilderState t
initialState = BuilderState
  { nextId    = 0
  , order     = initialOrder
  , unique    = Map.empty
  , terminals = Map.empty
  }

freshNodeId :: MonadState (BuilderState t) m => m Id
freshNodeId = do
    nid <- gets nextId
    modify $ \s -> s { nextId = nid + 1 }
    return nid


getVarOrder :: Monad m => BuilderT t s m VarOrder
getVarOrder = BuilderT (gets order)


createTerminal
    :: (Eq t, Hashable t, MonadState (BuilderState t) m) => t -> m (Node t)
createTerminal v = do
    nid <- freshNodeId
    let term = Node nid (Terminal v)
    modify $ \s -> s { terminals = Map.insert v term (terminals s) }
    return term

createNode
    :: MonadState (BuilderState t) m
    => Level -> Node t -> Node t -> m (Node t)
createNode lvl one zero = do
    nid <- freshNodeId
    let node = Node nid (Decision lvl one zero)
    modify $ \s ->
        s { unique = Map.insert (lvl, nodeId one, nodeId zero) node (unique s) }
    return node


findOrAddTerminal :: (Eq t, Hashable t, Monad m) => t -> BuilderT t s m (Node t)
findOrAddTerminal v = BuilderT $ do
    ts <- gets terminals
    case Map.lookup v ts of
        Just term -> return term
        Nothing   -> createTerminal v

findOrAddNode
    :: Monad m => Level -> Node t -> Node t -> BuilderT t s m (Node t)
findOrAddNode lvl one zero = BuilderT $ do
    ut <- gets unique
    case Map.lookup (lvl, nodeId one, nodeId zero) ut of
        Just node -> return node
        Nothing   -> createNode lvl one zero
