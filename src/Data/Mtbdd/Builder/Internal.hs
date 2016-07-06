{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Mtbdd.Builder.Internal
  ( Ref(..)

  , BuilderT(..)
  , runBuilderT
  , runBuilderTWith

  , Builder
  , runBuilder
  , runBuilderWith

  , getNumberOfVars
  , adjustNumberOfVars
  , getVarOrder
  , modifyVarOrder
  , getDecisionNodes
  , findOrAddTerminal
  , findOrAddNode
  ) where


import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.Mtbdd
import Data.VarOrder


newtype Ref t s = Ref (Node t)


newtype BuilderT t s m a = BuilderT { unB :: StateT (BuilderState t) m a }
                           deriving (Functor, Applicative, Monad, MonadTrans)

runBuilderT :: Monad m => (forall s. BuilderT t s m a) -> m a
runBuilderT (BuilderT m) = evalStateT m initialState

runBuilderTWith
    :: (Eq t, Hashable t, Monad m)
    => Mtbdd t -> (forall s. Ref t s -> BuilderT t s m a) -> m a
runBuilderTWith m f = evalStateT (unB (f (Ref (rootNode m)))) initState
  where
    initState  = mtbddState { order = varOrder m, numVars = numberOfVars m }
    mtbddState = flip execState initialState $
        forM_ (allNodes (rootNode m)) $ \node@(Node nid ty) -> do
            modify $ \s -> s { nextId = max (nid + 1) (nextId s) }
            case ty of
                Terminal v            -> insertTerminal v node
                Decision var one zero -> insertDecision var one zero node


type Builder t s a = BuilderT t s Identity a

runBuilder :: (forall s. Builder t s a) -> a
runBuilder m = runIdentity (runBuilderT m)

runBuilderWith
    :: (Eq t, Hashable t)
    => Mtbdd t -> (forall s. Ref t s -> Builder t s a) -> a
runBuilderWith m f = runIdentity (runBuilderTWith m f)


type UniqueTable t = HashMap (Var, Id, Id) (Node t)


type TerminalTable t = HashMap t (Node t)


data BuilderState t = BuilderState
  { nextId    :: !Id
  , numVars   :: !Int
  , order     :: !VarOrder
  , unique    :: UniqueTable t
  , terminals :: TerminalTable t
  }

initialState :: BuilderState t
initialState = BuilderState
  { nextId    = 0
  , numVars   = 0
  , order     = initialOrder
  , unique    = Map.empty
  , terminals = Map.empty
  }

freshNodeId :: MonadState (BuilderState t) m => m Id
freshNodeId = do
    nid <- gets nextId
    modify $ \s -> s { nextId = nid + 1 }
    return nid


getNumberOfVars :: Monad m => BuilderT t s m Int
getNumberOfVars = BuilderT (gets numVars)

adjustNumberOfVars :: Monad m => Var -> BuilderT t s m ()
adjustNumberOfVars (Var var) = BuilderT $
    modify $ \s -> s { numVars = max (var + 1) (numVars s) }


getVarOrder :: Monad m => BuilderT t s m VarOrder
getVarOrder = BuilderT (gets order)

modifyVarOrder :: Monad m => (VarOrder -> VarOrder) -> BuilderT t s m ()
modifyVarOrder f = BuilderT (modify $ \s -> s { order = f (order s) })


getDecisionNodes :: Monad m => BuilderT t s m [Node t]
getDecisionNodes = BuilderT (Map.elems <$> gets unique)


createTerminal
    :: (Eq t, Hashable t, MonadState (BuilderState t) m) => t -> m (Node t)
createTerminal v = do
    nid <- freshNodeId
    let node = Node nid (Terminal v)
    insertTerminal v node
    return node

insertTerminal
    :: (Eq t, Hashable t, MonadState (BuilderState t) m) => t -> Node t -> m ()
insertTerminal v node = modify $ \s ->
    s { terminals = Map.insert v node (terminals s) }


createDecision
    :: MonadState (BuilderState t) m
    => Var -> Node t -> Node t -> m (Node t)
createDecision var one zero = do
    nid <- freshNodeId
    let node = Node nid (Decision var one zero)
    insertDecision var one zero node
    return node

insertDecision
    :: MonadState (BuilderState t) m
    => Var -> Node t -> Node t -> Node t -> m ()
insertDecision var one zero node = modify $ \s ->
    s { unique = Map.insert (var, nodeId one, nodeId zero) node (unique s) }


findOrAddTerminal :: (Eq t, Hashable t, Monad m) => t -> BuilderT t s m (Node t)
findOrAddTerminal v = BuilderT $ do
    ts <- gets terminals
    case Map.lookup v ts of
        Just term -> return term
        Nothing   -> createTerminal v

findOrAddNode
    :: Monad m => Var -> Node t -> Node t -> BuilderT t s m (Node t)
findOrAddNode var one zero = BuilderT $ do
    ut <- gets unique
    case Map.lookup (var, nodeId one, nodeId zero) ut of
        Just node -> return node
        Nothing   -> createDecision var one zero
