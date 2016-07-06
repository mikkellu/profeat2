{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mtbdd.Reorder
  ( swap
  ) where


import Control.Monad.State

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.Mtbdd.Internal
import Data.Mtbdd.Builder.Internal
import Data.VarOrder


type ReorderT t s m a = StateT (HashMap Id (Node t)) (BuilderT t s m) a


swap :: (Eq t, Monad m) => Level -> Node t -> BuilderT t s m (Node t)
swap lvl node = do
    result <- evalStateT (go node) Map.empty

    vo <- getVarOrder
    let varNext = lookupVar vo lvlNext
    adjustNumberOfVars varNext
    modifyVarOrder (swapVars lvl)

    return result
  where
    go n@(Node nid ty) = do
        vo <- lift getVarOrder
        case ty of
            Terminal _ -> return n
            Decision var one zero
              | lookupLevel vo var < lvl  -> rebuildNode go var one zero
              | lookupLevel vo var == lvl -> unlessDone nid $
                  if min (level vo one) (level vo zero) == lvlNext
                      then swapNode var (lookupVar vo lvlNext) one zero
                      else return n
              | otherwise -> return n
    lvlNext = Level (i + 1)
    Level i = lvl


rebuildNode
    :: (Eq t, Monad m)
    => (Node t -> ReorderT t s m (Node t))
    -> Var
    -> Node t
    -> Node t
    -> ReorderT t s m (Node t)
rebuildNode f var one zero = do
    one'  <- f one
    zero' <- f zero

    if one' == zero'
        then return one'
        else lift (findOrAddNode var one' zero')


swapNode
    :: (Eq t, Monad m)
    => Var
    -> Var
    -> Node t
    -> Node t
    -> BuilderT t s m (Node t)
swapNode var varNext one zero = do
    one'  <- mkNode True  one zero
    zero' <- mkNode False one zero

    findOrAddNode varNext one' zero'
  where
    mkNode b one' zero' = do
        let one''  = child' one'  b
            zero'' = child' zero' b

        if one'' == zero''
               then return one''
               else findOrAddNode var one'' zero''

    child' node@(Node _ ty) b = case ty of
        Decision nodeVar one' zero'
          | nodeVar == varNext -> if b then one' else zero'
        _ -> node


unlessDone
    :: Monad m
    => Id
    -> BuilderT t s m (Node t)
    -> ReorderT t s m (Node t)
unlessDone nid m = do
    done <- get
    case Map.lookup nid done of
        Just node -> return node
        Nothing   -> do
            result <- lift m
            modify (Map.insert nid result)
            return result
