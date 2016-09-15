{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Mtbdd.Reorder
  ( sift
  , siftOnce
  , swap
  ) where


import Control.Monad.State

import Data.Hashable (Hashable)
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Mtbdd
import Data.Mtbdd.Builder
import Data.Mtbdd.Builder.Internal
import Data.VarOrder


type ReorderT t s m a = StateT (HashMap Id (Node t)) (BuilderT t s m) a


sift :: (Eq t, Hashable t) => Mtbdd t -> Mtbdd t
sift m = find m (tail (iterate siftOnce m)) where
    find r (c:cs)
      | size (rootNode r) > size (rootNode c) = find c cs
      | otherwise                             = r
    find _ [] = error "empty list"


siftOnce :: (Eq t, Hashable t) => Mtbdd t -> Mtbdd t
siftOnce m = runBuilderWith m $ \(Ref node) -> do
    numVars <- getNumberOfVars
    result <- foldM shiftVar node [0 .. numVars - 1]
    deref (Ref result)
  where
    shiftVar node (Var -> var) = do
        numVars <- getNumberOfVars
        vo <- getVarOrder
        let lvl = lookupLevel vo var
            s   = size node
            cs  = Map.singleton s (node, vo)

        cs' <- goUp cs lvl node
        setVarOrder vo
        cs'' <- goDown numVars cs' lvl node

        let (node', vo') = snd (Map.findMin cs'')
        setVarOrder vo'
        return node'

    goUp candidates (Level lvl) node
      | lvl == 0  = return candidates
      | otherwise = do
          let lvl' = Level (lvl - 1)
          node' <- swap lvl' node
          vo'   <- getVarOrder
          let s = size node'
              candidates' = Map.insert s (node', vo') candidates
          goUp candidates' lvl' node'

    goDown numVars candidates (Level lvl) node
      | lvl >= numVars - 2 = return candidates
      | otherwise = do
          node' <- swap (Level lvl) node
          vo'   <- getVarOrder
          let s = size node'
              candidates' = Map.insert s (node', vo') candidates
          goDown numVars candidates' (Level (lvl + 1)) node'



swap :: Monad m => Level -> Node t -> BuilderT t s m (Node t)
swap lvl node = do
    result <- evalStateT (go node) HashMap.empty

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
    :: Monad m
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
    :: Monad m
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
    case HashMap.lookup nid done of
        Just node -> return node
        Nothing   -> do
            result <- lift m
            modify (HashMap.insert nid result)
            return result
