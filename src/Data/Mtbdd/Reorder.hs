{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mtbdd.Reorder
  ( swap
  ) where


import Control.Monad.State

import qualified Data.HashMap.Strict as Map

import Data.Mtbdd.Internal
import Data.Mtbdd.Builder.Internal
import Data.VarOrder


swap :: (Eq t, Monad m) => Int -> Node t -> BuilderT t s m (Node t)
swap i node = do
    modifyVarOrder (swapVars i)
    evalStateT (go node) Map.empty
  where
    go n@(Node nid ty) = case ty of
        Terminal _ -> return n
        Decision (Level lvl) one zero
          | lvl < i -> do
              one'  <- go one
              zero' <- go zero

              if one' == zero'
                  then return one'
                  else lift (findOrAddNode (Level lvl) one' zero')
          | lvl == i -> do
              done <- get
              case Map.lookup nid done of
                  Just node' -> return node'
                  Nothing
                    | min (level one) (level zero) == Level (i + 1) -> do
                          result <- lift (swapNode i one zero)
                          modify (Map.insert nid result)
                          return result
                    | otherwise -> return n
          | otherwise -> return n

swapNode
    :: (Eq t, Monad m)
    => Int
    -> Node t
    -> Node t
    -> BuilderT t s m (Node t)
swapNode i one zero = do
    one'  <- mkNode True one zero
    zero' <- mkNode False one zero

    findOrAddNode (Level i) one' zero'
  where
    nextLvl = Level (i + 1)
    mkNode b one' zero' =
        let one''  = child one' nextLvl b
            zero'' = child zero' nextLvl b
        in if one'' == zero''
               then return one''
               else findOrAddNode nextLvl one'' zero''
