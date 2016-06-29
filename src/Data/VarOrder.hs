{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.VarOrder
  ( Var(..)
  , Level(..)

  , VarOrder
  , initialOrder
  , lookupVar
  , lookupLevel
  , swapVars
  ) where

import Data.Hashable

import Data.Maybe (fromMaybe)

import Data.Vector.Unboxed (Vector, (!), (!?), (//))
import qualified Data.Vector.Unboxed as Vec


-- | A variable in a binary decision diagram.
newtype Var = Var Int deriving (Eq, Ord, Show, Hashable)


-- | The level of a node in a binary decision diagram.
newtype Level = Level Int deriving (Eq, Ord, Show, Hashable)


data VarOrder = VarOrder
  { varToLvl :: Vector Int
  , lvlToVar :: Vector Int
  } deriving (Show)

initialOrder :: VarOrder
initialOrder = VarOrder Vec.empty Vec.empty

lookupVar :: VarOrder -> Level -> Var
lookupVar VarOrder{..} (Level lvl) = maybe (Var lvl) Var (lvlToVar !? lvl)

lookupLevel :: VarOrder -> Var -> Level
lookupLevel VarOrder{..} (Var var) = maybe (Level var) Level (varToLvl !? var)


-- | Swaps the variables at positions n and n+1 in the given 'VarOrder'.
swapVars :: Int -> VarOrder -> VarOrder
swapVars i (ensureSize (i + 2) -> VarOrder{..}) =
    let xLvl = i
        yLvl = i + 1
        xVar = lvlToVar ! xLvl
        yVar = lvlToVar ! yLvl

        varToLvl' = varToLvl // [(xVar, yLvl), (yVar, xLvl)]
        lvlToVar' = lvlToVar // [(xLvl, yVar), (yLvl, xVar)]
    in VarOrder varToLvl' lvlToVar'


ensureSize :: Int -> VarOrder -> VarOrder
ensureSize size vo@VarOrder{..}
  | Vec.length varToLvl >= size = vo
  | otherwise = VarOrder
      { varToLvl = copy varToLvl
      , lvlToVar = copy lvlToVar
      }
  where
    copy source = Vec.generate size (\i -> fromMaybe i (source !? i))
