module Tests.Util
  ( sat'
  ) where

import Data.Bdd
import Data.Sequence ( Seq )
import Data.Set      ( Set )

sat' :: Bdd -> Set (Seq Bool)
sat' f = sat (varCount f) f

varCount :: Bdd -> Int
varCount = (1+) . foldr max (-1) . fmap getVar . allNodes where
    getVar node = case viewNode node of
        Terminal _       -> -1
        Decision var _ _ -> getVariable var

