{-# LANGUAGE FlexibleContexts #-}

module Translator.Rewards
  ( trnsRewards
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid
import Data.Traversable

import Error
import Symbols
import Syntax
import Types

import Translator.Common

type RewardStructs = Map Ident [LReward]

trnsRewards :: Trans [LDefinition]
trnsRewards =
    mkRewardDefs <$> execStateT (forAllContexts_ extractRewards) Map.empty
  where
    mkRewardDefs           = fmap (RewardsDef . mkRewards) . Map.assocs
    mkRewards (ident, rws) = Rewards ident rws noLoc

extractRewards :: FeatureContext -> StateT RewardStructs Trans ()
extractRewards ctx =
    void . for (ctx^.this.fsRewards) $ \(Rewards ident rws _) ->
        for rws $ trnsReward >=> modify . Map.insertWith mappend ident

trnsReward :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
           => LReward
           -> m [LReward]
trnsReward (Reward action grd e a) = do
    actions' <- trnsActionLabel action
    for actions' $ \(action', _) ->
        Reward action' <$> ((operatingGuard `lAnd`) <$> trnsExpr isBoolType grd)
                       <*> trnsExpr isNumericType e
                       <*> pure a

