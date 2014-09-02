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
    mkRewards <$> execStateT (forAllContexts_ extractRewards) Map.empty
  where
    mkRewards = fmap (RewardsDef . uncurry Rewards) . Map.assocs

extractRewards :: FeatureContext -> StateT RewardStructs Trans ()
extractRewards ctx = void . for (ctx^.this.fsRewards) $ \(Rewards ident rws) ->
    for rws $ trnsReward >=> modify . Map.insertWith mappend ident

trnsReward :: (Applicative m, MonadReader TrnsInfo m, MonadEither Error m)
           => LReward
           -> m [LReward]
trnsReward (Reward action grd e a) = do
    actions' <- trnsActionLabel action
    for actions' $ \(action', _) ->
        Reward action' <$> ((operatingGuard `lAnd`) <$> trnsExpr isBoolType grd)
                       <*> trnsExpr isNumericType e
                       <*> pure a

