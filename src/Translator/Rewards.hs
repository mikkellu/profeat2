{-# LANGUAGE FlexibleContexts #-}

module Translator.Rewards
  ( trnsRewards
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Traversable

import Error
import Symbols
import Syntax
import Syntax.Util
import Types

import Translator.Common

type RewardStructs = Map Ident [LReward]

trnsRewards :: Trans [LDefinition]
trnsRewards =
    mkRewardDefs <$> execStateT (forAllContexts_ extractRewards) Map.empty
  where
    mkRewardDefs           = fmap (RewardsDef . mkRewards) . Map.assocs
    mkRewards (ident, rws) = Rewards ident (Repeatable (fmap One rws)) noLoc

extractRewards :: FeatureContext -> StateT RewardStructs Trans ()
extractRewards ctx =
    void . for (ctx^.this.fsRewards) $ \(Rewards ident rws _) ->
        forOf_ ones rws $ \rw -> do
            modify . Map.insertWith mappend ident =<< trnsReward rw
            return rw

trnsReward :: (MonadReader TrnsInfo m, MonadError Error m)
           => LReward
           -> m [LReward]
trnsReward (Reward action grd e a) = maybe ((:[]) <$> trns Nothing)
    (traverse (trns . Just . fst) <=< trnsActionLabel) action
  where
    trns act = Reward act <$> trnsExpr isBoolType grd
                          <*> trnsExpr isNumericType e
                          <*> pure a

