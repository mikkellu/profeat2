{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances #-}

module Control.Monad.Either.Class
  ( MonadEither(..)
  ) where

import Data.Monoid

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

class (Monad m) => MonadEither e m | m -> e where
    left :: e -> m a

instance MonadEither e (Either e) where
    left = Left

instance (MonadEither e m) => MonadEither e (ReaderT r m) where
    left = lift . left

instance (MonadEither e m) => MonadEither e (StateT s m) where
    left = lift . left

instance (Monoid w, MonadEither e m) => MonadEither e (WriterT w m) where
    left = lift . left
