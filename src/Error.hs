{-# LANGUAGE FlexibleContexts #-}

module Error
  ( Error(..)
  , ErrorDesc(..)
  , throw
  ) where

import Control.Monad.Either

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import SrcLoc

-- | Represents translation errors. It provides an error description and the
-- source location.
data Error = Error !SrcLoc !ErrorDesc

instance Pretty Error where
    pretty (Error l desc) = pretty l <> colon <> line <> pretty desc

-- | Throws an 'Error'.
throw :: (MonadEither Error m) => SrcLoc -> ErrorDesc -> m a
throw l = left . Error l

data ErrorDesc
  = SyntaxError !Text

instance Pretty ErrorDesc where
    pretty desc = case desc of
        SyntaxError msg -> string msg

