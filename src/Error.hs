{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Error
  ( Error(..)
  , ErrorDesc(..)
  , throw
  ) where

import Control.Monad.Either

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import SrcLoc
import Types

-- | Represents translation errors. It provides an error description and the
-- source location.
data Error = Error !SrcLoc !ErrorDesc deriving (Show)

instance Pretty Error where
    pretty (Error l desc) = pretty l <> colon <> line <> pretty desc

-- | Throws an 'Error'.
throw :: (MonadEither Error m) => SrcLoc -> ErrorDesc -> m a
throw l = left . Error l

data ErrorDesc
  = SyntaxError !Text
  | ArityError Ident !Int !Int -- expected actual
  deriving (Show)

instance Pretty ErrorDesc where
    pretty desc = case desc of
        SyntaxError msg -> string msg
        ArityError ident expected actual ->
            text ident <+> "should have" <+> int expected <+> "arguments" <+>
            "but has been given" <+> int actual

