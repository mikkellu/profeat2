{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Ident

  , Type(..)
  , CompoundType(..)
  , SimpleType(..)
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

type Ident = Text

-- | The type of a variable or constant.
data Type
  = CompoundType CompoundType
  | SimpleType   SimpleType
  deriving (Eq, Show)

instance Pretty Type where
    pretty t = case t of
        CompoundType ct -> pretty ct
        SimpleType st   -> pretty st

data SimpleType
  = BoolType
  | IntType (Maybe (Integer, Integer))
  | DoubleType
  deriving (Eq, Show)

instance Pretty SimpleType where
    pretty st = case st of
        BoolType                      -> "bool"
        IntType (Just (lower, upper)) ->
            brackets (integer lower <+> ".." <+> integer upper)
        IntType Nothing               -> "int"
        DoubleType                    -> "double"

data CompoundType
  = ArrayType (Integer, Integer) SimpleType
  deriving (Eq, Show)

instance Pretty CompoundType where
    pretty (ArrayType (lower, upper) st) =
        "array" <+> brackets (integer lower <+> ".." <+> integer upper) <+>
        "of" <+> pretty st

