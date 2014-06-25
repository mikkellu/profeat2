{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Ident

  , Type(..)
  , CompoundType(..)
  , SimpleType(..)

  , boolType
  , intType
  , intSimpleType
  , doubleType
  , types

  , isBoolType
  , isIntType
  , isNumericType
  , canBeCastedTo

  , Value(..)
  , Valuation

  , prettyValuation
  , prettyRange
  ) where

import Data.Map ( Map )
import qualified Data.Map as Map
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

data CompoundType
  = ArrayType (Maybe (Integer, Integer)) SimpleType
  deriving (Eq, Show)

instance Pretty CompoundType where
    pretty (ArrayType size st) =
        "array" <+> maybe empty prettyRange size <+> "of" <+> pretty st

data SimpleType
  = BoolType
  | IntType (Maybe (Integer, Integer))
  | DoubleType
  deriving (Eq, Show)

instance Pretty SimpleType where
    pretty st = case st of
        BoolType      -> "bool"
        IntType range -> maybe "int" prettyRange range
        DoubleType    -> "double"

prettyRange :: (Pretty a) => (a, a) -> Doc
prettyRange (lower, upper) = brackets (pretty lower <+> ".." <+> pretty upper)

intSimpleType :: SimpleType
intSimpleType = IntType Nothing

boolType, intType, doubleType :: Type
boolType   = SimpleType BoolType
intType    = SimpleType intSimpleType
doubleType = SimpleType DoubleType

-- | A list of all types an expression can possibly have.
types :: [Type]
types = map SimpleType [BoolType, intSimpleType, DoubleType]

-- | Returns 'True' if @t@ is 'BoolType'.
isBoolType :: Type -> Bool
isBoolType (SimpleType BoolType) = True
isBoolType _                     = False

-- | Returns @True@ if @t@ is an 'IntType'.
isIntType :: Type -> Bool
isIntType (SimpleType (IntType _)) = True
isIntType _                        = False

-- | Returns 'True' if @t@ is either 'IntType' or 'DoubleType'.
isNumericType :: Type -> Bool
isNumericType (SimpleType st) = case st of
    IntType _  -> True
    DoubleType -> True
    BoolType   -> False
isNumericType _ = False

-- | Returns 'True' if type @l@ can be casted to type @r@ and vice versa.
canBeCastedTo :: Type -> Type -> Bool
canBeCastedTo l r =
    (isBoolType l && isBoolType r) || (isNumericType l && isNumericType r)

-- | A @Value@ of a variable.
data Value
    = BoolVal !Bool
    | IntVal !Integer
    | DblVal !Double
    deriving (Eq, Show)

instance Pretty Value where
    pretty v = case v of
        BoolVal False -> "false"
        BoolVal True  -> "true"
        IntVal  i     -> integer i
        DblVal  d     -> double d

-- | A variable valuation.
type Valuation = Map Ident Value

-- | The 'Doc' representation of a 'Valuation'.
prettyValuation :: Valuation -> Doc
prettyValuation =
    sep . punctuate comma .
    map (\(name, v) -> text name <> equals <> pretty v) .
    Map.assocs

