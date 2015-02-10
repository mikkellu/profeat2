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

  , isAssignableTo
  , canBeCastedTo

  , ReconfType(..)
  , reconfType

  , Value(..)
  , Valuation

  , prettyValuation
  , prettyRange
  ) where

import Control.Lens.Iso

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

-- | Returns 'True' if an expression of the first type can be assigned to a
-- variable of the second type.
isAssignableTo :: Type -> Type -> Bool
isAssignableTo (CompoundType (ArrayType sl tl))
               (CompoundType (ArrayType sr tr)) =
    sl == sr && SimpleType tl `isAssignableTo` SimpleType tr
isAssignableTo tl@(SimpleType _) (CompoundType (ArrayType _ tr)) =
    tl `isAssignableTo` SimpleType tr
isAssignableTo (SimpleType tl) (SimpleType tr) = case (tl, tr) of
    (BoolType   , BoolType  ) -> True
    (IntType _  , IntType _ ) -> True
    (DoubleType , DoubleType) -> True
    (IntType _  , DoubleType) -> True
    (_          , _         ) -> False
isAssignableTo _ _ = False

-- | Returns 'True' if type @l@ can be casted to type @r@ and vice versa.
canBeCastedTo :: Type -> Type -> Bool
canBeCastedTo l r =
    (isBoolType l && isBoolType r) || (isNumericType l && isNumericType r)

-- | A reconfiguration may either activate or deactivate features.
data ReconfType = ReconfActivate | ReconfDeactivate deriving (Eq, Ord)

-- | 'ReconfType' is isomorphic to 'Bool'.
reconfType :: Iso' Bool ReconfType
reconfType = iso fromBool toBool
  where
    toBool ReconfActivate   = True
    toBool ReconfDeactivate = False
    fromBool True  = ReconfActivate
    fromBool False = ReconfDeactivate

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
type Valuation = Map (Ident, Integer) Value

-- | The 'Doc' representation of a 'Valuation'.
prettyValuation :: Valuation -> Doc
prettyValuation =
    sep . punctuate comma .
    map (\((name, i), v) -> text name <> idx i <> equals <> pretty v) .
    Map.assocs
  where
    idx i | i > 0     = brackets (integer i)
          | otherwise = empty

