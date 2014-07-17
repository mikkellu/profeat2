{-# LANGUAGE FlexibleContexts #-}

module Types.Util
  ( fromVarType
  , fromVarType'
  , fromConstType
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Error
import Symbols
import Syntax
import Types
import Template
import Typechecker

fromVarType :: ( Applicative m
               , MonadReader r m
               , MonadEither Error m
               , HasSymbolTable r
               , HasScope r
               )
            => LVarType
            -> m Type
fromVarType vt = case vt of
    CompoundVarType (ArrayVarType size svt) ->
        fmap CompoundType $ ArrayType <$> (Just <$> fromRange size)
                                      <*> fromSimpleVarType svt
    SimpleVarType svt -> SimpleType <$> fromSimpleVarType svt
  where
    fromSimpleVarType svt = case svt of
        BoolVarType      -> pure BoolType
        IntVarType range -> IntType . Just <$> fromRange range

fromRange :: ( Applicative m
             , MonadReader r m
             , MonadEither Error m
             , HasSymbolTable r
             , HasScope r
             )
          => LRange
          -> m (Integer, Integer)
fromRange = both preprocessExpr >=> evalRange >=> return . flipBounds
  where
    flipBounds (x, y)
      | x > y     = (y, x)
      | otherwise = (x, y)

-- | Converts a 'VarType' to a 'Type' but ignores the bounds for integer
-- types and array sizes.
fromVarType' :: VarType a -> Type
fromVarType' vt = case vt of
    CompoundVarType (ArrayVarType _ svt) ->
        CompoundType . ArrayType Nothing $ fromSimpleVarType svt
    SimpleVarType svt -> SimpleType $ fromSimpleVarType svt
  where
    fromSimpleVarType :: SimpleVarType a -> SimpleType
    fromSimpleVarType svt = case svt of
        BoolVarType  -> BoolType
        IntVarType _ -> intSimpleType

-- | Converts a 'ConstType' to a 'Type'.
fromConstType :: ConstType -> Type
fromConstType ct = case ct of
    BoolConstType   -> boolType
    IntConstType    -> intType
    DoubleConstType -> doubleType

