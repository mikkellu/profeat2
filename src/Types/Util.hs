{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Types.Util
  ( toVarSymbol
  , toParamSymbol
  , fromVarType
  , fromVarType'
  , fromConstType
  , toConstType
  ) where

import Control.Lens
import Control.Monad.Reader

import Data.List
import Data.List.NonEmpty

import Error
import Symbols
import Syntax
import Types
import Template
import Typechecker

toVarSymbol :: ( MonadReader r m
               , MonadError Error m
               , HasSymbolTable r
               , HasScope r
               )
            => Bool
            -> LVarDecl
            -> m VarSymbol
toVarSymbol isAttrib (VarDecl _ vt mInit l) = do
    t      <- fromVarType vt
    mInit' <- checkInit t mInit
    return $ VarSymbol l isAttrib t mInit'

toParamSymbol :: ( MonadReader r m
                 , MonadError Error m
                 , HasSymbolTable r
                 , HasScope r
                 )
              => LVarDecl
              -> m ParamSymbol
toParamSymbol (VarDecl ident vt mInit l) = do
    vt'    <- prepExprs vt
    t      <- fromVarType vt'
    mInit' <- checkInit t mInit
    return $ ParamSymbol t (VarDecl ident vt' mInit' l)

checkInit :: ( MonadReader r m
             , MonadError Error m
             , HasSymbolTable r
             , HasScope r
             )
          => Type
          -> Maybe LExpr
          -> m (Maybe LExpr)
checkInit t = _Just $ \e -> do
    e' <- prepExpr e
    checkInitialization t e'
    return e'

fromVarType :: ( MonadReader r m
               , MonadError Error m
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

fromRange :: ( MonadReader r m
             , MonadError Error m
             , HasSymbolTable r
             , HasScope r
             )
          => LRange
          -> m (Integer, Integer)
fromRange = both prepExpr >=> evalRange >=> return . flipBounds
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
fromConstType :: ConstType -> Expr a -> Type
fromConstType ct e =
    let st = case ct of
                 BoolConstType   -> BoolType
                 IntConstType    -> IntType Nothing
                 DoubleConstType -> DoubleType
    in case e of
        ArrayExpr (_ :| es) _ ->
            CompoundType $ ArrayType (Just (0, genericLength es)) st
        CallExpr (FuncExpr FuncBinom _) _ _ ->
            CompoundType $ ArrayType Nothing DoubleType
        _ -> SimpleType st

toConstType :: SimpleType -> ConstType
toConstType = \case
    BoolType   -> BoolConstType
    IntType _  -> IntConstType
    DoubleType -> DoubleConstType

