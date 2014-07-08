{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

module Typechecker
  ( evalRange

  , checkInitialization
  , checkIfConst
  , checkIfType
  , checkIfType_

  , typeOf
  ) where

import Control.Applicative
import Control.Lens hiding ( contains )
import Control.Monad.Either
import Control.Monad.Reader

import Data.Map ( member )
import Data.Traversable

import Text.PrettyPrint.Leijen.Text ( displayT, renderOneLine, pretty )

import Error
import Eval
import Symbols
import Syntax
import Types

-- | Evaluates the given range. The expressions must not contain loops or
-- unexpanded formulas.
evalRange :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
          => LRange
          -> m (Integer, Integer)
evalRange range = do
    val <- view constValues

    _ <- both (checkIfType_ isIntType) range
    _ <- both checkIfConst range

    (IntVal lower, IntVal upper) <- both (eval' val) range
    return (lower, upper)

checkInitialization :: ( Applicative m
                       , MonadReader SymbolTable m
                       , MonadEither Error m
                       )
                    => Type
                    -> LExpr
                    -> m ()
checkInitialization t e = checkIfConst e >> checkIfType_ (`isAssignableTo` t) e

checkIfConst :: (MonadReader SymbolTable m, MonadEither Error m)
             => LExpr
             -> m ()
checkIfConst e = view constants >>= \constTbl ->
    case unknownValues constTbl e of
        []    -> return ()
        names -> throw (exprAnnot e) $ UnknownValues e names

unknownValues :: Table ConstSymbol -> Expr a -> [Name a]
unknownValues constTbl = go where
    go (viewIdentExpr -> Just ident)
      | ident `member` constTbl = []
    go (NameExpr name _) = [name]
    go e = concatMap go (children e)

checkIfType :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
            => (Type -> Bool)
            -> LExpr
            -> m Type
checkIfType p e = do
    t <- typeOf e
    unless (p t) . throw (exprAnnot e) $ TypeMismatch expected t e
    return t
  where
    expected = filter p types

checkIfType_ :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
             => (Type -> Bool)
             -> LExpr
             -> m ()
checkIfType_ p e = void $ checkIfType p e

typeOf :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
       => LExpr
       -> m Type
typeOf (BinaryExpr (ArithBinOp Div) lhs rhs _) = do
    checkIfType_ isNumericType lhs; checkIfType_ isNumericType rhs
    return doubleType
typeOf (BinaryExpr binOpT lhs rhs loc) = case binOpT of
    ArithBinOp _ -> do
        tl <- typeOf lhs; tr <- typeOf rhs
        case (tl, tr) of -- cast to double if one of the arguments is double
            (SimpleType DoubleType, _) | isNumericType tr -> return doubleType
            (_, SimpleType DoubleType) | isNumericType tl -> return doubleType
            _ | isNumericType tl && isNumericType tr      -> return intType
              | otherwise -> throw loc $ NotApplicable binOpT tl tr

    EqBinOp _ -> do
        tl <- typeOf lhs; tr <- typeOf rhs
        case (tl, tr) of
            (SimpleType BoolType, SimpleType BoolType) -> return boolType
            _ | isNumericType tl && isNumericType tr   -> return boolType
              | otherwise -> throw loc $ NotApplicable binOpT tl tr

    RelBinOp _   -> do
        tl <- typeOf lhs; tr <- typeOf rhs
        unless (isNumericType tl && isNumericType tr) .
            throw loc $ NotApplicable binOpT tl tr
        return boolType
    LogicBinOp _ -> checkIfType_ isBoolType lhs >> checkIfType isBoolType rhs
    TempBinOp _  -> checkIfType_ isBoolType lhs >> checkIfType isBoolType rhs

typeOf (UnaryExpr unOpT e _) = case unOpT of
    ArithUnOp _                 -> checkIfType isNumericType e
    LogicUnOp _                 -> checkIfType isBoolType e
    TempUnOp _                  -> checkIfType isBoolType e
    ProbUnOp (Prob (Query _))   ->
        checkIfType_ isBoolType e >> return doubleType
    ProbUnOp (Steady (Query _)) ->
        checkIfType_ isBoolType e >> return doubleType
    ProbUnOp _                  -> checkIfType isBoolType e

typeOf (CondExpr cond te ee _) = do
    checkIfType_ isBoolType cond

    tet <- typeOf te
    checkIfType (`canBeCastedTo` tet) ee

typeOf (LoopExpr _ _) = error "Typechecker.typeOf: unresolved LoopExpr"

typeOf (CallExpr (FuncExpr function _) args l) = do
    ts <- for args $ checkIfType isNumericType

    case function of
        FuncMin   -> funcMinMax ts
        FuncMax   -> funcMinMax ts
        FuncFloor -> funcFloorCeil
        FuncCeil  -> funcFloorCeil
        FuncPow   -> do
            checkArgCount 2
            return $ if doubleType `elem` ts then doubleType else intType
        FuncMod   -> do
            checkArgCount 2
            void . for args $ checkIfType_ isIntType
            return intType
        FuncLog   -> checkArgCount 2 >> return doubleType
  where
    funcMinMax ts
      | doubleType `elem` ts = return doubleType
      | otherwise            = return intType
    funcFloorCeil = checkArgCount 1 >> return intType

    numArgs = length args
    checkArgCount n
      | numArgs /= n = throw l $
        ArityError (displayT . renderOneLine $ pretty function) n numArgs
      | otherwise    = return ()

typeOf (CallExpr e _ l) = throw l $ NotAFunction e

typeOf (NameExpr name l)  = lookupType name l
typeOf (FuncExpr f l)     = throw l $ StandaloneFuntion f
typeOf (DecimalExpr _ _)  = return doubleType
typeOf (IntegerExpr _ _)  = return intType
typeOf (BoolExpr _ _)     = return boolType
typeOf (MissingExpr _)    = error "Typechecker.typeOf: unresolved MissingExpr"

