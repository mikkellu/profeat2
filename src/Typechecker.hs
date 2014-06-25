{-# LANGUAGE FlexibleContexts #-}

module Typechecker
  ( extendSymbolTable
  , typeOf
  ) where

import Control.Applicative
import Control.Lens hiding ( contains )
import Control.Monad.Either
import Control.Monad.Reader
import Control.Monad.State

import Data.List ( (\\) )
import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable

import Text.PrettyPrint.Leijen.Text ( displayT, renderOneLine, pretty )

import Error
import SrcLoc
import Symbols
import Syntax
import Types

-- | Extends the 'SymbolTable' with the given 'Definition's. However, the
-- module counts as well as the variables bounds are not computed.
extendSymbolTable :: SymbolTable -> [LDefinition] -> Either Error SymbolTable
extendSymbolTable symTbl defs = flip execStateT symTbl $ do
    forOf_ (traverse._GlobalDef) defs $ \(VarDecl ident vt _ l) ->
        ifNot containsSymbol ident l $
            globals.at ident .= Just (GlobalSymbol l (fromVarType vt))

    forOf_ (traverse._ConstDef) defs $ \(Constant ct ident _ l) ->
        ifNot containsSymbol ident l $ constants.at ident .=
            Just (ConstSymbol l (fromConstType ct) Nothing)

    forOf_ (traverse._FormulaDef) defs $ \f@(Formula ident _ _ l) ->
        ifNot containsSymbol ident l $ formulas.at ident .= Just f

    checkIfNonCyclicFormulas =<< use formulas
  where
    ifNot contains ident loc m = do
        st <- get
        case st `contains` ident of
            Just l' -> throw loc $ MultipleDeclarations ident l'
            Nothing -> m

-- | Converts a 'VarType' to a 'Type' but ignores the bounds for integer
-- types and array sizes.
fromVarType :: VarType a -> Type
fromVarType vt = case vt of
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

checkIfNonCyclicFormulas :: (Applicative m, MonadEither Error m)
                         => Map Ident LFormula
                         -> m ()
checkIfNonCyclicFormulas = checkIfNonCyclic post frmAnnot
  where
    post (Formula _ params e _) =
        universe e^..traverse._NameExpr._1._Name \\ params

checkIfNonCyclic :: (Applicative m, MonadEither Error m)
                 => (a -> [Ident])
                 -> (a -> SrcLoc)
                 -> Map Ident a
                 -> m ()
checkIfNonCyclic post annot defs = void . for (Map.assocs defs) $ \(i, x) ->
    when (isCyclicDef post defs i) . throw (annot x) $ CyclicDependency i

isCyclicDef :: (a -> [Ident]) -> Map Ident a -> Ident -> Bool
isCyclicDef post defs = isCyclic Set.empty
  where
    isCyclic is i
      | i `Set.member` is = True
      | otherwise =
        case defs^.at i of
            Just x  -> let is' = Set.insert i is in any (isCyclic is') $ post x
            Nothing -> False

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

typeOf (NameExpr name l)  = lookupType name l =<< ask
typeOf (FuncExpr f l)     = throw l $ StandaloneFuntion f
typeOf (DecimalExpr _ _)  = return doubleType
typeOf (IntegerExpr _ _)  = return intType
typeOf (BoolExpr _ _)     = return boolType
typeOf (MissingExpr _)    = error "Typechecker.typeOf: unresolved MissingExpr"

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

