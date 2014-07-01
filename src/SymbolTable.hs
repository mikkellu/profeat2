{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module SymbolTable
  ( module Symbols
  , extendSymbolTable
  ) where

import Control.Applicative
import Control.Lens hiding ( contains )
import Control.Monad.Either
import Control.Monad.Reader
import Control.Monad.State

import Data.List ( (\\) )
import Data.Map ( Map, keys, member )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable

import Error
import Eval
import Symbols
import Syntax
import Template
import Typechecker
import Types

extendSymbolTable :: SymbolTable -> [LDefinition] -> Either Error SymbolTable
extendSymbolTable symTbl defs = flip evalStateT symTbl $ do
    forOf_ (traverse._GlobalDef) defs $ \(VarDecl ident vt e l) ->
        ifNot containsSymbol ident l $
            globals.at ident .= Just (GlobalSymbol l (fromVarType' vt) vt e)

    forOf_ (traverse._ConstDef) defs $ \(Constant ct ident e l) ->
        ifNot containsSymbol ident l $ constants.at ident .=
            Just (ConstSymbol l (fromConstType ct) ct e)


    forOf_ (traverse._FormulaDef) defs $ \f@(Formula ident _ _ l) ->
        ifNot containsSymbol ident l $ formulas.at ident .= Just f

    checkIfNonCyclicFormulas =<< use formulas

    expandExprsOf $ constants.traverse.csExpr
    checkIfNonCyclicConstants =<< use constants
    evalConstValues

    symTbl' <- get
    flip runReaderT symTbl' . forOf (globals.traverse) symTbl' $ \gs -> do
        t   <- fromVarType $ gs^.gsVarType
        vt' <- exprs preprocessExpr $ gs^.gsVarType
        e'  <- _Just preprocessExpr $ gs^.gsExpr
        return (gs & gsType .~ t & gsVarType .~ vt' & gsExpr .~ e')
  where
    ifNot contains ident loc m = do
        st <- get
        case st `contains` ident of
            Just l' -> throw loc $ MultipleDeclarations ident l'
            Nothing -> m

expandExprsOf :: (Applicative m, MonadState SymbolTable m, MonadEither Error m)
              => Traversal' SymbolTable LExpr
              -> m ()
expandExprsOf t = do
    symTbl <- get
    put =<< t (expandFormulas (symTbl^.formulas)) symTbl

evalConstValues :: ( Applicative m
                   , MonadState SymbolTable m
                   , MonadEither Error m
                   )
                => m ()
evalConstValues = do
    symTbl <- get
    void . flip runReaderT symTbl $
        for (symTbl^.constants.to keys) evalConstValue

evalConstValue :: ( Applicative m
                  , MonadState SymbolTable m
                  , MonadReader SymbolTable m
                  , MonadEither Error m
                  )
               => Ident
               -> m ()
evalConstValue ident = do
    val <- use constValues
    unless (ident `member` val) $ do -- check if we already evaluated the constant
        symTbl <- ask
        case symTbl^.constants.at ident of
            Nothing -> return () -- there is no constant with the given identifier; ignore it, as undefined variables will be catched by the type checker
            Just cs -> do
                let e = cs^.csExpr

                forOf_ (traverse.identifiers) (universe e) evalConstValue -- make sure all referenced constants are evaluated
                val' <- use constValues -- load new valuation

                local (constValues .~ val') $ do
                    e' <- unrollLoopExprs e
                    checkInitialization (cs^.csType) e'

                    v <- eval' val' e'

                    constValues.at ident            .= Just v
                    constants.at ident._Just.csExpr .= e'

checkIfNonCyclicFormulas :: (Applicative m, MonadEither Error m)
                         => Table LFormula
                         -> m ()
checkIfNonCyclicFormulas = checkIfNonCyclic post frmAnnot
  where
    post (Formula _ params e _) = universe e^..traverse.identifiers \\ params

checkIfNonCyclicConstants :: (Applicative m, MonadEither Error m)
                          => Table ConstSymbol
                          -> m ()
checkIfNonCyclicConstants = checkIfNonCyclic post (view csLoc)
  where
    post cs = universe (cs^.csExpr)^..traverse.identifiers

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

fromVarType :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
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

fromRange :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
          => LRange
          -> m (Integer, Integer)
fromRange = both preprocessExpr >=> evalRange

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

