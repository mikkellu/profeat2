{-# LANGUAGE FlexibleContexts
           , LambdaCase
           , ViewPatterns
           , RankNTypes
           , TupleSections #-}

module Typechecker
  ( SymbolInfo(..)
  , siType

  , evalRange
  , evalInteger

  , checkInitialization
  , checkIfConst
  , isConstExpr
  , checkIfType
  , checkIfType_

  , typeOf
  , getSymbolInfo
  , lookupSymbolInfo

  , getContext
  ) where

import Control.Applicative
import Control.Lens hiding ( contains )
import Control.Monad.Reader

import Data.Array
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import Data.Map ( member )
import Data.Maybe
import Data.Text.Lazy ( Text )
import Data.Traversable

import Text.PrettyPrint.Leijen.Text ( Pretty, displayT, renderOneLine, pretty )

import Error
import Eval
import Symbols
import Syntax
import Types

data SymbolInfo = SymbolInfo
  { siScope      :: !Scope
  , siIdent      :: !Ident
  , siIndex      :: Maybe LExpr
  , siSymbolType :: !Type
  }

-- | Evaluates the given range. The expressions must not contain loops or
-- unexpanded formulas.
evalRange :: (Applicative m, MonadReader Env m, MonadEither Error m)
          => LRange
          -> m (Integer, Integer)
evalRange = both evalInteger

-- | Evaluates the given expression as 'Integer'. The expression must not
-- contain loops or unexpanded formulas.
evalInteger :: ( Applicative m
               , MonadReader Env m
               , MonadEither Error m
               )
            => LExpr
            -> m Integer
evalInteger e = do
    checkIfType_ isIntType e >> checkIfConst e

    val <- view constValues
    IntVal v <- eval' val e

    return v

checkInitialization :: ( Applicative m
                       , MonadReader Env m
                       , MonadEither Error m
                       )
                    => Type
                    -> LExpr
                    -> m ()
checkInitialization t e = checkIfType_ (`isAssignableTo` t) e >> checkIfConst e

checkIfConst :: (MonadReader r m, MonadEither Error m, HasSymbolTable r)
             => LExpr
             -> m ()
checkIfConst e = view constants >>= \constTbl ->
    case unknownValues constTbl e of
        []    -> return ()
        names -> throw (exprAnnot e) $ UnknownValues e names

isConstExpr :: ( Functor m
               , MonadReader r m
               , MonadEither Error m
               , HasSymbolTable r
               )
        => LExpr
        -> m Bool
isConstExpr e = null . flip unknownValues e <$> view constants

unknownValues :: Table ConstSymbol -> Expr a -> [Name a]
unknownValues constTbl = go where
    go (viewIdentExpr -> Just ident)
      | ident `member` constTbl = []
    go (NameExpr name _) = [name]
    go e = concatMap go (children e)

checkIfType :: ( Applicative m
               , MonadReader Env m
               , MonadEither Error m
               )
            => (Type -> Bool)
            -> LExpr
            -> m Type
checkIfType p e = do
    t <- typeOf e
    unless (p t) . throw (exprAnnot e) $ TypeMismatch expected t e
    return t
  where
    expected = filter p types

checkIfType_ :: ( Applicative m
                , MonadReader Env m
                , MonadEither Error m
                )
             => (Type -> Bool)
             -> LExpr
             -> m ()
checkIfType_ p e = void $ checkIfType p e

typeOf :: ( Applicative m
          , MonadReader Env m
          , MonadEither Error m
          )
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

typeOf (CallExpr (FuncExpr function _) args l) = case function of
    FuncMin    -> do
        ts <- for args $ checkIfType isNumericType
        funcMinMax ts
    FuncMax    -> do
        ts <- for args $ checkIfType isNumericType
        funcMinMax ts
    FuncFloor  -> funcFloorCeil
    FuncCeil   -> funcFloorCeil
    FuncPow    -> do
        checkArgCount 2
        ts <- for args $ checkIfType isNumericType
        return $ if doubleType `elem` ts then doubleType else intType
    FuncMod    -> do
        checkArgCount 2
        void . for args $ checkIfType_ isIntType
        return intType
    FuncLog    -> checkArgCount 2 >> return doubleType
    FuncActive -> do
        checkArgCount 1
        void $ traverse checkIfFeature args
        return boolType
  where
    funcMinMax ts
      | doubleType `elem` ts = return doubleType
      | otherwise            = return intType
    funcFloorCeil = checkArgCount 1 >> return intType

    numArgs = length args
    checkArgCount n
      | numArgs /= n = throw l $
        ArityError (prettyText function) n numArgs
      | otherwise    = return ()

typeOf (CallExpr e _ l) = throw l $ NotAFunction e

typeOf (NameExpr name _)  = getSymbolInfo name >>= siType
typeOf (FuncExpr f l)     = throw l $ StandaloneFuntion f
typeOf (DecimalExpr _ _)  = return doubleType
typeOf (IntegerExpr _ _)  = return intType
typeOf (BoolExpr _ _)     = return boolType
typeOf (MissingExpr _)    = error "Typechecker.typeOf: unresolved MissingExpr"

checkIfFeature :: (Applicative m, MonadReader Env m, MonadEither Error m)
               => LExpr
               -> m ()
checkIfFeature (NameExpr name l) = do
    (_, name') <- getContext name
    unless (isNothing name') . throw l . NotAFeature $ prettyText name
checkIfFeature e = throw (exprAnnot e) . NotAFeature $ prettyText e

siType :: (MonadEither Error m) => SymbolInfo -> m Type
siType (SymbolInfo _ ident idx t) = case t of
    CompoundType (ArrayType _ st) -- TODO: check array bounds
      | isJust idx -> return (SimpleType st)
      | otherwise  -> return t
    SimpleType _ -> case idx of
        Just e  -> throw (exprAnnot e) $ NotAnArray ident
        Nothing -> return t

getSymbolInfo :: ( Applicative m
                 , MonadReader Env m
                 , MonadEither Error m
                 )
              => LName
              -> m SymbolInfo
getSymbolInfo name@(Name _ l) = lookupSymbolInfo name >>= \case
    Just si -> return si
    Nothing -> throw l $ UndefinedIdentifier (prettyText name)

lookupSymbolInfo :: ( Applicative m
                    , MonadReader Env m
                    , MonadEither Error m
                    )
                 => LName
                 -> m (Maybe SymbolInfo)
lookupSymbolInfo (viewSimpleName -> Just (ident, idx, _)) = do
    sc <- view scope
    lookupType ident >>= \case
        Just t  -> return . Just $ SymbolInfo sc ident idx t
        Nothing -> fmap (SymbolInfo Global ident idx) <$> lookupTypeGlobal ident
lookupSymbolInfo name@(Name _ l) = getContext name >>= \case
    (_, Nothing)      -> throw l $ NotAVariable (prettyText name)
    (ctx, Just name') -> case name' of
        (viewSimpleName -> Just (ident', idx', l')) ->
            case lookupTypeIn ctx ident' of
                Just t  -> return . Just $ SymbolInfo (Local ctx) ident' idx' t
                Nothing -> notAMember ctx name' l'
        Name _ l' -> notAMember ctx name' l'
  where
    notAMember ctx name' l' =
        throw l' $ NotAMember (prettyText ctx) (prettyText name')

lookupType :: ( Applicative m
              , MonadReader Env m
              , MonadEither Error m
              )
           => Ident
           -> m (Maybe Type)
lookupType ident = view scope >>= \case
    Local ctx  -> return $ lookupTypeIn ctx ident
    LocalCtrlr -> lookupTypeCtrlr ident
    Global     -> return Nothing

lookupTypeGlobal :: (MonadReader Env m) => Ident -> m (Maybe Type)
lookupTypeGlobal ident = do
    symTbl <- view symbolTable
    return $ lookupOf gsType (symTbl^.globals) ident
         <|> lookupOf csType (symTbl^.constants) ident

lookupTypeCtrlr:: (MonadReader Env m) => Ident -> m (Maybe Type)
lookupTypeCtrlr ident = do
    symTbl <- view symbolTable
    return $ symTbl^?controller._Just.ctsVars.at ident._Just.vsType

lookupTypeIn :: FeatureContext -> Ident -> Maybe Type
lookupTypeIn ctx = lookupOf vsType $ ctx^.this.fsVars

lookupOf :: Getter a Type -> Table a -> Ident -> Maybe Type
lookupOf g tbl ident = tbl^?at ident._Just.g

getContext :: ( Applicative m
              , MonadReader Env m
              , MonadEither Error m
              )
           => LName
           -> m (FeatureContext, Maybe LName)
getContext (Name name l) = do
    let (ident, idx) :| qs = name

    i   <- _Just evalInteger idx
    ctx <- findContext ident i l

    over _2 (fmap (flip Name l) . nonEmpty) <$> go ctx qs
  where
    go ctx qs = case qs of
        (ident, idx):qs' ->
            match ident idx (ctx^.this) >>= \case
                Just fs' -> go (extendContext fs' ctx) qs'
                Nothing  -> return (ctx, qs)
        [] -> return (ctx, [])

    match ident idx fs   = _Just (select ident idx) $ fs^.fsChildren.at ident
    select ident idx fss = let card = bounds fss in case idx of
        Just e -> do
            i <- evalInteger e
            unless (inRange card i) . throw l $ IndexOutOfBounds card i
            return $ fss ! i
        Nothing -> do
            when (featureCardinality fss > 1) . throw l $ MissingIndex ident
            return $ fss ! 0

findContext :: ( MonadReader r m
               , MonadEither Error m
               , HasSymbolTable r
               )
            => Ident
            -> Maybe Integer
            -> SrcLoc
            -> m FeatureContext
findContext ident idx l = do
    root <- view rootFeature
    let ctxs = filter (match . thisFeature) $ allContexts root

    case ctxs of
        []    -> throw l $ UndefinedIdentifier name
        [ctx] -> return ctx
        _     -> throw l . AmbiguousFeature name $ fmap prettyText ctxs
  where
    match = case idx of
        Just i  -> liftA2 (&&) ((ident ==) . _fsIdent) ((i ==) . _fsIndex)
        Nothing -> (ident ==) . _fsIdent
    name = prettyText $ Name ((ident, fmap (flip IntegerExpr ()) idx) :| []) ()

prettyText :: (Pretty a) => a -> Text
prettyText = displayT . renderOneLine . pretty

