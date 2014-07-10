{-# LANGUAGE FlexibleContexts
           , LambdaCase
           , ViewPatterns
           , RankNTypes
           , TupleSections #-}

module Typechecker
  ( evalRange
  , evalInteger

  , checkInitialization
  , checkIfConst
  , checkIfType
  , checkIfType_

  , typeOf

  , getContext
  ) where

import Control.Applicative
import Control.Lens hiding ( contains )
import Control.Monad.Either
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
        ArityError (prettyText function) n numArgs
      | otherwise    = return ()

typeOf (CallExpr e _ l) = throw l $ NotAFunction e

typeOf (NameExpr name l) = lookupType name >>= \case
    Just t  -> return t
    Nothing -> throw l $ UndefinedIdentifier (prettyText name)

typeOf (FuncExpr f l)     = throw l $ StandaloneFuntion f
typeOf (DecimalExpr _ _)  = return doubleType
typeOf (IntegerExpr _ _)  = return intType
typeOf (BoolExpr _ _)     = return boolType
typeOf (MissingExpr _)    = error "Typechecker.typeOf: unresolved MissingExpr"

lookupType :: ( Applicative m
              , MonadReader Env m
              , MonadEither Error m
              )
           => LName
           -> m (Maybe Type)
lookupType (viewSimpleName -> Just (ident, idx, l)) = -- no member access
    lookupTypeLocal ident idx l >>= \case       -- first check local scope of the feature...
        Just t  -> return (Just t)
        Nothing -> lookupTypeGlobal ident idx l -- ...and then the global scope
lookupType name@(Name _ l) = getContext name >>= \case -- name has a context
    (_, Nothing)      -> throw l $ NotAVariable (prettyText name)
    (ctx, Just name') -> case name' of
        (viewSimpleName -> Just (ident', idx', l')) ->
            lookupTypeLocalIn ctx ident' idx' l' >>= \case
                Just t  -> return (Just t)
                Nothing -> notAMember ctx name' l'
        Name _ l' -> notAMember ctx name' l'
  where
    notAMember ctx name' l' =
        throw l' $ NotAMember (prettyText ctx) (prettyText name')

lookupTypeGlobal :: ( Applicative m
                    , MonadReader r m
                    , MonadEither Error m
                    , HasSymbolTable r
                    )
                 => Ident
                 -> Maybe LExpr
                 -> SrcLoc
                 -> m (Maybe Type)
lookupTypeGlobal ident idx l = do
    symTbl <- view symbolTable
    liftA2 (<|>) (lookupOf gsType (symTbl^.globals)   ident idx l)
                 (lookupOf csType (symTbl^.constants) ident idx l)

lookupTypeLocal :: ( Applicative m
                   , MonadReader Env m
                   , MonadEither Error m
                   )
                => Ident
                -> Maybe LExpr
                -> SrcLoc
                -> m (Maybe Type)
lookupTypeLocal ident idx l = view scope >>= \case
    Local ctx -> lookupTypeLocalIn ctx ident idx l
    Global    -> return Nothing

lookupTypeLocalIn :: ( Applicative m
                     , MonadReader r m
                     , MonadEither Error m
                     )
                  => FeatureContext
                  -> Ident
                  -> Maybe LExpr
                  -> SrcLoc
                  -> m (Maybe Type)
lookupTypeLocalIn ctx = lookupOf vsType (ctx^.to thisFeature.fsVars)

lookupOf :: (Applicative m, MonadEither Error m)
         => Getter a Type
         -> Table a
         -> Ident
         -> Maybe LExpr
         -> SrcLoc
         -> m (Maybe Type)
lookupOf g tbl ident idx l =
    forOf _Just (tbl^?at ident._Just.g) $ \t -> case t of
        CompoundType (ArrayType _ st) -- TODO: check array bounds
          | isJust idx -> return (SimpleType st)
          | otherwise  -> return t
        SimpleType _
          | isJust idx -> throw l $ NotAnArray ident
          | otherwise  -> return t

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
            match ident idx (thisFeature ctx) >>= \case
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

