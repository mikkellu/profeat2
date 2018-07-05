{-# LANGUAGE FlexibleContexts, OverloadedStrings, ViewPatterns #-}

-- | The Eval module provides the 'eval' function that allows to evaluate
-- 'Expr'essions under a given 'Valuation'.
--
-- The 'eval' function assumes that the expression contains no type errors
-- and that the 'Valuation' provides a 'Value' for each used identifier. If
-- any of these assumptions are violated 'eval' will crash.

module Eval
  ( Value(..)
  , Valuation
  , prettyValuation

  , eval
  , eval'
  ) where

import Control.Monad.Reader

import Data.Map ( (!) )

import Error
import Syntax
import Syntax.Util
import Types

-- | Evaluates an expression under a given variable valuation. If
-- a division by zero is encountered 'Nothing' is returned.
eval :: Valuation -> Expr a -> Maybe Value
eval val e = runReaderT (evalImpl e) val

-- | Like 'eval', but throws a 'DivisionByZero' error on division by zero.
eval' :: (MonadError Error m) => Valuation -> LExpr -> m Value
eval' val e = case eval val e of
    Just v  -> return v
    Nothing -> throw (exprAnnot e) $ DivisionByZero val

evalImpl :: Expr a -> ReaderT Valuation Maybe Value
evalImpl (BinaryExpr binOp l r _) = do
    vl <- evalImpl l; vr <- evalImpl r

    case binOp of
        ArithBinOp Div -> case vr of
            IntVal 0   -> lift Nothing
            DblVal 0.0 -> lift Nothing
            _          -> return ()
        _              -> return ()

    return $ case (binOp, vl, vr) of
        (ArithBinOp Add  , IntVal l' , IntVal r' ) -> IntVal $ l' + r'
        (ArithBinOp Add  , DblVal l' , IntVal r' ) -> DblVal $ l' + fromInteger r'
        (ArithBinOp Add  , IntVal l' , DblVal r' ) -> DblVal $ fromInteger l' + r'
        (ArithBinOp Add  , DblVal l' , DblVal r' ) -> DblVal $ l' + r'
        (ArithBinOp Sub  , IntVal l' , IntVal r' ) -> IntVal $ l' - r'
        (ArithBinOp Sub  , DblVal l' , IntVal r' ) -> DblVal $ l' - fromInteger r'
        (ArithBinOp Sub  , IntVal l' , DblVal r' ) -> DblVal $ fromInteger l' - r'
        (ArithBinOp Sub  , DblVal l' , DblVal r' ) -> DblVal $ l' - r'
        (ArithBinOp Mul  , IntVal l' , IntVal r' ) -> IntVal $ l' * r'
        (ArithBinOp Mul  , DblVal l' , IntVal r' ) -> DblVal $ l' * fromInteger r'
        (ArithBinOp Mul  , IntVal l' , DblVal r' ) -> DblVal $ fromInteger l' * r'
        (ArithBinOp Mul  , DblVal l' , DblVal r' ) -> DblVal $ l' * r'
        (ArithBinOp Div  , IntVal l' , IntVal r' ) -> DblVal $ fromInteger l' / fromInteger r'
        (ArithBinOp Div  , DblVal l' , IntVal r' ) -> DblVal $ l' / fromInteger r'
        (ArithBinOp Div  , IntVal l' , DblVal r' ) -> DblVal $ fromInteger l' / r'
        (ArithBinOp Div  , DblVal l' , DblVal r' ) -> DblVal $ l' / r'

        (EqBinOp Eq      , BoolVal l', BoolVal r') -> BoolVal $ l' == r'
        (EqBinOp Eq      , IntVal l' , IntVal r' ) -> BoolVal $ l' == r'
        (EqBinOp Eq      , IntVal l' , DblVal r' ) -> BoolVal $ fromInteger l' == r'
        (EqBinOp Eq      , DblVal l' , IntVal r' ) -> BoolVal $ l' == fromInteger r'
        (EqBinOp Eq      , DblVal l' , DblVal r' ) -> BoolVal $ l' == r'
        (EqBinOp Neq     , BoolVal l', BoolVal r') -> BoolVal $ l' /= r'
        (EqBinOp Neq     , IntVal l' , IntVal r' ) -> BoolVal $ l' /= r'
        (EqBinOp Neq     , IntVal l' , DblVal r' ) -> BoolVal $ fromInteger l' /= r'
        (EqBinOp Neq     , DblVal l' , IntVal r' ) -> BoolVal $ l' /= fromInteger r'
        (EqBinOp Neq     , DblVal l' , DblVal r' ) -> BoolVal $ l' /= r'

        (RelBinOp Lt     , IntVal l' , IntVal r' ) -> BoolVal $ l' < r'
        (RelBinOp Lt     , DblVal l' , IntVal r' ) -> BoolVal $ l' < fromInteger r'
        (RelBinOp Lt     , IntVal l' , DblVal r' ) -> BoolVal $ fromInteger l' < r'
        (RelBinOp Lt     , DblVal l' , DblVal r' ) -> BoolVal $ l' < r'
        (RelBinOp Gt     , IntVal l' , IntVal r' ) -> BoolVal $ l' > r'
        (RelBinOp Gt     , DblVal l' , IntVal r' ) -> BoolVal $ l' > fromInteger r'
        (RelBinOp Gt     , IntVal l' , DblVal r' ) -> BoolVal $ fromInteger l' > r'
        (RelBinOp Gt     , DblVal l' , DblVal r' ) -> BoolVal $ l' > r'
        (RelBinOp Lte    , IntVal l' , IntVal r' ) -> BoolVal $ l' <= r'
        (RelBinOp Lte    , DblVal l' , IntVal r' ) -> BoolVal $ l' <= fromInteger r'
        (RelBinOp Lte    , IntVal l' , DblVal r' ) -> BoolVal $ fromInteger l' <= r'
        (RelBinOp Lte    , DblVal l' , DblVal r' ) -> BoolVal $ l' <= r'
        (RelBinOp Gte    , IntVal l' , IntVal r' ) -> BoolVal $ l' >= r'
        (RelBinOp Gte    , DblVal l' , IntVal r' ) -> BoolVal $ l' >= fromInteger r'
        (RelBinOp Gte    , IntVal l' , DblVal r' ) -> BoolVal $ fromInteger l' >= r'
        (RelBinOp Gte    , DblVal l' , DblVal r' ) -> BoolVal $ l' >= r'

        (LogicBinOp LImpl, BoolVal l', BoolVal r') -> BoolVal $ not l' || r'
        (LogicBinOp LEq  , BoolVal l', BoolVal r') -> BoolVal $ l' == r'
        (LogicBinOp LAnd , BoolVal l', BoolVal r') -> BoolVal $ l' && r'
        (LogicBinOp LOr  , BoolVal l', BoolVal r') -> BoolVal $ l' || r'

        _                                         -> typeError

evalImpl (UnaryExpr (ArithUnOp Neg) e' _) = do
    v <- evalImpl e'
    return $ case v of
        IntVal i -> IntVal $ negate i
        DblVal d -> DblVal $ negate d
        _        -> typeError
evalImpl (UnaryExpr (LogicUnOp LNot) e' _) = do
    v <- evalImpl e'
    case v of
        BoolVal b -> return . BoolVal $ not b
        _         -> typeError
evalImpl UnaryExpr {} = error "Eval.eval: illegal operator"

evalImpl (CondExpr c t e _) = do
    cv <- evalImpl c
    case cv of
        BoolVal True  -> evalImpl t
        BoolVal False -> evalImpl e
        _             -> typeError

evalImpl (LoopExpr _ _) = error "Eval.eval: unresolved LoopExpr"

evalImpl (CallExpr (FuncExpr function _) args _) = do
    vs@(v:vs') <- traverse evalImpl args
    return $ case function of
        FuncMin
          | any isDblVal vs -> DblVal . minimum $ map toDouble vs
          | otherwise       -> IntVal . minimum $ map toInt vs
        FuncMax
          | any isDblVal vs -> DblVal . maximum $ map toDouble vs
          | otherwise       -> IntVal . maximum $ map toInt vs
        FuncFloor -> IntVal . floor   $ toDouble v
        FuncCeil  -> IntVal . ceiling $ toDouble v
        FuncPow ->
          let r = toDouble v ** toDouble (head vs')
          in if any isDblVal vs then DblVal r else IntVal $ truncate r
        FuncMod     -> IntVal $ toInt v `mod` toInt (head vs')
        FuncLog     -> DblVal $ toDouble v `logBase` toDouble (head vs')
        FuncActive  -> error "Eval.eval: active-function"
        FuncIActive -> error "Eval.eval: iactive-function"
        FuncBinom   -> error "Eval.eval: binom-function"
  where
    isDblVal (DblVal _) = True
    isDblVal _          = False

    toInt (IntVal i) = i
    toInt _          = typeError

    toDouble (IntVal i) = fromInteger i
    toDouble (DblVal d) = d
    toDouble _          = typeError

evalImpl CallExpr {}       = typeError
evalImpl (NameExpr (viewSimpleName -> Just (ident, idx, _)) _) = case idx of
    Just e  -> do
        IntVal i <- evalImpl e
        (! (ident, i)) <$> ask
    Nothing -> (! (ident, 0)) <$> ask
evalImpl NameExpr {}        = error "Eval.eval: illegal name"
evalImpl (FuncExpr _ _)     = typeError
evalImpl SampleExpr {}      = error "Eval.eval: SampleExpr"
evalImpl FilterExpr {}      = error "Eval.eval: FilterExpr"
evalImpl ProbExpr {}        = error "Eval.eval: ProbExpr"
evalImpl SteadyExpr {}      = error "Eval.eval: SteadyExpr"
evalImpl RewardExpr {}      = error "Eval.eval: RewardExpr"
evalImpl ConditionalExpr {} = error "Eval.eval: ConditionalExpr"
evalImpl QuantileExpr {}    = error "Eval.eval: QuantileExpr"
evalImpl LabelExpr {}       = error "Eval.eval: LabelExpr"
evalImpl ArrayExpr {}       = typeError
evalImpl (DecimalExpr d _)  = return $ DblVal d
evalImpl (IntegerExpr i _)  = return $ IntVal i
evalImpl (BoolExpr b _)     = return $ BoolVal b
evalImpl (MissingExpr _)    = typeError

typeError :: a
typeError = error "Eval.eval': type error"

