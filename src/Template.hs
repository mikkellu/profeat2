{-# LANGUAGE FlexibleContexts #-}

module Template
  ( unrollRepeatable
  , unrollLoopExprs
  , expandFormulas
  , instantiate
  , substitute
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Either
import Control.Monad.Reader

import Data.Map ( Map )
import qualified Data.Map as Map

import Error
import Eval
import SrcLoc
import Symbols
import Syntax
import Typechecker
import Types

class Template n where
    parameters :: n a -> [Ident]

unrollRepeatable :: ( Applicative m
                    , MonadReader SymbolTable m
                    , MonadEither Error m
                    , HasExprs b
                    )
                 => Valuation
                 -> LRepeatable b
                 -> m (LRepeatable b)
unrollRepeatable val (Repeatable ss) = Repeatable <$> rewriteM f ss where
    f (s:ss') = case s of
        One _     -> return Nothing
        Many loop -> Just . (++ ss') <$> unrollRepeatableLoop val loop
    f [] = return Nothing

unrollRepeatableLoop :: ( Applicative m
                        , MonadReader SymbolTable m
                        , MonadEither Error m
                        , HasExprs b
                        )
                     => Valuation
                     -> LForLoop (Repeatable b)
                     -> m [Some b SrcLoc]
unrollRepeatableLoop = unrollLoop f where
    f (Repeatable ss) = return . concatMap (\defs -> map (substitute defs) ss)

unrollLoopExprs :: ( Applicative m
                   , MonadReader SymbolTable m
                   , MonadEither Error m
                   )
                => Valuation
                -> LExpr
                -> m LExpr
unrollLoopExprs val = rewriteM' f where
    f e = case e of
        LoopExpr loop _ -> Just <$> unrollExprLoop val loop
        _               -> return Nothing

unrollExprLoop :: ( Applicative m
                  , MonadReader SymbolTable m
                  , MonadEither Error m
                  )
               => Valuation
               -> LForLoop Expr
               -> m LExpr
unrollExprLoop = unrollLoop f where
    f e defss = do
        checkLoopBody e
        return $ transformOf plateBody (unrollExpr defss) e

unrollExpr :: [Map Ident LExpr] -> LExpr -> LExpr
unrollExpr defss e = case e of
    BinaryExpr binOp e' (MissingExpr _) _ ->
        let e's = map (`substitute` e') defss
        in foldr1 (binaryExpr binOp) e's
    _ -> e

unrollLoop :: (Applicative m, MonadReader SymbolTable m, MonadEither Error m)
           => (a SrcLoc -> [Map Ident LExpr] -> m b)
           -> Valuation
           -> LForLoop a
           -> m b
unrollLoop f val (ForLoop ident range body _) = do
    range' <- both (unrollLoopExprs val) range

    _ <- both (checkIfType_ isIntType) range'
    _ <- runReaderT (both checkIfConst range') val

    (IntVal lower, IntVal upper) <- both (eval' val) range'

    let is | lower <= upper = [lower .. upper]
           | otherwise      = [lower, lower - 1 .. upper]

    f body (map (Map.singleton ident . flip IntegerExpr noLoc) is)

expandFormulas :: (Applicative m, HasExprs n, MonadEither Error m)
               => Map Ident LFormula
               -> n SrcLoc
               -> m (n SrcLoc)
expandFormulas frms = exprs (rewriteM' expand)
  where
    expand e = case e of
        CallExpr (NameExpr (Name ident) _) args l -> call ident args l
        NameExpr (Name ident) l                   -> call ident [] l
        _                                         -> return Nothing

    call ident args l =
        _Just (fmap frmExpr . instantiate ident args l) (frms^.at ident)

instantiate :: (Template n, HasExprs n, MonadEither Error m)
            => Ident    -- ^ the template name
            -> [LExpr]  -- ^ the argument list
            -> SrcLoc   -- ^ 'SrcLoc' where the instantiation happens, required
                        --   for error reporting
            -> n SrcLoc -- ^ the template
            -> m (n SrcLoc)
instantiate ident args l template =
    let params     = parameters template
        paramCount = length params
        argCount   = length args
        defs       = Map.fromList $ zip params args
    in if argCount /= paramCount
           then throw l $ ArityError ident paramCount argCount
           else return $ substitute defs template

substitute :: (HasExprs n) => Map Ident LExpr -> n SrcLoc -> n SrcLoc
substitute defs
  | Map.null defs = id
  | otherwise     = over exprs . transform $ \node -> case node of
    NameExpr (Name ident) l ->
        maybe node (fmap (reLoc l)) $ defs^.at ident
    _ -> node

-- | Check whether the given expression contains exactly one expression of
-- the form @e * ...@, where @*@ is any binary operator.
checkLoopBody :: (Functor m, MonadEither Error m) => LExpr -> m ()
checkLoopBody e = go e >>= \cnt ->
    when (cnt /= 1) (throw (exprAnnot e) MalformedLoopBody)
  where
    go :: (Functor m, MonadEither Error m) => LExpr -> m Integer
    go e' = case e' of
        BinaryExpr _ lhs (MissingExpr _) _
          | has (traverse._MissingExpr) $ universeOf plateBody lhs ->
                throw (exprAnnot lhs) MalformedLoopBody
          | otherwise -> return 1
        MissingExpr _ -> throw (exprAnnot e') MalformedLoopBody
        _             -> sum <$> mapM go (e'^..plateBody)

-- Traversal of the immediate children of the given expression, ommitting
-- the body of nested 'LoopExpr's.
plateBody :: Traversal' (Expr a) (Expr a)
plateBody f e = case e of
    LoopExpr (ForLoop ident range body a) a' ->
        LoopExpr <$> (ForLoop ident <$> both f range <*> pure body <*> pure a)
                 <*> pure a'
    _ -> plate f e

-- Rewrite by applying the monadic everywhere you can in a top-down manner.
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteM' :: (Monad m, Applicative m, Plated a)
          => (a -> m (Maybe a))
          -> a
          -> m a
rewriteM' = rewriteMOf' plate

rewriteMOf' :: (Monad m) => LensLike' m a a -> (a -> m (Maybe a)) -> a -> m a
rewriteMOf' l f = go where
    go e = f e >>= maybe (l go e) go

instance Template Feature where
    parameters = featParams

instance Template Module where
    parameters = modParams

instance Template Formula where
    parameters = frmParams

