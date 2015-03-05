{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Template
  ( Template(..)

  , prepModuleBody
  , prepExprs
  , prepExpr

  , unrollRepeatable
  , unrollLoopExprs

  , expandFormulas

  , instantiateWithId
  , instantiate
  , substitute
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Traversable

import Error
import Symbols
import Syntax
import Syntax.Util
import Typechecker

class Template n where
    parameters :: n a -> [Ident]

prepModuleBody :: ( Applicative m
                  , MonadReader r m
                  , MonadError Error m
                  , HasSymbolTable r
                  , HasScope r
                  )
               => LModuleBody
               -> m LModuleBody
prepModuleBody (ModuleBody decls stmts a) =
    ModuleBody <$> traverse prepExprs decls
               <*> prepRepeatable prepStmt stmts
               <*> pure a

prepStmt :: ( Applicative m
            , MonadReader r m
            , MonadError Error m
            , HasSymbolTable r
            , HasScope r
            )
         => LStmt
         -> m LStmt
prepStmt (Stmt action grd upds a) =
    Stmt <$> prepExprs action
         <*> prepExpr grd
         <*> prepRepeatable prepUpdate upds
         <*> pure a

prepUpdate :: ( Applicative m
              , MonadReader r m
              , MonadError Error m
              , HasSymbolTable r
              , HasScope r
              )
           => LUpdate
           -> m LUpdate
prepUpdate (Update e asgns a) =
    Update <$> traverse prepExpr e
           <*> prepRepeatable prepExprs asgns
           <*> pure a

prepExpr :: ( Applicative m
            , MonadReader r m
            , MonadError Error m
            , HasSymbolTable r
            , HasScope r
            )
         => LExpr
         -> m LExpr
prepExpr = prepExprs

prepExprs :: ( Applicative m
             , MonadReader r m
             , MonadError Error m
             , HasSymbolTable r
             , HasScope r
             , HasExprs n
             )
          => n SrcLoc
          -> m (n SrcLoc)
prepExprs n = do
    frms <- view formulas
    exprs (expandFormulas frms >=> unrollLoopExprs) n

prepRepeatable :: ( Applicative m
                  , MonadReader r m
                  , MonadError Error m
                  , HasSymbolTable r
                  , HasScope r
                  , HasExprs b
                  )
               => (b SrcLoc -> m (b SrcLoc))
               -> LRepeatable b
               -> m (LRepeatable b)
prepRepeatable prep r = do
    Repeatable ss <- unrollRepeatable r
    fmap Repeatable . for ss $ \(One x) ->
        One <$> prep x

unrollRepeatable :: ( Applicative m
                    , MonadReader r m
                    , MonadError Error m
                    , HasSymbolTable r
                    , HasScope r
                    , HasExprs b
                    )
                 => LRepeatable b
                 -> m (LRepeatable b)
unrollRepeatable (Repeatable ss) = Repeatable <$> rewriteM f ss where
    f (s:ss') = case s of
        One _     -> return Nothing
        Many loop -> Just . (++ ss') <$> unrollRepeatableLoop loop
    f [] = return Nothing

unrollRepeatableLoop :: ( Applicative m
                        , MonadReader r m
                        , MonadError Error m
                        , HasSymbolTable r
                        , HasScope r
                        , HasExprs b
                        )
                     => LForLoop (Repeatable b)
                     -> m [Some b SrcLoc]
unrollRepeatableLoop = unrollLoop f where
    f (Repeatable ss) = return . concatMap (\defs -> map (substitute defs) ss)

unrollLoopExprs :: ( Applicative m
                   , MonadReader r m
                   , MonadError Error m
                   , HasSymbolTable r
                   , HasScope r
                   )
                => LExpr
                -> m LExpr
unrollLoopExprs = rewriteM' $ \case
    LoopExpr loop _ -> Just <$> unrollExprLoop loop
    _               -> return Nothing

unrollExprLoop :: ( Applicative m
                  , MonadReader r m
                  , MonadError Error m
                  , HasSymbolTable r
                  , HasScope r
                  )
               => LForLoop Expr
               -> m LExpr
unrollExprLoop = unrollLoop f where
    f e defss = do
        checkLoopBody e
        flip (transformMOf plateBody) e $ \case
            BinaryExpr binOp e' (MissingExpr _) l ->
                maybe (throw l $ NoNeutralElement binOp) return $
                    unrollExpr (binaryExpr binOp) (neutralElement binOp) defss e'
            CallExpr func [e', MissingExpr _] l ->
                maybe (throw l $ NoNeutralElementFunc func) return $
                    unrollExpr (funcExpr func) Nothing defss e'
            e' -> return e'
    funcExpr func e1 e2 = CallExpr func [e1, e2] noLoc

unrollExpr :: (LExpr -> LExpr -> LExpr)
           -> Maybe LExpr
           -> [Map Ident LExpr]
           -> LExpr
           -> Maybe LExpr
unrollExpr combinator onEmpty defss e = case defss of
    [] -> onEmpty
    _  -> Just . foldr1 combinator . map (`substitute` e) $ defss

unrollLoop :: ( Applicative m
              , MonadReader r m
              , MonadError Error m
              , HasSymbolTable r
              , HasScope r
              )
           => (a SrcLoc -> [Map Ident LExpr] -> m b)
           -> LForLoop a
           -> m b
unrollLoop f (ForLoop ident range body _) = do
    (lower, upper) <- evalRange =<< both prepExpr range
    f body (map (Map.singleton ident . flip IntegerExpr noLoc) [lower .. upper])

expandFormulas :: (Applicative m, HasExprs n, MonadError Error m)
               => Map Ident LFormula
               -> n SrcLoc
               -> m (n SrcLoc)
expandFormulas frms = exprs . rewriteM' $ \case
    CallExpr (viewIdentExpr -> Just ident) args l -> call ident args l
    NameExpr (viewIdent -> Just ident) l          -> call ident [] l
    _ -> return Nothing
  where
    call ident args l =
        _Just (fmap frmExpr . instantiate ident args l) (frms^.at ident)

instantiateWithId :: (Template n, HasExprs n, MonadError Error m)
                  => Integer
                  -> Ident    -- ^ the template name
                  -> [LExpr]  -- ^ the argument list
                  -> SrcLoc   -- ^ 'SrcLoc' where the instantiation happens, required
                              --   for error reporting
                  -> n SrcLoc -- ^ the template
                  -> m (n SrcLoc)
instantiateWithId i ident args l =
    let idDef = Map.singleton "id" $ IntegerExpr i noLoc
    in instantiate ident args l . substitute idDef

instantiate :: (Template n, HasExprs n, MonadError Error m)
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
    NameExpr (viewIdent -> Just ident) l ->
        maybe node (fmap (reLoc l)) $ defs^.at ident
    _ -> node

-- | Check whether the given expression contains exactly one expression of
-- the form @e * ...@ (where @*@ is any binary operator) or @f(e, ...)@.
checkLoopBody :: (Applicative m, MonadError Error m) => LExpr -> m ()
checkLoopBody e = go e >>= \cnt ->
    when (cnt /= 1) (throw (exprAnnot e) MalformedLoopBody)
  where
    go e' = case e' of
        BinaryExpr _ lhs (MissingExpr _) _
          | hasMissingExpr lhs -> throw (exprAnnot lhs) MalformedLoopBody
          | otherwise          -> return (1 :: Integer)
        CallExpr _ [e'', MissingExpr _] _
          | hasMissingExpr e'' -> throw (exprAnnot e'') MalformedLoopBody
          | otherwise          -> return 1
        MissingExpr _ -> throw (exprAnnot e') MalformedLoopBody
        _             -> sum <$> traverse go (e'^..plateBody)
    hasMissingExpr = has (traverse._MissingExpr) . universeOf plateBody

-- Traversal of the immediate children of the given expression, ommitting
-- the body of nested 'LoopExpr's.
plateBody :: Traversal' (Expr a) (Expr a)
plateBody f e = case e of
    LoopExpr (ForLoop ident range body a) a' ->
        LoopExpr <$> (ForLoop ident <$> both f range <*> pure body <*> pure a)
                 <*> pure a'
    _ -> plate f e

-- Rewrite by applying the monadic rule everywhere you can in a top-down
-- manner. Ensures that the rule cannot be applied anywhere in the result.
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

