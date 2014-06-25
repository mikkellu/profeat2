{-# LANGUAGE FlexibleContexts #-}

module Syntax.Template
  ( expandFormulas
  , instantiate
  , substitute
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Either

import Data.Map ( Map )
import qualified Data.Map as Map

import Error
import SrcLoc
import Syntax

class Template n where
    parameters :: n a -> [Ident]

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

-- Rewrite by applying the monadic everywhere you can in a top-down manner.
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteM' :: (Monad m, Applicative m, Plated a)
          => (a -> m (Maybe a))
          -> a
          -> m a
rewriteM' f = go
  where
    go e = f e >>= maybe (plate go e) go

instance Template Feature where
    parameters = featParams

instance Template Module where
    parameters = modParams

instance Template Formula where
    parameters = frmParams

