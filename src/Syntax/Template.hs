{-# LANGUAGE FlexibleContexts #-}

module Syntax.Template
  ( expandFormulas
  , instantiate
  , substitute
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
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
expandFormulas frms = exprs (rewriteM expand)
  where
    expand e = case e of
        NameExpr (Name ident) l ->
            case frms^.at ident of
                Just f -> do
                    let paramCount = length (frmParams f)
                    unless (paramCount == 0) . throw l $
                        ArityError ident paramCount 0
                    return . Just $ frmExpr f
                Nothing -> return Nothing
        FuncExpr (Func ident) args l ->
            case frms^.at ident of
                Just f -> do
                    f' <- instantiate f ident args l
                    return . Just $ frmExpr f'
                Nothing -> return Nothing
        _ -> return Nothing

instantiate :: (Template n, HasExprs n, MonadEither Error m)
            => n SrcLoc -- ^ the template
            -> Ident    -- ^ the template name
            -> [LExpr]  -- ^ the argument list
            -> SrcLoc   -- ^ 'SrcLoc' where the instantiation happens, required
                        --   for error reporting
            -> m (n SrcLoc)
instantiate template ident args l =
    let params     = parameters template
        paramCount = length params
        argCount   = length args
        defs       = Map.fromList $ zip params args
    in if argCount /= paramCount
           then throw l $ ArityError ident paramCount argCount
           else return $ substitute defs template

substitute :: (HasExprs n) => Map Ident LExpr -> n SrcLoc -> n SrcLoc
substitute defs = over exprs . transform $ \node -> case node of
    NameExpr (Name ident) l ->
        maybe node (fmap (reLoc l)) $ defs^.at ident
    _ -> node

instance Template Feature where
    parameters = featParams

instance Template Module where
    parameters = modParams

instance Template Formula where
    parameters = frmParams

