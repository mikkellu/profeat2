{-# LANGUAGE FlexibleContexts #-}

module Syntax.Template
  ( instantiate
  , substitute
  ) where

import Control.Lens
import Control.Monad.Either

import Data.Map ( Map )
import qualified Data.Map as Map

import Error
import SrcLoc
import Syntax

class Template n where
    parameters :: n a -> [Ident]

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
    NameExpr (Name (BaseName ident [] l)) _ ->
        maybe node (fmap (reLoc l)) $ defs^.at ident
    _ -> node

instance Template Feature where
    parameters = featParams

instance Template Module where
    parameters = modParams

instance Template Formula where
    parameters = frmParams

