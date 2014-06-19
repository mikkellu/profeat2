module Syntax.Template
  ( substitute
  ) where

import Control.Lens

import Data.Map ( Map )

import SrcLoc
import Syntax

substitute :: (HasExprs n) => Map Ident LExpr -> n SrcLoc -> n SrcLoc
substitute defs = over exprs . transform $ \node -> case node of
    NameExpr (Name (BaseName ident [] l)) _ ->
        maybe node (fmap (reLoc l)) $ defs^.at ident
    _ -> node

