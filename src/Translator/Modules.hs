module Translator.Modules
  ( trnsModules
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Map ( assocs )
import Data.Traversable

import Symbols
import Syntax

import Translator.Common
import Translator.Names

trnsModules :: Trans [LDefinition]
trnsModules = do
    root <- view rootFeature
    fmap concat . for (allContexts root) $ \ctx -> local (scope .~ Local ctx) $
        for (ctx^.this.fsModules.to assocs) $ \(ident, body) ->
            ModuleDef <$> trnsModule ident body

trnsModule :: Ident -> LModuleBody -> Trans LModule
trnsModule ident body = do
    Local ctx <- view scope
    Module (moduleIdent ctx ident) [] [] <$> trnsModuleBody body

