module Parser
  ( parseModel
  ) where

import Data.Text.Lazy ( Text )

import Text.Parsec ( SourceName )

import Error
import Syntax

import Parser.Internal

parseModel :: SourceName -> Text -> Either Error LModel
parseModel = parseFile model

