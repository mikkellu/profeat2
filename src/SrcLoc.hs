{-# LANGUAGE OverloadedStrings #-}

module SrcLoc
  ( SrcLoc

  , srcLoc
  , noLoc
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

data SrcLoc
  = SrcLoc !Text !Int !Int
  | NoLoc
  deriving (Show)

srcLoc :: Text -> Int -> Int -> SrcLoc
srcLoc = SrcLoc

noLoc :: SrcLoc
noLoc = NoLoc

instance Pretty SrcLoc where
    pretty (SrcLoc file y x) =
        dquotes (text file) <+> parens (
            "line:" <+> int y <> comma <+> "column:" <+> int x)

    pretty NoLoc = "unknown location"
