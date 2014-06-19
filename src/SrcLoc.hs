{-# LANGUAGE OverloadedStrings #-}

module SrcLoc
  ( SrcLoc

  , srcLoc
  , reLoc
  , noLoc
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

data SrcLoc
  = SrcLoc !Text !Int !Int
  | ReLoc !SrcLoc !SrcLoc -- new location, old location
  | NoLoc
  deriving (Show)

srcLoc :: Text -> Int -> Int -> SrcLoc
srcLoc = SrcLoc

reLoc :: SrcLoc -> SrcLoc -> SrcLoc
reLoc (ReLoc _ newLoc) = ReLoc newLoc
reLoc newLoc           = ReLoc newLoc

noLoc :: SrcLoc
noLoc = NoLoc

instance Pretty SrcLoc where
    pretty (SrcLoc file y x) =
        dquotes (text file) <+> parens (
            "line:" <+> int y <> comma <+> "column:" <+> int x)

    pretty (ReLoc newLoc oldLoc) =
        pretty newLoc <+> parens ("defined at:" <+> pretty oldLoc)

    pretty NoLoc = "unknown location"
