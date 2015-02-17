{-# LANGUAGE OverloadedStrings #-}

module SrcLoc
  ( SrcLoc

  , srcLoc
  , reLoc
  , noLoc
  ) where

import Data.Monoid    ( mappend )
import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

data SrcLoc
  = SrcLoc !Text !Int !Int
  | ReLoc !SrcLoc !SrcLoc -- old location, new location
  | NoLoc
  deriving (Eq, Show)

instance Ord SrcLoc where
    compare x y = case x of
        SrcLoc _ xLine xCol -> case y of
            SrcLoc _ yLine yCol -> compare xLine yLine `mappend`
                                   compare xCol yCol
            ReLoc _ y'          -> compare x y'
            NoLoc               -> GT
        ReLoc _ x' -> compare x' y
        NoLoc -> case y of
            NoLoc -> EQ
            _     -> LT

instance Pretty SrcLoc where
    pretty (SrcLoc file y x) =
        dquotes (text file) <+> parens (
            "line:" <+> int y <> comma <+> "column:" <+> int x)

    pretty (ReLoc oldLoc newLoc) =
        pretty newLoc <+> parens ("defined at:" <+> pretty oldLoc)

    pretty NoLoc = "unknown location"

srcLoc :: Text -> Int -> Int -> SrcLoc
srcLoc = SrcLoc

reLoc :: SrcLoc -> SrcLoc -> SrcLoc
reLoc newLoc (ReLoc oldLoc _) = ReLoc oldLoc newLoc
reLoc newLoc oldLoc           = ReLoc oldLoc newLoc

noLoc :: SrcLoc
noLoc = NoLoc

