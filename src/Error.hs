{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Error
  ( Error(..)
  , ErrorDesc(..)
  , throw
  ) where

import Control.Monad.Either

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import Syntax
import Types

-- | Represents translation errors. It provides an error description and the
-- source location.
data Error = Error !SrcLoc !ErrorDesc deriving (Show)

instance Pretty Error where
    pretty (Error l desc) = pretty l <> colon <> line <> pretty desc

-- | Throws an 'Error'.
throw :: (MonadEither Error m) => SrcLoc -> ErrorDesc -> m a
throw l = left . Error l

data ErrorDesc
  = SyntaxError !Text
  | UndefinedIdentifier !Ident
  | MultipleDeclarations !Ident !SrcLoc
  | CyclicDependency !Ident
  | ArityError Ident !Int !Int -- expected actual
  | TypeMismatch [Type] !Type !LExpr
  | NotApplicable !BinOp !Type !Type
  | NotAFunction !LExpr
  | StandaloneFuntion !Function
  | UnknownValues LExpr [LName]
  | MalformedLoopBody
  | DivisionByZero Valuation
  deriving (Show)

instance Pretty ErrorDesc where
    pretty desc = case desc of
        SyntaxError msg -> string msg
        UndefinedIdentifier ident -> "undefined identifier" <+> text ident
        MultipleDeclarations ident l ->
            "multiple declarations of" <+> text ident <+> parens (
            "first declaration was at" <+> pretty l)
        CyclicDependency ident ->
            "cyclic dependency in the definition of" <+> text ident
        ArityError ident expected actual ->
            text ident <+> "should have" <+> int expected <+> "arguments" <+>
            "but has been given" <+> int actual
        TypeMismatch expected actual e ->
            "type mismatch" <> line <>
            "expression" <+> pretty e <> line <>
            "has type" <> colon <+> pretty actual <> line <>
            "expected" <> colon <+>
            sep (punctuate (space <> "or") $ map pretty expected)
        NotApplicable binOpT tl tr ->
            "operator" <+> pretty binOpT <+>
            "is not applicable for expressions of type" <+> pretty tl <+>
            "and" <+> pretty tr
        NotAFunction e ->
            pretty e <+> "is not a function"
        StandaloneFuntion f ->
            pretty f <+> "is a function, but no arguments are given"
        UnknownValues e names ->
            "the expression" <+> pretty e <+> "cannot be evaluated as the" <+>
            "values of the following variables is not known:" <> line <>
            sep (punctuate comma $ map pretty names)
        MalformedLoopBody ->
            "malformed loop"
        DivisionByZero val ->
            "division by zero with valuation" <+> prettyValuation val

