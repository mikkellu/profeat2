{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Error
  ( module Control.Monad.Either

  , Error(..)
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
  | AmbiguousDecomposition !Ident
  | AmbiguousIdentifier !Ident
  | AmbiguousFeature !Ident [Ident]
  | InvalidFeatureCardinality !Integer
  | MissingIndex !Ident
  | IndexOutOfBounds !(Integer, Integer) !Integer
  | NotAnArray !Ident
  | NotAVariable !Ident
  | NotAMember !Ident !Ident
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
        AmbiguousDecomposition ident ->
            "the feature" <+> text ident <+> "is referenced more than once"
        AmbiguousIdentifier ident ->
            text ident <+> "is defined by more than one referenced module"
        AmbiguousFeature ident idents ->
            "the name" <+> text ident <+> "is ambigouos" <> line <>
            "possible candidates are:" <> line <>
            vsep (map text idents)
        InvalidFeatureCardinality i ->
            "feature cardinality must be greater than zero" <+>
            parens ("given:" <+> integer i)
        MissingIndex ident ->
            text ident <+> "is a multi-feature, but no index is given"
        IndexOutOfBounds range actual ->
            "index out of bounds" <> line <>
            "should be in:" <+> prettyRange range <> line <>
            "given:" <+> integer actual
        NotAnArray ident ->
            text ident <+> "is not an array"
        NotAVariable ident ->
            text ident <+> "is used as a variable, but it is a feature"
        NotAMember fIdent ident ->
            text ident <+> "is not a member of" <+> text fIdent
        DivisionByZero val ->
            "division by zero with valuation" <+> prettyValuation val

