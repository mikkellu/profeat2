{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Error
  ( module Control.Monad.Except

  , Error(..)
  , ErrorDesc(..)
  , throw
  ) where

import Control.Monad.Except

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
throw :: (MonadError Error m) => SrcLoc -> ErrorDesc -> m a
throw l = throwError . Error l

data ErrorDesc
  = SyntaxError !Text
  | InvalidConstraint
  | UndefinedIdentifier !Ident
  | MultipleDeclarations !Ident !SrcLoc
  | CyclicDependency !Ident
  | ArityError Ident !Int !Int -- expected actual
  | TypeMismatch [Type] !Type !LExpr
  | NotApplicable !BinOp !Type !Type
  | NotAFunction !LExpr
  | StandaloneFuntion !Function
  | StandaloneMissingExpr
  | UnknownValues LExpr [LName]
  | NonConstExpr LExpr
  | MalformedLoopBody
  | NoNeutralElement !BinOp
  | AmbiguousDecomposition !Ident
  | AmbiguousIdentifier !Ident
  | AmbiguousFeature !Ident [Ident]
  | InvalidFeatureCardinality !Integer
  | MissingIndex !Ident
  | IndexOutOfBounds !(Integer, Integer) !Integer
  | NotAnArray !Ident
  | NotAVariable !Ident
  | NotAMember !Ident !Ident
  | NotAFeature !Ident
  | NonLocalThis
  | IllegalConstAssignment
  | IllegalWriteAccess
  | IllegalMandatoryReconf
  | IllegalReconf
  | IllegalReconfLabel
  | DivisionByZero Valuation
  deriving (Show)

instance Pretty ErrorDesc where
    pretty desc = case desc of
        SyntaxError msg -> string msg
        InvalidConstraint -> "invalid constraint"
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
        StandaloneMissingExpr ->
            "expansion operator '...' used outside of for loop"
        UnknownValues e names ->
            "the expression" <+> pretty e <+> "cannot be evaluated as the" <+>
            "values of the following variables is not known:" <> line <>
            sep (punctuate comma $ map pretty names)
        NonConstExpr e ->
            "the expression" <+> pretty e <+> "is not constant"
        MalformedLoopBody ->
            "malformed loop"
        NoNeutralElement binOpT ->
            "the loop generates an empty expression," <> line <> "but the" <+>
            pretty binOpT <+> "operator does not have a neutral element"
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
        NotAFeature ident ->
            text ident <+> "is not a feature"
        NonLocalThis ->
            "the 'this' keyword is not defined in this context"
        IllegalConstAssignment ->
            "illegal assignment to constant"
        IllegalWriteAccess ->
            "illegal write access"
        IllegalMandatoryReconf ->
            "illegal reconfiguration of a mandatory feature"
        IllegalReconf ->
            "illegal reconfiguration outside of controller"
        IllegalReconfLabel ->
            "illegal reconfiguration label"
        DivisionByZero val ->
            "division by zero with valuation" <+> prettyValuation val

