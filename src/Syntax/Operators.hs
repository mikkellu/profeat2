{-# LANGUAGE OverloadedStrings #-}

module Syntax.Operators
  ( BinOp(..)
  , ArithBinOp(..)
  , EqBinOp(..)
  , RelBinOp(..)
  , LogicBinOp(..)
  , binOpPrec

  , UnOp(..)
  , ArithUnOp(..)
  , LogicUnOp(..)
  , unOpPrec

  , callPrec
  ) where

import Text.PrettyPrint.Leijen.Text

data BinOp
  = ArithBinOp !ArithBinOp
  | EqBinOp !EqBinOp
  | RelBinOp !RelBinOp
  | LogicBinOp !LogicBinOp
  deriving (Eq, Show)

data ArithBinOp
  = Mul
  | Div
  | Add
  | Sub
  deriving (Bounded, Enum, Eq, Show)

data EqBinOp
  = Eq
  | Neq
  deriving (Bounded, Enum, Eq, Show)

data RelBinOp
  = Gt
  | Lt
  | Gte
  | Lte
  deriving (Bounded, Enum, Eq, Show)

data LogicBinOp
  = LImpl
  | LEq
  | LAnd
  | LOr
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Returns the precedence level of a function call.
callPrec :: Int
callPrec = 12

-- | Returns the precedence level of the given operator.
binOpPrec :: BinOp -> Int
binOpPrec binOpT = case binOpT of
    ArithBinOp binOp -> case binOp of
        Mul     -> 10
        Div     -> 10
        Add     -> 9
        Sub     -> 9
    EqBinOp _   -> 8
    RelBinOp _  -> 8
    LogicBinOp binOp -> case binOp of
        LAnd    -> 6
        LOr     -> 5
        LImpl   -> 4
        LEq     -> 4

data UnOp
  = ArithUnOp !ArithUnOp
  | LogicUnOp !LogicUnOp
  deriving (Eq, Show)

data ArithUnOp
  = Neg
  deriving (Bounded, Enum, Eq, Show)

data LogicUnOp
 = LNot
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Returns the precedence level of the given operator.
unOpPrec :: UnOp -> Int
unOpPrec unOpT = case unOpT of
    ArithUnOp _  -> 11
    LogicUnOp _  -> 7

instance Pretty BinOp where
    pretty binOpT = case binOpT of
        ArithBinOp binOp -> pretty binOp
        EqBinOp    binOp -> pretty binOp
        RelBinOp   binOp -> pretty binOp
        LogicBinOp binOp -> pretty binOp

instance Pretty ArithBinOp where
    pretty binOp = case binOp of
        Div -> "/"
        Mul -> "*"
        Sub -> "-"
        Add -> "+"

instance Pretty EqBinOp where
    pretty binOp = case binOp of
        Eq  -> "="
        Neq -> "!="

instance Pretty RelBinOp where
    pretty binOp = case binOp of
        Gt  -> ">"
        Lt  -> "<"
        Gte -> ">="
        Lte -> "<="

instance Pretty LogicBinOp where
    pretty binOp = case binOp of
        LImpl -> "=>"
        LEq   -> "<=>"
        LAnd  -> "&"
        LOr   -> "|"

instance Pretty UnOp where
    pretty unOpT = case unOpT of
        ArithUnOp unOp     -> pretty unOp
        LogicUnOp unOp     -> pretty unOp

instance Pretty ArithUnOp where
    pretty unOp = case unOp of
        Neg -> "-"

instance Pretty LogicUnOp where
    pretty unOp = case unOp of
        LNot -> "!"
