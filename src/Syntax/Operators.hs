{-# LANGUAGE OverloadedStrings #-}

module Syntax.Operators
  ( BinOp(..)
  , ArithBinOp(..)
  , EqBinOp(..)
  , RelBinOp(..)
  , LogicBinOp(..)
  , TempBinOp(..)
  , binOpPrec

  , UnOp(..)
  , ArithUnOp(..)
  , LogicUnOp(..)
  , TempUnOp(..)
  , ProbUnOp(..)
  , Bound(..)
  , BoundOp(..)
  , QueryType(..)
  , unOpPrec

  , FilterOp(..)

  , callPrec
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

data BinOp
  = ArithBinOp !ArithBinOp
  | EqBinOp !EqBinOp
  | RelBinOp !RelBinOp
  | LogicBinOp !LogicBinOp
  | TempBinOp !TempBinOp
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

data TempBinOp
  = Until
  | WeakUntil
  | Release
  deriving (Bounded, Enum, Eq, Show)

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
    TempBinOp _ -> 3

data UnOp
  = ArithUnOp !ArithUnOp
  | LogicUnOp !LogicUnOp
  | TempUnOp !TempUnOp
  | ProbUnOp !ProbUnOp
  deriving (Eq, Show)

data ArithUnOp
  = Neg
  deriving (Bounded, Enum, Eq, Show)

data LogicUnOp
 = LNot
  deriving (Bounded, Enum, Eq, Ord, Show)

data TempUnOp
  = Next
  | Finally
  | Globally
  | Exists
  | Forall
  deriving (Bounded, Enum, Eq, Show)

data ProbUnOp
  = Prob Bound
  | Steady Bound
  deriving (Eq, Show)

data Bound
  = Bound !BoundOp !Text
  | Query !QueryType
  deriving (Eq, Show)

data BoundOp
  = BGt
  | BLt
  | BGte
  | BLte
  deriving (Bounded, Enum, Eq, Show)

data QueryType
  = QueryProb
  | QueryMinProb
  | QueryMaxProb
  deriving (Bounded, Enum, Eq, Show)

-- | Returns the precedence level of the given operator.
unOpPrec :: UnOp -> Int
unOpPrec unOpT = case unOpT of
    ArithUnOp _  -> 11
    LogicUnOp _  -> 7
    TempUnOp unOp -> case unOp of
        Next     -> 3
        Finally  -> 3
        Globally -> 3
        Exists   -> 2
        Forall   -> 2
    ProbUnOp _   -> 2

data FilterOp
 = FilterMin
 | FilterMax
 | FilterArgmin
 | FilterArgmax
 | FilterCount
 | FilterSum
 | FilterAvg
 | FilterFirst
 | FilterRange
 | FilterForall
 | FilterExists
 | FilterPrint
 | FilterPrintall
 | FilterState
 deriving (Eq, Show)

instance Pretty BinOp where
    pretty binOpT = case binOpT of
        ArithBinOp binOp -> pretty binOp
        EqBinOp    binOp -> pretty binOp
        RelBinOp   binOp -> pretty binOp
        LogicBinOp binOp -> pretty binOp
        TempBinOp  binOp -> pretty binOp

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

instance Pretty TempBinOp where
    pretty binOp = case binOp of
        Until     -> "U"
        WeakUntil -> "W"
        Release   -> "R"

instance Pretty UnOp where
    pretty unOpT = case unOpT of
        ArithUnOp unOp     -> pretty unOp
        LogicUnOp unOp     -> pretty unOp
        TempUnOp  unOp     -> pretty unOp
        _                  -> error $ "Ast.hs: unmatched operator" ++ show unOpT -- Prob and Steady are handled in Pretty instance for Expr

instance Pretty ArithUnOp where
    pretty unOp = case unOp of
        Neg -> "-"

instance Pretty LogicUnOp where
    pretty unOp = case unOp of
        LNot -> "!"

instance Pretty TempUnOp where
    pretty unOp = case unOp of
        Next     -> "X"
        Finally  -> "F"
        Globally -> "G"
        Exists   -> "E"
        Forall   -> "A"

instance Pretty ProbUnOp where
    pretty unOp = case unOp of
        Prob _   -> "P"
        Steady _ -> "S"

instance Pretty Bound where
    pretty (Bound boundOp prob) = pretty boundOp <> text prob
    pretty (Query query)        = pretty query

instance Pretty BoundOp where
    pretty boundOp = case boundOp of
        BGt  -> ">"
        BLt  -> "<"
        BGte -> ">="
        BLte -> "<="

instance Pretty QueryType where
    pretty qt = case qt of
        QueryProb    -> "=?"
        QueryMinProb -> "min=?"
        QueryMaxProb -> "max=?"

instance Pretty FilterOp where
    pretty fOp = case fOp of
        FilterMin      -> "min"
        FilterMax      -> "max"
        FilterArgmin   -> "argmin"
        FilterArgmax   -> "argmax"
        FilterCount    -> "count"
        FilterSum      -> "sum"
        FilterAvg      -> "avg"
        FilterFirst    -> "first"
        FilterRange    -> "range"
        FilterForall   -> "forall"
        FilterExists   -> "exists"
        FilterPrint    -> "print"
        FilterPrintall -> "printall"
        FilterState    -> "state"

