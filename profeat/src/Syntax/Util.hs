{-# LANGUAGE LambdaCase #-}

module Syntax.Util
  ( identExpr
  , valueExpr

  , neutralElement

  , normalizeExpr
  , conjunction

  , containsLabelExpr

  , _One
  , ones
  , _NameExpr
  , _MissingExpr
  , _Ident
  , identifiers

  , viewIdentExpr
  , viewIdent
  , viewSimpleName

  , sortVarDeclsByLoc
  ) where

import Control.Applicative
import Control.Lens

import Data.List          ( foldl', sortBy )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Ord           ( comparing )

import Syntax
import Types

-- | Generates a 'NameExpr' with the given identifier.
identExpr :: Ident -> SrcLoc -> LExpr
identExpr ident l = NameExpr (_Ident # (ident, l)) l

-- | Generate an 'Expr'ession that represents a 'Value'.
valueExpr :: Value -> LExpr
valueExpr = \case
    BoolVal b -> BoolExpr b noLoc
    IntVal i  -> IntegerExpr i noLoc
    DblVal d  -> DecimalExpr d noLoc

-- | Returns the neutral element for the given 'BinOp' if it exists.
neutralElement :: BinOp -> Maybe LExpr
neutralElement = \case
    ArithBinOp binOp -> Just $ case binOp of
        Mul   -> intExpr 1
        Div   -> intExpr 1
        Add   -> intExpr 0
        Sub   -> intExpr 0
    LogicBinOp binOp -> case binOp of
        LImpl -> Nothing
        LEq   -> Nothing
        LAnd  -> Just $ BoolExpr True noLoc
        LOr   -> Just $ BoolExpr False noLoc
    _ -> Nothing

-- | Normalizes the given expression. The following rewrite rules are
-- applied:
--  - @!!e = e@
--  - @-(literal) = (-literal)@
normalizeExpr :: Expr a -> Expr a
normalizeExpr = transform $ \e -> case e of
    -- remove double negation
    UnaryExpr (LogicUnOp LNot) (UnaryExpr (LogicUnOp LNot) e' _) _ -> e'
    -- replace negated literals by negative literals
    UnaryExpr (ArithUnOp Neg) (IntegerExpr i a) _ -> IntegerExpr (negate i) a
    UnaryExpr (ArithUnOp Neg) (DecimalExpr d a) _ -> DecimalExpr (negate d) a
    _ -> e

-- | Isomorphism of a list of 'Expr'essions and their conjunction.
conjunction :: Iso' [LExpr] LExpr
conjunction = iso f g where
    f = foldl' lAnd (BoolExpr True noLoc)
    g (BinaryExpr (LogicBinOp LAnd) l r _) = g l ++ g r
    g (BoolExpr True _)                    = []
    g e                                    = [e]

-- | Returns True if the given expression contains labels.
containsLabelExpr :: Expr a -> Bool
containsLabelExpr = any isLabel . universe
  where
    isLabel (LabelExpr _ _) = True
    isLabel _               = False

_One :: Prism' (Some b a) (b a)
_One = prism' One f where
    f (One x) = Just x
    f _       = Nothing

ones :: Traversal' (Repeatable b a) (b a)
ones f (Repeatable ss) = Repeatable <$> (traverse._One) f ss

-- | This 'Prism' provides a 'Traversal' for 'NameExpr's.
_NameExpr :: Prism' (Expr a) (Name a, a)
_NameExpr = prism' (uncurry NameExpr) f
  where
    f (NameExpr name a) = Just (name, a)
    f _                 = Nothing

-- | This 'Prism' provides a 'Traversal' for 'MissingExpr's.
_MissingExpr :: Prism' (Expr a) a
_MissingExpr = prism' MissingExpr f
  where
    f (MissingExpr a) = Just a
    f _               = Nothing

-- | This 'Prism' provides a 'Traversal' for 'Name's consisting solely of
-- an identifier.
_Ident :: Prism' (Name a) (Ident, a)
_Ident = prism' c d where
    c (ident, a)                        = Name ((ident, Nothing) :| []) a
    d (Name ((ident, Nothing) :| []) a) = Just (ident, a)
    d _                                 = Nothing

-- | A 'Traversal' of all identifiers in an expression.
identifiers :: Traversal' (Expr a) Ident
identifiers = _NameExpr._1._Ident._1

-- | If the given 'Expr' is an identifier, 'Just' this identifier is
-- returned.
viewIdentExpr :: Expr a -> Maybe Ident
viewIdentExpr = preview $ _NameExpr._1._Ident._1

-- | If the given 'Name' is an identifier, 'Just' this identifier is
-- returned.
viewIdent :: Name a -> Maybe Ident
viewIdent = preview $ _Ident._1

-- | If the given 'Name' does not contain any member access, 'Just' its
-- identifier, index and annotation are returned.
viewSimpleName :: Name a -> Maybe (Ident, Maybe (Expr a), a)
viewSimpleName name = case name of
    Name ((ident, idx) :| []) l -> Just (ident, idx, l)
    _                           -> Nothing

sortVarDeclsByLoc :: [LVarDecl] -> [LVarDecl]
sortVarDeclsByLoc = sortBy (comparing declAnnot)

