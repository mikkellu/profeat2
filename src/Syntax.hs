{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module Syntax
  ( module Syntax.Operators

  , Ident

  , Model(..)
  , Definition(..)
  , Feature(..)
  , Decomposition(..)
  , FeatureRef(..)
  , Instance(..)
  , Rewards(..)
  , Reward(..)
  , Controller(..)
  , Module(..)
  , ModuleBody(..)
  , VarDecl(..)
  , VarType(..)
  , CompoundVarType(..)
  , SimpleVarType(..)
  , Constant(..)
  , Formula(..)
  , Stmt(..)
  , ActionLabel(..)
  , Update(..)
  , Assign(..)
  , Expr(..)
  , Repeatable(..)
  , Some(..)
  , ForLoop(..)
  , Function(..)
  , Name(..)
  , Range

  , exprAnnot

  , LModel
  , LDefinition
  , LFeature
  , LDecomposition
  , LFeatureRef
  , LInstance
  , LRewards
  , LReward
  , LController
  , LModule
  , LModuleBody
  , LVarDecl
  , LVarType
  , LCompoundVarType
  , LSimpleVarType
  , LConstant
  , LFormula
  , LStmt
  , LActionLabel
  , LUpdate
  , LAssign
  , LExpr
  , LForLoop
  , LName
  , LRange
  ) where

import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text

import SrcLoc

import Syntax.Operators

type Ident = Text

data Model a = Model [Definition a] deriving (Eq, Functor, Show)

data Definition a
  = FeatureDef    (Feature a)
  | ControllerDef (Controller a)
  | ModuleDef     (Module a)
  | GlobalDef     (VarDecl a)
  | ConstDef      (Constant a)
  | FormulaDef    (Formula a)
  deriving (Eq, Functor, Show)

data Feature a = Feature
  { featIdent       :: !Ident
  , featParams      :: [Ident]
  , featDecomp      :: Decomposition a
  , featChildren    :: [FeatureRef a]
  , featConstraints :: [Expr a]
  , featModules     :: [Instance a]
  , featRewards     :: [Rewards a]
  , featAnnot       :: !a
  } deriving (Eq, Functor, Show)

data Decomposition a
  = AllOf
  | OneOf
  | SomeOf
  | Group (Range a)
  deriving (Eq, Functor, Show)

data FeatureRef a = FeatureRef !Bool (Instance a) deriving (Eq, Functor, Show)

data Instance a = Instance !Ident [Expr a] !a deriving (Eq, Functor, Show)

data Rewards a = Rewards !Ident [Reward a] deriving (Eq, Functor, Show)

data Reward a = Reward
  { rwAction :: ActionLabel a
  , rwGuard  :: Expr a
  , rwReward :: Expr a
  , rwAnnot  :: !a
  } deriving (Eq, Functor, Show)

data Controller a = Controller (ModuleBody a) deriving (Eq, Functor, Show)

data Module a = Module
  { modIdent    :: !Ident
  , modParams   :: [Ident]
  , modProvides :: [Ident]
  , modBody     :: ModuleBody a
  } deriving (Eq, Functor, Show)

data ModuleBody a = ModuleBody
  { modVars     :: [VarDecl a]
  , modStmts    :: Repeatable Stmt a
  , modAnnot    :: !a
  } deriving (Eq, Functor, Show)

data VarDecl a = VarDecl
  { declIdent :: !Ident
  , declType  :: VarType a
  , declInit  :: Maybe (Expr a)
  , declAnnot :: !a
  } deriving (Eq, Functor, Show)

data VarType a
  = CompoundVarType (CompoundVarType a) !a
  | SimpleVarType   (SimpleVarType a) !a
  deriving (Eq, Functor, Show)

data CompoundVarType a
  = ArrayVarType (Range a) (SimpleVarType a)
  deriving (Eq, Functor, Show)

data SimpleVarType a
  = BoolVarType
  | IntVarType (Range a)
  deriving (Eq, Functor, Show)

data Constant a = Constant
  { constType  :: !ConstType
  , constIdent :: !Ident
  , constValue :: Expr a
  , constAnnot :: !a
  } deriving (Eq, Functor, Show)

data ConstType
  = BoolConstType
  | IntConstType
  | DoubleConstType
  deriving (Eq, Show)

data Formula a = Formula
  { frmIdent :: !Ident
  , frmParams :: [Ident]
  , frmExpr   :: Expr a
  , frmAnnot  :: !a
  } deriving (Eq, Functor, Show)

data Stmt a = Stmt
  { stmtAction :: ActionLabel a
  , stmtGuard  :: Expr a
  , stmtUpdate :: Repeatable Update a
  , stmtAnnot  :: !a
  } deriving (Eq, Functor, Show)

data ActionLabel a
  = ActInitialize !a
  | ActActivate !a
  | ActDeactivate !a
  | Action (Name a) !a
  | NoAction
  deriving (Eq, Functor, Show)

data Update a = Update
  { updProb   :: Maybe (Expr a)
  , updAssign :: Repeatable Assign a
  , updAnnot  :: !a
  } deriving (Eq, Functor, Show)

data Assign a
  = Assign !Ident (Expr a) !a
  | Activate (Name a) !a
  | Deavtivate (Name a) !a
  deriving (Eq, Functor, Show)

data Expr a
  = BinaryExpr !BinOp (Expr a) (Expr a) !a
  | UnaryExpr !UnOp (Expr a) !a
  | CondExpr (Expr a) (Expr a) (Expr a) !a
  | LoopExpr (ForLoop Expr a) !a
  | FuncExpr !Function [Expr a] !a
  | NameExpr (Name a) !a
  | DecimalExpr !Double !a
  | IntegerExpr !Integer !a
  | BoolExpr !Bool !a
  | MissingExpr !a
  deriving (Eq, Functor, Show)

newtype Repeatable b a = Repeatable [Some b a] deriving (Eq, Functor, Show)

data Some b a
  = One (b a)
  | Many (ForLoop (Repeatable b) a)
  deriving (Eq, Functor, Show)

data ForLoop b a = ForLoop
  { forVar   :: !Ident
  , forRange :: Range a
  , forBody  :: b a
  , forAnnot :: !a
  } deriving (Eq, Functor, Show)

data Function
  = FuncMin
  | FuncMax
  | FuncFloor
  | FuncCeil
  | FuncPow
  | FuncMod
  | FuncLog
  | Func !Ident
  deriving (Eq, Show)

data Name a
  = Name !Ident !a
  | Member (Name a) !Ident !a
  | Index  (Name a) (Expr a)
  | Concat (Name a) (Expr a)
  deriving (Eq, Functor, Show)

type Range a = (Expr a, Expr a)

exprAnnot :: Expr a -> a
exprAnnot e = case e of
    BinaryExpr _ _ _ a -> a
    UnaryExpr _ _ a    -> a
    CondExpr _ _ _ a   -> a
    LoopExpr _ a       -> a
    FuncExpr _ _ a     -> a
    NameExpr _ a       -> a
    DecimalExpr _ a    -> a
    IntegerExpr _ a    -> a
    BoolExpr _ a       -> a
    MissingExpr a      -> a

type LModel           = Model SrcLoc
type LDefinition      = Definition SrcLoc
type LFeature         = Feature SrcLoc
type LDecomposition   = Decomposition SrcLoc
type LFeatureRef      = FeatureRef SrcLoc
type LInstance        = Instance SrcLoc
type LRewards         = Rewards SrcLoc
type LReward          = Reward SrcLoc
type LController      = Controller SrcLoc
type LModule          = Module SrcLoc
type LModuleBody      = ModuleBody SrcLoc
type LVarDecl         = VarDecl SrcLoc
type LVarType         = VarType SrcLoc
type LCompoundVarType = CompoundVarType SrcLoc
type LSimpleVarType   = SimpleVarType SrcLoc
type LConstant        = Constant SrcLoc
type LFormula         = Formula SrcLoc
type LStmt            = Stmt SrcLoc
type LActionLabel     = ActionLabel SrcLoc
type LUpdate          = Update SrcLoc
type LAssign          = Assign SrcLoc
type LExpr            = Expr SrcLoc
type LForLoop b       = ForLoop b SrcLoc
type LName            = Name SrcLoc
type LRange           = Range SrcLoc

instance Pretty (Model a) where
    pretty (Model defs ) = vsep (punctuate line $ map pretty defs) <> line

instance Pretty (Definition a) where
    pretty def = case def of
        FeatureDef  f   -> pretty f
        ControllerDef c -> pretty c
        ModuleDef   m   -> pretty m
        GlobalDef   g   -> "global" <+> pretty g
        ConstDef    c   -> pretty c
        FormulaDef  f   -> pretty f

instance Pretty (Feature a) where
    pretty (Feature ident params decomp children constrs mods rws _) =
        "feature" <+> text ident <> tupled (map text params) <> line <>
        indent 4 body <> line <> "endfeature"
      where
        body = decompList <> line <> constrList <> line <> line <>
               modList <> line <> line <>
               vsep (map pretty rws)
        decompList =
            pretty decomp <+>
            hang 4 (fillSep . punctuate comma $ map pretty children) <> semi
        constrList = vsep (map prettyConstraint constrs)
        modList
          | null mods = empty
          | otherwise = "modules" <+>
            hang 4 (fillSep . punctuate comma $ map pretty mods) <> semi

prettyConstraint :: Expr a -> Doc
prettyConstraint e = "constraint" <+> pretty e <> semi

instance Pretty (Decomposition a) where
    pretty decomp = case decomp of
        AllOf       -> "allOf"
        OneOf       -> "oneOf"
        SomeOf      -> "someOf"
        Group range -> prettyRange range

instance Pretty (FeatureRef a) where
    pretty (FeatureRef optional inst) =
        (if optional then "optional" else empty) <+> pretty inst

instance Pretty (Instance a) where
    pretty (Instance ident args _) = text ident <> tupled (map pretty args)

instance Pretty (Rewards a) where
    pretty (Rewards ident rws) =
        "costs" <+> dquotes (text ident) <> line <>
        indent 4 (vsep $ map pretty rws) <> line <>
        "endcosts"

instance Pretty (Reward a) where
    pretty (Reward action grd e _) =
        brackets (pretty action) <+> pretty grd <+> colon <+> pretty e <> semi

instance Pretty (Controller a) where
    pretty (Controller body) =
        "controller" <> line <>
        indent 4 (pretty body) <> line <> "endcontroller"

instance Pretty (Module a) where
    pretty (Module ident params provides body) =
        "module" <+> text ident <> tupled (map pretty params) <> line <>
        indent 4 (providesList <> line <> pretty body) <> line <> "endmodule"
      where
        providesList
          | null provides = empty
          | otherwise     = "provides" <+>
            hang 4 (fillSep . punctuate comma $ map text provides) <> semi

instance Pretty (ModuleBody a) where
    pretty (ModuleBody decls stmts _) =
        vsep (map pretty decls) <> line <> line <>
        prettyRepeatable False (<$>) empty stmts

instance Pretty (VarDecl a) where
    pretty (VarDecl ident vt e _) =
        text ident <+> colon <+> pretty vt <>
        maybe empty ((" init" <+>) . pretty) e <> semi

instance Pretty (VarType a) where
    pretty vt = case vt of
        CompoundVarType cvt _ -> pretty cvt
        SimpleVarType svt   _ -> pretty svt

instance Pretty (CompoundVarType a) where
    pretty (ArrayVarType range svt) =
        "array" <+> prettyRange range <+> "of" <+> pretty svt

instance Pretty (SimpleVarType a) where
    pretty svt = case svt of
        BoolVarType      -> "bool"
        IntVarType range -> pretty range

instance Pretty (Constant a) where
    pretty (Constant ct ident e _) =
        "const" <+> pretty ct <+> text ident <+> equals <+> pretty e <> semi

instance Pretty ConstType where
    pretty ct = case ct of
        BoolConstType   -> "bool"
        IntConstType    -> "int"
        DoubleConstType -> "double"

instance Pretty (Formula a) where
    pretty (Formula ident params e _) =
        "formula" <> tupled (map text params) <+> text ident <+> equals <+>
        pretty e <> semi

instance Pretty (Stmt a) where
    pretty (Stmt action grd upds _) = hang 4 $
        brackets (pretty action) <+> pretty grd <+> "->" </>
        prettyRepeatable True (\l r -> l <+> "+" <+> r) "true" upds <> semi

instance Pretty (ActionLabel a) where
    pretty action = brackets $ case action of
        ActInitialize _ -> "initialize"
        ActActivate _   -> "activate"
        ActDeactivate _ -> "deactivate"
        Action n _      -> pretty n
        NoAction        -> empty

instance Pretty (Update a) where
    pretty (Update e asgns _) = prob e <>
        align (prettyRepeatable True (\l r -> l <+> "&" <+> r) "true" asgns)
      where
        prob = maybe empty ((<> colon) . pretty)

instance Pretty (Assign a) where
    pretty (Assign name e _) =
            parens (text name <> squote <+> equals <+> pretty e)
    pretty (Activate n _)   = "activate" <> parens (pretty n)
    pretty (Deavtivate n _) = "deactivate" <> parens (pretty n)

instance Pretty (Expr a) where
    pretty = prettyExpr 0

prettyExpr :: Int -> Expr a -> Doc
prettyExpr prec e = case e of
    BinaryExpr binOp lhs rhs _ ->
        let prec' = binOpPrec binOp
        in  parens' (prec >= prec') $
                prettyExpr prec' lhs <+> pretty binOp <+> prettyExpr prec' rhs
    UnaryExpr unOpT e' _ ->
        let prec' = unOpPrec unOpT
        in parens' (prec >= prec') $ pretty unOpT <> prettyExpr prec' e'
    CondExpr cond te ee _ -> parens' (prec > 0) $
        prettyExpr 1 cond <+> char '?' <+>
        prettyExpr 1 te <+> colon <+> prettyExpr 1 ee
    LoopExpr loop _       -> prettyLoop pretty True loop
    FuncExpr func args _  ->
        pretty func <> parens (align . cat . punctuate comma $ map pretty args)
    NameExpr n _          -> pretty n
    DecimalExpr d _       -> double d
    IntegerExpr i _       -> integer i
    BoolExpr True _       -> "true"
    BoolExpr False _      -> "false"
    MissingExpr _         -> "..."
  where
    parens' True  = parens
    parens' False = id

prettyRepeatable :: (Pretty (b a))
                 => Bool
                 -> (Doc -> Doc -> Doc)
                 -> Doc
                 -> Repeatable b a
                 -> Doc
prettyRepeatable inline sep' empty' (Repeatable xs) = case xs of
    [] -> empty'
    _  -> foldr1 sep' $ map ppSome xs
  where
    ppSome (One x)     = pretty x
    ppSome (Many loop) =
        prettyLoop (prettyRepeatable inline sep' empty') inline loop

prettyLoop :: (b a -> Doc) -> Bool -> ForLoop b a -> Doc
prettyLoop pp inline (ForLoop v range body _) =
    let opening = "for" <+> text v <+> "in" <+> prettyRange range
        closing = "endfor"
    in if inline
          then opening <+> pp body <+> closing
          else opening <> line <> nest 4 (pp body) <> line <> closing

instance Pretty Function where
    pretty func = case func of
        FuncMin    -> "min"
        FuncMax    -> "max"
        FuncFloor  -> "floor"
        FuncCeil   -> "ceil"
        FuncPow    -> "pow"
        FuncMod    -> "mod"
        FuncLog    -> "log"
        Func ident -> text ident

instance Pretty (Name a) where
    pretty n = case n of
        Name ident _      -> text ident
        Member n' ident _ -> pretty n' <> dot <> text ident
        Index n' e        -> pretty n' <> brackets (pretty e)
        Concat n' e       -> pretty n' <> char '#' <> pretty e

prettyRange :: Range a -> Doc
prettyRange (lower, upper) = brackets (pretty lower <+> ".." <+> pretty upper)

