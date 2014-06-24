{-# LANGUAGE DeriveFunctor, OverloadedStrings, TemplateHaskell #-}

module Syntax
  ( module Syntax.Operators

  , HasExprs(..)

  , Ident

  , Model(..)
  , Specification(..)

  , Definition(..)
  , _FeatureDef
  , _ControllerDef
  , _ModuleDef
  , _GlobalDef
  , _ConstDef
  , _FormulaDef
  , _PropertyDef

  , Feature(..)
  , Decomposition(..)
  , DecompOp(..)
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
  , ConstType(..)
  , Formula(..)
  , Property(..)
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

  , _NameExpr
  , _Name

  , exprAnnot
  , unaryExpr
  , binaryExpr
  , normalizeExpr

  , LModel
  , LSpecification
  , LDefinition
  , LFeature
  , LDecomposition
  , LDecompOp
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
  , LProperty
  , LStmt
  , LActionLabel
  , LUpdate
  , LAssign
  , LExpr
  , LRepeatable
  , LSome
  , LForLoop
  , LName
  , LRange
  ) where

import Control.Applicative hiding ( empty, optional )
import Control.Lens

import Text.PrettyPrint.Leijen.Text hiding ( (<$>) )
import qualified Text.PrettyPrint.Leijen.Text as PP

import SrcLoc
import Types

import Syntax.Operators

-- | Any syntax tree node @n@ that 'HasExprs' provides a 'Traversal' of all
-- 'Expr's contained in that node and its subtree.
class HasExprs n where
    exprs :: Traversal' (n a) (Expr a)

data Model a = Model [Definition a] deriving (Eq, Functor, Show)

data Specification a = Specification [Definition a] deriving (Eq, Functor, Show)

data Definition a
  = FeatureDef    (Feature a)
  | ControllerDef (Controller a)
  | ModuleDef     (Module a)
  | GlobalDef     (VarDecl a)
  | ConstDef      (Constant a)
  | FormulaDef    (Formula a)
  | PropertyDef   (Property a)
  deriving (Eq, Functor, Show)

data Feature a = Feature
  { featIdent       :: !Ident
  , featParams      :: [Ident]
  , featDecomp      :: Maybe (Decomposition a)
  , featConstraints :: [Expr a]
  , featModules     :: [Instance a]
  , featRewards     :: [Rewards a]
  , featAnnot       :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Feature where
    exprs f (Feature ident params decomp constrs mods rws a) =
        Feature ident params <$> traverse (exprs f) decomp
                             <*> traverse f constrs
                             <*> traverse (exprs f) mods
                             <*> traverse (exprs f) rws
                             <*> pure a

data Decomposition a = Decomposition (DecompOp a) [FeatureRef a] !a
                     deriving (Eq, Functor, Show)

instance HasExprs Decomposition where
    exprs f (Decomposition decompOp refs a) =
        Decomposition <$> exprs f decompOp
                      <*> traverse (exprs f) refs
                      <*> pure a

data DecompOp a
  = AllOf
  | OneOf
  | SomeOf
  | Group (Range a)
  deriving (Eq, Functor, Show)

instance HasExprs DecompOp where
    exprs f decompOp = case decompOp of
        Group range -> Group <$> both (exprs f) range
        _           -> pure decompOp

data FeatureRef a = FeatureRef
  { frOptional :: !Bool
  , frInstance :: Instance a
  , frCount    :: Maybe (Expr a)
  , frAlias    :: Maybe Ident
  } deriving (Eq, Functor, Show)

instance HasExprs FeatureRef where
    exprs f (FeatureRef opt inst cnt alias) =
        FeatureRef opt <$> exprs f inst
                       <*> traverse (exprs f) cnt
                       <*> pure alias

data Instance a = Instance !Ident [Expr a] !a deriving (Eq, Functor, Show)

instance HasExprs Instance where
    exprs f (Instance ident args a) =
        Instance ident <$> traverse (exprs f) args <*> pure a

data Rewards a = Rewards !Ident [Reward a] deriving (Eq, Functor, Show)

instance HasExprs Rewards where
    exprs f (Rewards ident rws) = Rewards ident <$> traverse (exprs f) rws

data Reward a = Reward
  { rwAction :: ActionLabel a
  , rwGuard  :: Expr a
  , rwReward :: Expr a
  , rwAnnot  :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Reward where
    exprs f (Reward actionLabel grd e a) =
        Reward <$> exprs f actionLabel <*> f grd <*> f e <*> pure a

data Controller a = Controller (ModuleBody a) deriving (Eq, Functor, Show)

instance HasExprs Controller where
    exprs f (Controller body) = Controller <$> exprs f body

data Module a = Module
  { modIdent    :: !Ident
  , modParams   :: [Ident]
  , modProvides :: [Ident]
  , modBody     :: ModuleBody a
  } deriving (Eq, Functor, Show)

instance HasExprs Module where
    exprs f (Module ident params provides body) =
        Module ident params provides <$> exprs f body

data ModuleBody a = ModuleBody
  { modVars     :: [VarDecl a]
  , modStmts    :: Repeatable Stmt a
  , modAnnot    :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs ModuleBody where
    exprs f (ModuleBody vars stmts a) =
        ModuleBody <$> traverse (exprs f) vars <*> exprs f stmts <*> pure a

data VarDecl a = VarDecl
  { declIdent :: !Ident
  , declType  :: VarType a
  , declInit  :: Maybe (Expr a)
  , declAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs VarDecl where
    exprs f (VarDecl ident vt e a) =
        VarDecl ident vt <$> traverse (exprs f) e <*> pure a

data VarType a
  = CompoundVarType (CompoundVarType a)
  | SimpleVarType   (SimpleVarType a)
  deriving (Eq, Functor, Show)

instance HasExprs VarType where
    exprs f vt = case vt of
        CompoundVarType cvt -> CompoundVarType <$> exprs f cvt
        SimpleVarType   svt -> SimpleVarType   <$> exprs f svt

data CompoundVarType a
  = ArrayVarType (Range a) (SimpleVarType a)
  deriving (Eq, Functor, Show)

instance HasExprs CompoundVarType where
    exprs f (ArrayVarType range svt) =
        ArrayVarType <$> both (exprs f) range <*> exprs f svt

data SimpleVarType a
  = BoolVarType
  | IntVarType (Range a)
  deriving (Eq, Functor, Show)

instance HasExprs SimpleVarType where
    exprs f svt = case svt of
        IntVarType range -> IntVarType <$> both (exprs f) range
        BoolVarType      -> pure BoolVarType

data Constant a = Constant
  { constType  :: !ConstType
  , constIdent :: !Ident
  , constValue :: Expr a
  , constAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Constant where
    exprs f (Constant ct name e a) = Constant ct name <$> f e <*> pure a

data ConstType
  = BoolConstType
  | IntConstType
  | DoubleConstType
  deriving (Eq, Show)

data Formula a = Formula
  { frmIdent  :: !Ident
  , frmParams :: [Ident]
  , frmExpr   :: Expr a
  , frmAnnot  :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Formula where
    exprs f (Formula ident params e a) =
        Formula ident params <$> f e <*> pure a

data Property a = Property
  { propIdent :: Maybe Ident
  , propExpr  :: Expr a
  , propAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Property where
    exprs f (Property ident e a) = Property ident <$> f e <*> pure a

data Stmt a = Stmt
  { stmtAction :: ActionLabel a
  , stmtGuard  :: Expr a
  , stmtUpdate :: Repeatable Update a
  , stmtAnnot  :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Stmt where
    exprs f (Stmt action grd upds a) =
        Stmt action <$> f grd <*> exprs f upds <*> pure a

data ActionLabel a
  = ActInitialize !a
  | ActActivate !a
  | ActDeactivate !a
  | Action (Name a) !a
  | NoAction
  deriving (Eq, Functor, Show)

instance HasExprs ActionLabel where
    exprs f actionLabel = case actionLabel of
        Action name a -> Action <$> exprs f name <*> pure a
        _             -> pure actionLabel

data Update a = Update
  { updProb   :: Maybe (Expr a)
  , updAssign :: Repeatable Assign a
  , updAnnot  :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Update where
    exprs f (Update prob asgns a) =
        Update <$> traverse f prob <*> exprs f asgns <*> pure a

data Assign a
  = Assign (Name a) (Expr a) !a
  | Activate (Name a) !a
  | Deactivate (Name a) !a
  deriving (Eq, Functor, Show)

instance HasExprs Assign where
    exprs f asgn = case asgn of
        Assign name e a   -> Assign     <$> exprs f name <*> f e <*> pure a
        Activate name a   -> Activate   <$> exprs f name <*> pure a
        Deactivate name a -> Deactivate <$> exprs f name <*> pure a

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

instance Plated (Expr a) where
    plate f e = case e of
        BinaryExpr binOp lhs rhs a ->
            BinaryExpr binOp <$> f lhs <*> f rhs <*> pure a
        UnaryExpr unOp e' a        -> UnaryExpr unOp <$> f e' <*> pure a
        CondExpr cond te ee a      ->
            CondExpr <$> f cond <*> f te <*> f ee <*> pure a
        LoopExpr loop a            -> LoopExpr <$> exprs f loop <*> pure a
        FuncExpr func args a       ->
            FuncExpr func <$> traverse f args <*> pure a
        NameExpr name a            -> NameExpr <$> exprs f name <*> pure a
        _                          -> pure e

instance HasExprs Expr where
    exprs f = f

newtype Repeatable b a = Repeatable [Some b a] deriving (Eq, Functor, Show)

instance (HasExprs b) => HasExprs (Repeatable b) where
    exprs f (Repeatable xs) = Repeatable <$> traverse (exprs f) xs

data Some b a
  = One (b a)
  | Many (ForLoop (Repeatable b) a)
  deriving (Eq, Functor, Show)

instance (HasExprs b) => HasExprs (Some b) where
    exprs f (One x)     = One  <$> exprs f x
    exprs f (Many loop) = Many <$> exprs f loop

data ForLoop b a = ForLoop
  { forVar   :: !Ident
  , forRange :: Range a
  , forBody  :: b a
  , forAnnot :: !a
  } deriving (Eq, Functor, Show)

instance (HasExprs b) => HasExprs (ForLoop b) where
    exprs f (ForLoop ident range body a) =
        ForLoop ident <$> both f range <*> exprs f body <*> pure a

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
  = Name !Ident
  | Member (Name a) !Ident
  | Index  (Name a) (Expr a)
  deriving (Eq, Functor, Show)

instance HasExprs Name where
    exprs f name = case name of
        Name _             -> pure name
        Member name' ident -> Member <$> exprs f name' <*> pure ident
        Index  name' e     -> Index  <$> exprs f name' <*> f e

type Range a = (Expr a, Expr a)

-- | This 'Prism' provides a 'Traversal' for 'IdentExpr's.
_NameExpr :: Prism' (Expr a) (Name a, a)
_NameExpr = prism' (uncurry NameExpr) f
  where
    f (NameExpr name a) = Just (name, a)
    f _                 = Nothing

-- | This 'Prism' provides a 'Traversal' for 'Name's.
_Name :: Prism' (Name a) Ident
_Name = prism' Name f
  where
    f (Name ident) = Just ident
    f _            = Nothing

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

-- | Smart constructor for 'BinaryExpr' which attaches the annotation of
-- the left inner expression @l@ to the newly created expression.
binaryExpr :: BinOp -> Expr a -> Expr a -> Expr a
binaryExpr binOp lhs rhs = BinaryExpr binOp lhs rhs (exprAnnot lhs)

-- | Smart constructor for 'UnaryExpr' which attaches the annotation of the
-- inner expression @e@ to the newly created expression.
unaryExpr :: UnOp -> Expr a -> Expr a
unaryExpr unOp e = UnaryExpr unOp e (exprAnnot e)

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

type LModel           = Model SrcLoc
type LSpecification   = Specification SrcLoc
type LDefinition      = Definition SrcLoc
type LFeature         = Feature SrcLoc
type LDecomposition   = Decomposition SrcLoc
type LDecompOp        = DecompOp SrcLoc
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
type LProperty        = Property SrcLoc
type LExpr            = Expr SrcLoc
type LRepeatable b    = Repeatable b SrcLoc
type LSome b          = Some b SrcLoc
type LForLoop b       = ForLoop b SrcLoc
type LName            = Name SrcLoc
type LRange           = Range SrcLoc

makePrisms ''Definition

instance Pretty (Model a) where
    pretty (Model defs ) = vsep (punctuate line $ map pretty defs) <> line

instance Pretty (Specification a) where
    pretty (Specification defs) =
        vsep (punctuate line $ map pretty defs) <> line

instance Pretty (Definition a) where
    pretty def = case def of
        FeatureDef  f   -> pretty f
        ControllerDef c -> pretty c
        ModuleDef   m   -> pretty m
        GlobalDef   g   -> "global" <+> pretty g
        ConstDef    c   -> pretty c
        FormulaDef  f   -> pretty f
        PropertyDef p   -> pretty p

instance Pretty (Feature a) where
    pretty (Feature ident params decomp constrs mods rws _) =
        "feature" <+> text ident <> prettyParams params <> line <>
        indent 4 body <> line <> "endfeature"
      where
        body = pretty decomp <> line <> constrList <> line <> line <>
               modList <> line <> line <>
               vsep (map pretty rws)
        constrList = vsep (map prettyConstraint constrs)
        modList
          | null mods = empty
          | otherwise = "modules" <+> colon <+>
            hang 4 (fillSep . punctuate comma $ map pretty mods) <> semi

prettyConstraint :: Expr a -> Doc
prettyConstraint e = "constraint" <+> colon <+> pretty e <> semi

instance Pretty (Decomposition a) where
    pretty (Decomposition decompOp cs _) = pretty decompOp <+> colon <+>
        hang 4 (fillSep . punctuate comma $ map pretty cs) <> semi

instance Pretty (DecompOp a) where
    pretty decomp = case decomp of
        AllOf       -> "allOf"
        OneOf       -> "oneOf"
        SomeOf      -> "someOf"
        Group range -> prettyRange range

instance Pretty (FeatureRef a) where
    pretty (FeatureRef optional inst cnt alias) =
        (if optional then "optional" else empty) <+>
        pretty inst <> maybe empty (brackets . pretty) cnt <+>
        maybe empty (("as" <+>) . text) alias

instance Pretty (Instance a) where
    pretty (Instance ident args _) = text ident <> prettyArgs args

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
        "module" <+> text ident <> prettyParams params <> line <>
        indent 4 (providesList <> line <> pretty body) <> line <> "endmodule"
      where
        providesList
          | null provides = empty
          | otherwise     = "provides" <+>
            hang 4 (fillSep . punctuate comma $ map text provides) <> semi

instance Pretty (ModuleBody a) where
    pretty (ModuleBody decls stmts _) =
        vsep (map pretty decls) <> line <> line <>
        prettyRepeatable False (PP.<$>) empty stmts

instance Pretty (VarDecl a) where
    pretty (VarDecl ident vt e _) =
        text ident <+> colon <+> pretty vt <>
        maybe empty ((" init" <+>) . pretty) e <> semi

instance Pretty (VarType a) where
    pretty vt = case vt of
        CompoundVarType cvt -> pretty cvt
        SimpleVarType svt   -> pretty svt

instance Pretty (CompoundVarType a) where
    pretty (ArrayVarType size svt) =
        "array" <+> prettyRange size <+> "of" <+> pretty svt

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
        "formula" <> prettyParams params <+> text ident <+> equals <+>
        pretty e <> semi

instance Pretty (Property a) where
    pretty (Property (Just ident) e _) =
        dquotes (text ident) <> colon <+> pretty e <> semi
    pretty (Property Nothing e _) = pretty e <> semi

instance Pretty (Stmt a) where
    pretty (Stmt action grd upds _) = hang 4 $
        brackets (pretty action) <+> pretty grd <+> "->" </>
        prettyRepeatable True (\l r -> l <+> "+" <+> r) "true" upds <> semi

instance Pretty (ActionLabel a) where
    pretty action = case action of
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
        parens (pretty name <> squote <+> equals <+> pretty e)
    pretty (Activate n _)   = "activate" <> parens (pretty n)
    pretty (Deactivate n _) = "deactivate" <> parens (pretty n)

instance Pretty (Expr a) where
    pretty = prettyExpr 0

prettyExpr :: Int -> Expr a -> Doc
prettyExpr prec e = case e of
    BinaryExpr binOp lhs rhs _ ->
        let prec' = binOpPrec binOp
        in  parens' (prec >= prec') $
                prettyExpr prec' lhs <+> pretty binOp <+> prettyExpr prec' rhs
    CondExpr cond te ee _ -> parens' (prec > 0) $
        prettyExpr 1 cond <+> char '?' <+>
        prettyExpr 1 te <+> colon <+> prettyExpr 1 ee
    UnaryExpr (ProbUnOp (Prob bound)) e' _ ->
        "P" <> pretty bound <+> brackets (pretty e')
    UnaryExpr (ProbUnOp (Steady bound)) e' _ ->
        "S" <> pretty bound <+> brackets (pretty e')
    UnaryExpr (TempUnOp Exists) e' _ -> "E" <+> brackets (pretty e')
    UnaryExpr (TempUnOp Forall) e' _ -> "A" <+> brackets (pretty e')
    UnaryExpr unOpT e' _ ->
        let prec' = unOpPrec unOpT
            sep'  = case unOpT of
                        TempUnOp _   -> (<+>)
                        _            -> (<>)
        in parens' (prec >= prec') $ pretty unOpT `sep'` prettyExpr prec' e'
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
        Name ident      -> text ident
        Member n' ident -> pretty n' <> dot <> text ident
        Index n' e      -> pretty n' <> brackets (pretty e)

prettyArgs :: (Pretty a) => [a] -> Doc
prettyArgs [] = empty
prettyArgs xs = parens (align . cat . punctuate comma $ map pretty xs)

prettyParams :: (Pretty a) => [a] -> Doc
prettyParams = prettyArgs

