{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Syntax
  ( module Syntax.Operators
  , module SrcLoc

  , HasExprs(..)

  , Ident

  , Model(..)
  , ModelType(..)
  , Specification(..)

  , Definition(..)
  , _FamilyDef
  , _FeatureDef
  , _ControllerDef
  , _ModuleDef
  , _GlobalDef
  , _ConstDef
  , _FormulaDef
  , _LabelDef
  , _InitDef
  , _RewardsDef
  , _InvariantDef
  , _PropertyDef

  , Family(..)
  , Feature(..)
  , Decomposition(..)
  , DecompOp(..)
  , Constraint(..)
  , FeatureRef(..)
  , Instance(..)
  , Rewards(..)
  , Reward(..)
  , Init(..)
  , Invariant(..)
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
  , Label(..)
  , Property(..)
  , Stmt(..)
  , ActionLabel(..)
  , Update(..)
  , Assign(..)
  , Expr(..)
  , SampleArg(..)
  , RewardProp(..)
  , Bound(..)
  , MinMax(..)
  , Repeatable(..)
  , Some(..)
  , ForLoop(..)
  , Function(..)
  , Name(..)
  , Range

  , initConfLabelIdent

  , defAnnot
  , exprAnnot

  , unaryExpr
  , binaryExpr
  , intExpr

  , (?), ThenElse(..)
  , eq, neq, gt, lt, gte, lte
  , lAnd

  , LModel
  , LSpecification
  , LDefinition
  , LFamily
  , LFeature
  , LDecomposition
  , LDecompOp
  , LConstraint
  , LFeatureRef
  , LInstance
  , LRewards
  , LReward
  , LInit
  , LInvariant
  , LController
  , LModule
  , LModuleBody
  , LVarDecl
  , LVarType
  , LCompoundVarType
  , LSimpleVarType
  , LConstant
  , LFormula
  , LLabel
  , LProperty
  , LStmt
  , LActionLabel
  , LUpdate
  , LAssign
  , LExpr
  , LSampleArg
  , LRewardProp
  , LBound
  , LRepeatable
  , LSome
  , LForLoop
  , LName
  , LRange
  ) where

import Control.Lens

import Data.List.NonEmpty
import Data.Text.Lazy ( Text )

import Text.PrettyPrint.Leijen.Text hiding ( (<$>) )
import qualified Text.PrettyPrint.Leijen.Text as PP

import SrcLoc
import Types

import Syntax.Operators

-- | Any syntax tree node @n@ that 'HasExprs' provides a 'Traversal' of all
-- 'Expr's contained in that node and its subtree.
class HasExprs n where
    exprs :: Traversal' (n a) (Expr a)

data Model a = Model !ModelType [Definition a] deriving (Eq, Functor, Show)

newtype Specification a = Specification [Definition a] deriving (Eq, Functor, Show)

data Definition a
  = FamilyDef     (Family a)
  | FeatureDef    (Feature a)
  | ControllerDef (Controller a)
  | ModuleDef     (Module a)
  | GlobalDef     (VarDecl a)
  | ConstDef      (Constant a)
  | FormulaDef    (Formula a)
  | LabelDef      (Label a)
  | RewardsDef    (Rewards a)
  | InitDef       (Init a)
  | InvariantDef  (Invariant a)
  | PropertyDef   (Property a)
  deriving (Eq, Functor, Show)

data Family a = Family
  { famParameters  :: [VarDecl a]
  , famConstraints :: [Expr a]
  , famFeatures    :: [Name a]
  , famAnnot       :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Family where
    exprs f Family{..} =
        Family <$> traverse (exprs f) famParameters
               <*> traverse f famConstraints
               <*> traverse (exprs f) famFeatures
               <*> pure famAnnot

data Feature a = Feature
  { featIsRoot      :: !Bool
  , featIdent       :: !Ident
  , featParams      :: [Ident]
  , featAttributes  :: [VarDecl a]
  , featDecomp      :: Maybe (Decomposition a)
  , featConstraints :: [Constraint a]
  , featBlocking    :: [Name a]
  , featModules     :: [Instance a]
  , featRewards     :: [Rewards a]
  , featAnnot       :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Feature where
    exprs f Feature{..} =
        Feature featIsRoot featIdent featParams
            <$> traverse (exprs f) featAttributes
            <*> traverse (exprs f) featDecomp
            <*> traverse (exprs f) featConstraints
            <*> traverse (exprs f) featBlocking
            <*> traverse (exprs f) featModules
            <*> traverse (exprs f) featRewards
            <*> pure featAnnot

data Decomposition a = Decomposition
  { decompOperator :: DecompOp a
  , decompChildren :: [FeatureRef a]
  , decompAnnot    :: !a
  } deriving (Eq, Functor, Show)

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

data Constraint a = Constraint
  { constrInitial :: !Bool
  , constrExpr    :: Expr a
  , constrAnnot   :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Constraint where
    exprs f (Constraint initial e a) = Constraint initial <$> f e <*> pure a

data FeatureRef a = FeatureRef
  { frOptional :: !Bool
  , frInstance :: Instance a
  , frAlias    :: Maybe Ident
  , frCount    :: Maybe (Expr a)
  } deriving (Eq, Functor, Show)

instance HasExprs FeatureRef where
    exprs f (FeatureRef opt inst alias cnt) =
        FeatureRef opt <$> exprs f inst
                       <*> pure alias
                       <*> traverse (exprs f) cnt

data Instance a = Instance
  { instIdent :: !Ident
  , instArgs  :: [Expr a]
  , instAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Instance where
    exprs f (Instance ident args a) =
        Instance ident <$> traverse (exprs f) args <*> pure a

data Rewards a = Rewards
  { rwsIdent   :: !Ident
  , rwsRewards :: Repeatable Reward a
  , rwsAnnot   :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Rewards where
    exprs f (Rewards ident rws a) =
        Rewards ident <$> exprs f rws <*> pure a

data Reward a = Reward
  { rwAction :: Maybe (ActionLabel a)
  , rwGuard  :: Expr a
  , rwReward :: Expr a
  , rwAnnot  :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Reward where
    exprs f (Reward actionLabel grd e a) =
        Reward <$> traverse (exprs f) actionLabel <*> f grd <*> f e <*> pure a

data Init a = Init
  { initExpr  :: Expr a
  , initAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Init where
    exprs f (Init e a) = Init <$> f e <*> pure a

data Invariant a = Invariant
  { invExpr  :: Expr a
  , invAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Invariant where
    exprs f (Invariant e a) = Invariant <$> f e <*> pure a

newtype Controller a = Controller (ModuleBody a) deriving (Eq, Functor, Show)

instance HasExprs Controller where
    exprs f (Controller body) = Controller <$> exprs f body

data Module a = Module
  { modIdent  :: !Ident
  , modParams :: [Ident]
  , modBody   :: ModuleBody a
  } deriving (Eq, Functor, Show)

instance HasExprs Module where
    exprs f (Module ident params body) =
        Module ident params <$> exprs f body

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
        VarDecl ident <$> exprs f vt <*> traverse (exprs f) e <*> pure a

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
  , constValue :: Maybe (Expr a)
  , constAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Constant where
    exprs f (Constant ct name e a) = Constant ct name <$> traverse f e <*> pure a

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

data Label a = Label
  { lblIdent :: !Ident
  , lblExpr  :: Expr a
  , lblAnnot :: !a
  } deriving (Eq, Functor, Show)

instance HasExprs Label where
    exprs f (Label ident e a) = Label ident <$> f e <*> pure a

initConfLabelIdent :: Ident
initConfLabelIdent = "initconf"

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
        Stmt <$> exprs f action <*> f grd <*> exprs f upds <*> pure a

data ActionLabel a
  = ActActivate !a
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
  | CallExpr (Expr a) [Expr a] !a
  | NameExpr (Name a) !a
  | FuncExpr !Function !a
  | SampleExpr [SampleArg a] !a
  | FilterExpr !FilterOp (Expr a) (Maybe (Expr a)) !a
  | ProbExpr (Bound a) (Expr a) !a
  | SteadyExpr (Bound a) (Expr a) !a
  | RewardExpr (Maybe (Expr a)) (Bound a) (RewardProp a) !a
  | ConditionalExpr (Expr a) (Expr a) !a
  | QuantileExpr !MinMax !Ident (Expr a) !a
  | LabelExpr !Ident !a
  | ArrayExpr (NonEmpty (Expr a)) !a
  | DecimalExpr !Double !a
  | IntegerExpr !Integer !a
  | BoolExpr !Bool !a
  | MissingExpr !a
  deriving (Eq, Functor, Show)

instance Num (Expr SrcLoc) where
    (IntegerExpr 0 _) + e = e
    e + (IntegerExpr 0 _) = e
    (DecimalExpr 0 _) + e = e
    e + (DecimalExpr 0 _) = e
    lhs + rhs             = binaryExpr (ArithBinOp Add) lhs rhs

    (IntegerExpr 1 _) * e = e
    e * (IntegerExpr 1 _) = e
    (DecimalExpr 1 _) * e = e
    e * (DecimalExpr 1 _) = e
    (IntegerExpr 0 _) * _ = 0
    _ * (IntegerExpr 0 _) = 0
    (DecimalExpr 0 _) * _ = DecimalExpr 0.0 noLoc
    _ * (DecimalExpr 0 _) = DecimalExpr 0.0 noLoc
    lhs * rhs             = binaryExpr (ArithBinOp Mul) lhs rhs

    e - (IntegerExpr 0 _) = e
    (IntegerExpr 0 _) - e = negate e
    e - (DecimalExpr 0 _) = e
    (DecimalExpr 0 _) - e = negate e
    lhs - rhs             = binaryExpr (ArithBinOp Sub) lhs rhs

    negate (IntegerExpr i _) = IntegerExpr (negate i) noLoc
    negate (DecimalExpr d _) = DecimalExpr (negate d) noLoc
    negate e                 = unaryExpr (ArithUnOp Neg) e

    abs e = e `lt` 0 ? negate e :? e

    signum e = e `lt` 0 ? -1 :? (e `gt` 0 ? 1 :? 0)

    fromInteger = intExpr

instance Plated (Expr a) where
    plate f e = case e of
        BinaryExpr binOp lhs rhs a ->
            BinaryExpr binOp <$> f lhs <*> f rhs <*> pure a
        UnaryExpr unOp e' a -> UnaryExpr unOp <$> f e' <*> pure a
        CondExpr cond te ee a ->
            CondExpr <$> f cond <*> f te <*> f ee <*> pure a
        LoopExpr loop a -> LoopExpr <$> exprs f loop <*> pure a
        CallExpr e' args a -> CallExpr <$> f e' <*> traverse f args <*> pure a
        NameExpr name a -> NameExpr <$> exprs f name <*> pure a
        FuncExpr _ _ -> pure e
        SampleExpr args a -> SampleExpr <$> traverse (exprs f) args <*> pure a
        FilterExpr fOp prop grd a  ->
            FilterExpr fOp <$> f prop <*> traverse f grd <*> pure a
        ArrayExpr es a             ->
            ArrayExpr <$> traverse f es <*> pure a
        ProbExpr bound e' a -> ProbExpr <$> exprs f bound <*> f e' <*> pure a
        SteadyExpr bound e' a ->
            SteadyExpr <$> exprs f bound <*> f e' <*> pure a
        RewardExpr struct bound prop a ->
            RewardExpr struct <$> exprs f bound <*> exprs f prop <*> pure a
        ConditionalExpr prop cond a ->
            ConditionalExpr <$> f prop <*> f cond <*> pure a
        QuantileExpr minMax v e' a -> QuantileExpr minMax v <$> f e' <*> pure a
        LabelExpr _ _   -> pure e -- list leaf nodes to get a warning if some
        DecimalExpr _ _ -> pure e -- (newly added) constructor is missing
        IntegerExpr _ _ -> pure e
        BoolExpr _ _    -> pure e
        MissingExpr _   -> pure e

instance HasExprs Expr where
    exprs f = f

data SampleArg a
    = ArgExpr (Expr a)
    | ArgString !Ident
    deriving (Eq, Functor, Show)

instance HasExprs SampleArg where
    exprs f sa = case sa of
        ArgExpr e -> ArgExpr <$> f e
        _         -> pure sa

data RewardProp a
  = Reachability (Expr a)
  | Cumulative !Text
  | Instant !Text
  | Steady
  deriving (Eq, Functor, Show)

instance HasExprs RewardProp where
    exprs f prop = case prop of
        Reachability prop' -> Reachability <$> f prop'
        _                  -> pure prop

data Bound a = Bound
  { boundMinMax :: Maybe MinMax
  , boundOp     :: !BoundOp
  , boundExprs  :: [Expr a]
  } deriving (Eq, Functor, Show)

instance HasExprs Bound where
    exprs f Bound{..} = Bound boundMinMax boundOp <$> traverse f boundExprs

data MinMax
  = Min
  | Max
  deriving (Eq, Show)

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
  | FuncActive
  | FuncIActive
  | FuncBinom
  deriving (Eq, Show)

data Name a = Name (NonEmpty (Ident, Maybe (Expr a))) !a
            deriving (Eq, Functor, Show)

instance HasExprs Name where
    exprs f (Name name a) = Name <$> (traverse._2._Just) f name <*> pure a

type Range a = (Expr a, Expr a)

defAnnot :: Definition a -> a
defAnnot = \case
    FamilyDef f                  -> famAnnot f
    FeatureDef f                 -> featAnnot f
    ControllerDef (Controller c) -> modAnnot c
    ModuleDef m                  -> modAnnot (modBody m)
    GlobalDef g                  -> declAnnot g
    ConstDef c                   -> constAnnot c
    FormulaDef f                 -> frmAnnot f
    LabelDef l                   -> lblAnnot l
    RewardsDef r                 -> rwsAnnot r
    InitDef i                    -> initAnnot i
    InvariantDef i               -> invAnnot i
    PropertyDef p                -> propAnnot p

exprAnnot :: Expr a -> a
exprAnnot e = case e of
    BinaryExpr _ _ _ a    -> a
    UnaryExpr _ _ a       -> a
    CondExpr _ _ _ a      -> a
    LoopExpr _ a          -> a
    CallExpr _ _ a        -> a
    NameExpr _ a          -> a
    FuncExpr _ a          -> a
    SampleExpr _ a        -> a
    FilterExpr _ _ _ a    -> a
    ProbExpr _ _ a        -> a
    SteadyExpr _ _ a      -> a
    RewardExpr _ _ _ a    -> a
    ConditionalExpr _ _ a -> a
    QuantileExpr _ _ _ a  -> a
    LabelExpr _ a         -> a
    ArrayExpr _ a         -> a
    DecimalExpr _ a       -> a
    IntegerExpr _ a       -> a
    BoolExpr _ a          -> a
    MissingExpr a         -> a

-- | Smart constructor for 'BinaryExpr' which attaches the annotation of
-- the left inner expression @l@ to the newly created expression.
binaryExpr :: BinOp -> Expr a -> Expr a -> Expr a
binaryExpr binOp lhs rhs = BinaryExpr binOp lhs rhs (exprAnnot lhs)

-- | Smart constructor for 'UnaryExpr' which attaches the annotation of the
-- inner expression @e@ to the newly created expression.
unaryExpr :: UnOp -> Expr a -> Expr a
unaryExpr unOp e = UnaryExpr unOp e (exprAnnot e)

-- | Generates an 'IntegerExpr'.
intExpr :: Integer -> LExpr
intExpr = flip IntegerExpr noLoc

infixl 0 ?
infixl 1 :?

data ThenElse a = Expr a :? Expr a

(?) :: Expr a -> ThenElse a -> Expr a
cond ? (te :? ee) = CondExpr cond te ee $ exprAnnot cond

eq, neq, gt, lt, gte, lte, lAnd :: Expr a -> Expr a -> Expr a
eq  = binaryExpr (EqBinOp Eq)
neq = binaryExpr (EqBinOp Neq)
gt  = binaryExpr (RelBinOp Gt)
lt  = binaryExpr (RelBinOp Lt)
gte = binaryExpr (RelBinOp Gte)
lte = binaryExpr (RelBinOp Lte)

lAnd (BoolExpr True _) rhs = rhs
lAnd lhs (BoolExpr True _) = lhs
lAnd lhs rhs               = binaryExpr (LogicBinOp LAnd) lhs rhs

type LModel           = Model SrcLoc
type LSpecification   = Specification SrcLoc
type LDefinition      = Definition SrcLoc
type LFamily          = Family SrcLoc
type LFeature         = Feature SrcLoc
type LDecomposition   = Decomposition SrcLoc
type LDecompOp        = DecompOp SrcLoc
type LConstraint      = Constraint SrcLoc
type LFeatureRef      = FeatureRef SrcLoc
type LInstance        = Instance SrcLoc
type LRewards         = Rewards SrcLoc
type LReward          = Reward SrcLoc
type LInit            = Init SrcLoc
type LInvariant       = Invariant SrcLoc
type LController      = Controller SrcLoc
type LModule          = Module SrcLoc
type LModuleBody      = ModuleBody SrcLoc
type LVarDecl         = VarDecl SrcLoc
type LVarType         = VarType SrcLoc
type LCompoundVarType = CompoundVarType SrcLoc
type LSimpleVarType   = SimpleVarType SrcLoc
type LConstant        = Constant SrcLoc
type LFormula         = Formula SrcLoc
type LLabel           = Label SrcLoc
type LStmt            = Stmt SrcLoc
type LActionLabel     = ActionLabel SrcLoc
type LUpdate          = Update SrcLoc
type LAssign          = Assign SrcLoc
type LProperty        = Property SrcLoc
type LExpr            = Expr SrcLoc
type LSampleArg       = SampleArg SrcLoc
type LRewardProp      = RewardProp SrcLoc
type LBound           = Bound SrcLoc
type LRepeatable b    = Repeatable b SrcLoc
type LSome b          = Some b SrcLoc
type LForLoop b       = ForLoop b SrcLoc
type LName            = Name SrcLoc
type LRange           = Range SrcLoc

makePrisms ''Definition

instance Pretty (Model a) where
    pretty (Model t defs) = pretty t <> line <> line <>
        vsep (punctuate line $ fmap pretty defs) <> line

instance Pretty (Specification a) where
    pretty (Specification defs) =
        vsep (punctuate line $ fmap pretty defs) <> line

instance Pretty (Definition a) where
    pretty def = case def of
        FamilyDef     f -> pretty f
        FeatureDef    f -> pretty f
        ControllerDef c -> pretty c
        ModuleDef     m -> pretty m
        GlobalDef     g -> "global" <+> pretty g
        ConstDef      c -> pretty c
        FormulaDef    f -> pretty f
        LabelDef      l -> pretty l
        RewardsDef    r -> pretty r
        InitDef       i -> pretty i
        InvariantDef  i -> pretty i
        PropertyDef   p -> pretty p

instance Pretty (Family a) where
    pretty Family{..} = "family" <> line <> indent 4 body <> line <> "endfamily"
      where
        body       = paramList <> line <> constrList <> line <> featList
        paramList  = hsep (fmap pretty famParameters)
        constrList = case famConstraints of
            [] -> empty
            cs -> hsep (fmap constr cs)
        featList = case famFeatures of
            [] -> empty
            fs -> "features" <+> hsep (punctuate comma (fmap pretty fs)) <> semi
        constr c = "initial" <+> "constraint" <+> pretty c <> semi

instance Pretty (Feature a) where
    pretty Feature{..}
      | featIsRoot = "root" <+> "feature" <> line <> indent 4 body <> line <>
                     "endfeature"
      | otherwise  = "feature" <+> text featIdent <> prettyParams featParams <>
                     line <> indent 4 body <> line <> "endfeature"
      where
        body = attribList featAttributes <>
               pretty featDecomp <> line <>
               constrList <> line <> line <>
               blockList <>
               modList <> line <> line <>
               vsep (fmap pretty featRewards)
        attribList = \case
            [] -> empty
            as -> vsep (fmap pretty as) <> line <> line
        constrList = vsep (fmap pretty featConstraints)
        blockList
          | null featBlocking = empty
          | otherwise         = "block" <+> vsep (fmap pretty featBlocking)
        modList
          | null featModules = empty
          | otherwise        = "modules" <+>
            hang 4 (hsep . punctuate comma $ fmap pretty featModules) <> semi

instance Pretty (Decomposition a) where
    pretty (Decomposition decompOp cs _) = pretty decompOp <+> "of" <+>
        hang 4 (hsep . punctuate comma $ fmap pretty cs) <> semi

instance Pretty (DecompOp a) where
    pretty decomp = case decomp of
        AllOf       -> "all"
        OneOf       -> "one"
        SomeOf      -> "some"
        Group range -> prettyRange range

instance Pretty (Constraint a) where
    pretty (Constraint initial e _) =
        (if initial then "initial" else empty) <+>
        "constraint" <+> pretty e <> semi

instance Pretty (FeatureRef a) where
    pretty (FeatureRef optional inst alias cnt) =
        (if optional then "optional" else empty) <+>
        pretty inst <+> maybe empty (("as" <+>) . text) alias <>
        maybe empty (brackets . pretty) cnt

instance Pretty (Instance a) where
    pretty (Instance ident args _) = text ident <> prettyArgs args

instance Pretty (Rewards a) where
    pretty (Rewards ident rws _) =
        "rewards" <+> dquotes (text ident) <> line <>
        indent 4 (prettyRepeatable NoInline (PP.<$>) empty rws) <> line <>
        "endrewards"

instance Pretty (Reward a) where
    pretty (Reward action grd e _) =
        maybe empty (brackets . pretty) action <+>
        pretty grd <+> colon <+> pretty e <> semi

instance Pretty (Init a) where
    pretty (Init e _) =
        "init" <> line <> indent 4 (pretty e) <> line <> "endinit"

instance Pretty (Invariant a) where
    pretty (Invariant e _) =
        "invariant" <> line <> indent 4 (pretty e) <> line <> "endinvariant"

instance Pretty (Controller a) where
    pretty (Controller body) =
        "controller" <> line <>
        indent 4 (pretty body) <> line <> "endcontroller"

instance Pretty (Module a) where
    pretty (Module ident params body) =
        "module" <+> text ident <> prettyParams params <> line <>
        indent 4 (pretty body) <> line <> "endmodule"

instance Pretty (ModuleBody a) where
    pretty (ModuleBody decls stmts _) =
        vsep (fmap pretty decls) <> line <> line <>
        prettyRepeatable NoInline (PP.<$>) empty stmts

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
        IntVarType range -> prettyRange range

instance Pretty (Constant a) where
    pretty (Constant ct ident me _) =
        "const" <+> pretty ct <+> text ident <>
        case me of
            Just e  -> space <> equals <+> pretty e <> semi
            Nothing -> semi

instance Pretty ConstType where
    pretty ct = case ct of
        BoolConstType   -> "bool"
        IntConstType    -> "int"
        DoubleConstType -> "double"

instance Pretty (Formula a) where
    pretty (Formula ident params e _) =
        ("formula" <> prettyParams params) <+> text ident <+> equals <+>
        pretty e <> semi

instance Pretty (Label a) where
    pretty (Label ident e _) =
        "label" <+> dquotes (text ident) <+> equals <+> pretty e <> semi

instance Pretty (Property a) where
    pretty (Property (Just ident) e _) =
        dquotes (text ident) <> colon <+> pretty e <> semi
    pretty (Property Nothing e _) = pretty e <> semi

instance Pretty (Stmt a) where
    pretty (Stmt action grd upds _) =
        brackets (pretty action) <+> pretty grd <+> "->" <+>
        prettyRepeatable Inline (\l r -> l <+> "+" <+> r) "true" upds <> semi

instance Pretty (ActionLabel a) where
    pretty action = case action of
        ActActivate _   -> "activate"
        ActDeactivate _ -> "deactivate"
        Action n _      -> pretty n
        NoAction        -> empty

instance Pretty (Update a) where
    pretty (Update e asgns _) = prob e <>
        prettyRepeatable Inline (\l r -> l <+> "&" <+> r) "true" asgns
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
                prettyExpr prec' lhs <+>
                pretty binOp <+>
                prettyExpr prec' rhs
    CondExpr cond te ee _ -> parens' (prec > 0) $
        prettyExpr 1 cond <+> char '?' <+>
        prettyExpr 1 te <+> colon <+> prettyExpr 1 ee
    UnaryExpr (TempUnOp Exists) e' _ -> "E" <+> brackets (pretty e')
    UnaryExpr (TempUnOp Forall) e' _ -> "A" <+> brackets (pretty e')
    UnaryExpr (TempUnOp o)      e' _ -> parens (pretty o <+> pretty e')
    UnaryExpr unOpT e' _ ->
        let prec' = unOpPrec unOpT
        in parens' (prec >= prec') $ pretty unOpT <> prettyExpr prec' e'
    LoopExpr loop _       -> prettyLoop pretty Inline loop
    CallExpr e' args _    ->
        prettyExpr callPrec e' <>
        parens (hcat . punctuate comma $ fmap pretty args)
    NameExpr n _          -> pretty n
    FuncExpr func _       -> pretty func
    SampleExpr es _       ->
        "sample" <> parens (hcat . punctuate comma . fmap pretty $ es)
    FilterExpr fOp p ms _  ->
        "filter" <> parens (
            pretty fOp <> comma <+>
            pretty p <>
            maybe empty (\s -> comma <+> pretty s) ms)
    ArrayExpr es _ -> braces . hcat . punctuate comma . fmap pretty $ toList es
    ProbExpr bound e' _ ->
        "P" <> pretty bound <+> brackets (pretty e')
    SteadyExpr bound e' _ ->
        "S" <> pretty bound <+> brackets (pretty e')
    RewardExpr struct bound prop _ ->
        "R" <> maybe empty (braces . pretty) struct <> pretty bound <+>
        brackets (pretty prop)
    ConditionalExpr prop cond _ -> pretty prop <> brackets (pretty cond)
    QuantileExpr minMax v e' _ -> "quantile" <>
        parens (pretty minMax <+> text v <> comma <+> pretty e')
    LabelExpr ident _ -> dquotes $ text ident
    DecimalExpr d _   -> double d
    IntegerExpr i _   -> integer i
    BoolExpr True _   -> "true"
    BoolExpr False _  -> "false"
    MissingExpr _     -> "..."
  where
    parens' True  = parens
    parens' False = id

instance Pretty (RewardProp a) where
    pretty rOp = case rOp of
        Reachability prop -> "F" <+> pretty prop
        Cumulative t      -> "C<=" <> text t
        Instant t         -> "I=" <> text t
        Steady            -> "S"

instance Pretty (SampleArg a) where
    pretty (ArgExpr e)   = pretty e
    pretty (ArgString s) = dquotes (text s)

instance Pretty (Bound a) where
    pretty Bound{..} =
        pretty boundMinMax <> pretty boundOp <> prettyBounds boundExprs
      where
        prettyBounds = \case
            []  -> "?"
            [e] -> pretty e
            es  -> braces (hcat . punctuate comma $ fmap pretty es)

instance Pretty MinMax where
    pretty = \case
        Min -> "min"
        Max -> "max"

data LoopInline = Inline | NoInline

prettyRepeatable :: (Pretty (b a))
                 => LoopInline
                 -> (Doc -> Doc -> Doc)
                 -> Doc
                 -> Repeatable b a
                 -> Doc
prettyRepeatable inline sep' empty' (Repeatable xs) = case xs of
    [] -> empty'
    _  -> foldr1 sep' $ fmap ppSome xs
  where
    ppSome (One x)     = pretty x
    ppSome (Many loop) =
        prettyLoop (prettyRepeatable inline sep' empty') inline loop

prettyLoop :: (b a -> Doc) -> LoopInline -> ForLoop b a -> Doc
prettyLoop pp inline (ForLoop v range body _) =
    let opening = "for" <+> text v <+> "in" <+> prettyRange range
        closing = "endfor"
    in case inline of
        Inline   -> opening <+> pp body <+> closing
        NoInline -> opening <> line <> nest 4 (pp body) <> line <> closing

instance Pretty Function where
    pretty func = case func of
        FuncMin     -> "min"
        FuncMax     -> "max"
        FuncFloor   -> "floor"
        FuncCeil    -> "ceil"
        FuncPow     -> "pow"
        FuncMod     -> "mod"
        FuncLog     -> "log"
        FuncActive  -> "active"
        FuncIActive -> "iactive"
        FuncBinom   -> "binom"

instance Pretty (Name a) where
    pretty (Name name _) = hcat . punctuate dot . fmap qualifier . toList $ name
      where
        qualifier (ident, idx) =
            text ident <> maybe empty (brackets . pretty) idx

prettyArgs :: (Pretty a) => [a] -> Doc
prettyArgs [] = empty
prettyArgs xs = parens (hcat . punctuate comma $ fmap pretty xs)

prettyParams :: (Pretty a) => [a] -> Doc
prettyParams = prettyArgs

