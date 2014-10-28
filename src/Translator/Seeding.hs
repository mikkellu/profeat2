{-# LANGUAGE FlexibleContexts, LambdaCase, TemplateHaskell #-}

module Translator.Seeding
  ( genOperatingFormula
  , genInitConfLabel
  , genInitConfStmt
  , genSeedVar
  , genSeeding
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Array ( Array, bounds, elems )
import Data.Foldable ( toList )
import Data.List ( genericTake, genericLength )
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Traversable

import Error
import Symbols
import Syntax
import Syntax.Util
import Types

import Translator.Common
import Translator.Constraints
import Translator.Names

data SeedInfo = SeedInfo
  { _seedLoc         :: !Integer           -- ^ the next seeding step
  , _seedVisited     :: Set FeatureContext -- ^ visited roots of atomic sets
  , _seedConstraints :: Set ConstraintExpr -- ^ remaining constraints
  }

makeLenses ''SeedInfo

genOperatingFormula :: Maybe Integer -> LDefinition
genOperatingFormula i = FormulaDef Formula
  { frmIdent  = operatingIdent
  , frmParams = []
  , frmExpr   = maybe (BoolExpr True noLoc) seedVarGuard i
  , frmAnnot  = noLoc
  }

genInitConfLabel :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
                 => Integer
                 -> m LDefinition
genInitConfLabel i = do
    lbl <- _Just (trnsExpr isBoolType) =<< view initConfLabel
    return $ LabelDef Label
      { lblIdent = initConfLabelIdent
      , lblExpr  = fromMaybe (seedVarGuard $ i - 1) lbl
      , lblAnnot = noLoc
      }

genInitConfStmt :: (MonadReader r m, HasSymbolTable r)
                => Integer
                -> m (Maybe LStmt, Integer)
genInitConfStmt i = do
    root <- view rootFeature
    lbl  <- view initConfLabel
    return $ if hasSingleConfiguration root || isJust lbl
        then (Nothing, i)
        else let asgn = incSeedVar i
                 upd  = Update Nothing (Repeatable [One asgn]) noLoc
                 stmt = Stmt NoAction (seedVarGuard i) (Repeatable [One upd]) noLoc
             in (Just stmt, i + 1)

genSeedVar :: Integer -> LVarDecl
genSeedVar i = VarDecl
  { declIdent = seedVarIdent
  , declType  = SimpleVarType $ IntVarType (0, intExpr i)
  , declInit  = Just 0
  , declAnnot = noLoc
  }

seedVarGuard :: Integer -> LExpr
seedVarGuard = eq (identExpr seedVarIdent noLoc) . intExpr

genSeeding :: (MonadReader TrnsInfo m)
           => InitialConstraintSet
           -> m ([LStmt], Integer)
genSeeding (InitialConstraintSet initConstrs) = do
    root    <- view rootFeature
    constrs <- view constraints

    let seedInfo = SeedInfo 0 Set.empty (constrs `Set.union` initConstrs)

    return . over _2 _seedLoc . flip runState seedInfo . fmap concat $
        for (allContexts root) seed

seed :: FeatureContext -> State SeedInfo [LStmt]
seed ctx = do
    let fs         = ctx^.this
        childFeats = fs^..fsChildren.traverse.traverse
        atomicSets = filter (not . _fsMandatory) childFeats

    if null atomicSets
        then return []
        else seedAtomicSets ctx atomicSets

seedAtomicSets :: FeatureContext -> [FeatureSymbol] -> State SeedInfo [LStmt]
seedAtomicSets ctx atomicSets = do
    let fs    = ctx^.this
        confs = configurations (^.fsOptional)
                               (fs^.fsGroupCard)
                               (fs^..fsChildren.traverse)
        atomicCtxs = fmap (`extendContext` ctx) atomicSets

    seedVisited %= Set.union (Set.fromList atomicCtxs)

    constrs <- applicableConstraints

    stmts <- for confs (genStmt constrs atomicCtxs)
    seedLoc += 1
    return stmts
  where
    genStmt constrs atomicCtxs conf = do
        i <- use seedLoc
        let confCtxs  = fmap (`extendContext` ctx) conf
            constrs'  = fmap (specialize atomicCtxs confCtxs) constrs
            constrGrd = conjunction $ fmap trnsConstraintExpr constrs'
            locGrd    = identExpr seedVarIdent noLoc `eq` intExpr i
            grd       = locGrd `lAnd` constrGrd
            upd       = genUpdate i confCtxs
        return $ Stmt NoAction grd (Repeatable [One upd]) noLoc

    genUpdate i ctxs =
        let ctxs' = filter (not . _fsMandatory . thisFeature) ctxs
            sAsgn = One (incSeedVar i)
            asgns = fmap (\c -> One $ Assign (activeName c) 1 noLoc) ctxs'
        in Update Nothing (Repeatable $ sAsgn:asgns) noLoc

    specialize atomicCtxs chosenCtxs = transform $ \case
        FeatConstr ctx' -> case atomicSetRoot ctx' of
            Nothing -> BoolConstr True
            Just ctx''
              | ctx'' `elem` atomicCtxs -> BoolConstr (ctx'' `elem` chosenCtxs)
              | otherwise               -> FeatConstr ctx''
        c -> c

    applicableConstraints = do
        visited <- use seedVisited
        (appConstrs, constrs') <-
            Set.partition (canEvalConstraint visited) <$> use seedConstraints

        seedConstraints .= constrs'
        return $ toList appConstrs

incSeedVar :: Integer -> LAssign
incSeedVar i = Assign seedVarName (intExpr $ i + 1) noLoc

configurations :: (a -> Bool)
               -> (Integer, Integer)
               -> [Array Integer a]
               -> [[a]]
configurations isOptional (lower, upper) as = filter valid . subsequences $ as
  where
    opt = genericLength . filter isOptional $ as^..traverse.traverse
    valid xs = let cnt  = genericLength xs
                   mand = genericLength . filter (not . isOptional) $ xs
               in lower - opt <= mand && cnt <= upper

subsequences :: [Array Integer a] -> [[a]]
subsequences (a:as) = let (lower, upper) = bounds a in do
    ss <- subsequences as
    i  <- enumFromTo lower (upper + 1)
    return $ genericTake i (elems a) ++ ss
subsequences [] = return []

