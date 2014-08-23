{-# LANGUAGE FlexibleContexts
           , LambdaCase
           , OverloadedStrings
           , TemplateHaskell #-}

module Translator.Controller
  ( trnsController
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Array ( Array, bounds, elems )
import Data.Foldable ( toList )
import Data.List ( genericTake, genericLength )
import Data.Maybe
import Data.Map ( Map, assocs, keys, singleton, unions )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Traversable

import Error
import Symbols
import Syntax
import Typechecker
import Types

import Translator.Common
import Translator.Constraints
import Translator.Names

data SeedInfo = SeedInfo
  { _seedLoc         :: !Integer
  , _seedVisited     :: Set FeatureContext
  , _seedConstraints :: Set ConstraintExpr
  }

makeLenses ''SeedInfo

seedInfo :: FeatureContext -> Set ConstraintExpr -> SeedInfo
seedInfo = SeedInfo 0 . Set.singleton

type Reconfiguration = Map FeatureContext ReconfType

trnsController :: InitialConstraintSet
               -> Trans (Maybe LDefinition, LabelSets)
trnsController initConstrs =
    flip runStateT Set.empty . local (scope .~ LocalCtrlr) $ do
        (decls, stmts, l) <- view controller >>= \case
            Just cts -> do
                body <- trnsControllerBody $ cts^.ctsBody
                let ModuleBody decls (Repeatable stmts) l = body
                return (decls, stmts, l)
            Nothing -> return ([], [], noLoc)

        actDecls       <- genActiveVars
        (seedStmts, i) <- genSeeding initConstrs

        let seedVar = VarDecl seedVarIdent
                      (SimpleVarType $ IntVarType (0, intExpr i))
                      (Just 0)
                      noLoc
            locGrd  = identExpr seedVarIdent noLoc `eq` intExpr i

        let decls'  = actDecls ++ decls
            stmts'  = fmap (prependLocGuard locGrd) stmts
            stmts'' = Repeatable (fmap One seedStmts ++ stmts')
            body'   = ModuleBody (seedVar:decls') stmts'' l

        return $ if null decls' && null stmts
            then Nothing
            else Just . ModuleDef $ Module "_controller" [] [] body'
      where
        prependLocGuard locGrd (One (Stmt action grd upds l)) =
            One $ Stmt action (locGrd `lAnd` grd) upds l
        prependLocGuard _ s = s

trnsControllerBody :: LModuleBody -> StateT LabelSets Trans LModuleBody
trnsControllerBody (ModuleBody decls stmts l) =
    ModuleBody <$> trnsLocalVars decls
               <*> ones trnsStmt stmts
               <*> pure l

trnsLocalVars :: (Applicative m, MonadReader TrnsInfo m, MonadEither Error m)
              => [LVarDecl]
              -> m [LVarDecl]
trnsLocalVars decls = do
    cts <- view controller
    fmap concat . for decls $ \decl ->
        let t = cts^?!_Just.ctsVars.at (declIdent decl)._Just.vsType
        in trnsVarDecl t decl

trnsStmt :: LStmt -> StateT LabelSets Trans LStmt
trnsStmt (Stmt action grd (Repeatable ss) l) = do
    res <- for ss $ \(One upd) -> do
        (upd', reconf) <- runWriterT (trnsUpdate trnsAssign upd)
        return (One upd', reconf)

    let upds'   = Repeatable $ fmap fst res
        reconfs = fmap snd res

    action' <- genActionLabel action (reconfsToLabelSet reconfs)
    grd'    <- trnsExpr isBoolType grd

    constrGrds  <- traverse constraintGuard reconfs
    let cardGrds = fmap cardGuards reconfs
        grd''    = conjunction $ grd' : cardGrds ++ constrGrds

    return $ Stmt action' grd'' upds' l
  where
    cardGuards reconf =
        let parentCtxs = toList . Set.fromList .
                         catMaybes . fmap parentContext $ keys reconf
        in conjunction $ fmap (cardGuard reconf) parentCtxs

    cardGuard reconf parentCtx =
        let fs             = parentCtx^.this
            (lower, upper) = fs^.fsGroupCard
            opt            = genericLength . filter (^.fsOptional) $
                             fs^..fsChildren.traverse.traverse

            childCtxs      = childContexts parentCtx
            mandChildCtxs  = filter (not . _fsOptional . thisFeature) childCtxs

            sumAll         = sumActiveExpr reconf childCtxs
            sumMand        = sumActiveExpr reconf mandChildCtxs

            grdLower       = intExpr (lower - opt) `lte` sumMand
            grdUpper       = sumAll `lte` intExpr upper
        in grdLower `lAnd` grdUpper

    sumActiveExpr reconf = sum . fmap active'
      where
        active' childCtx = case reconf^.at childCtx of
            Just ReconfActivate   -> 1
            Just ReconfDeactivate -> 0
            Nothing -> identExpr (activeIdent childCtx) noLoc

    constraintGuard reconf =
        conjunction .
        fmap (trnsConstraintExpr . specialize reconf) .
        filter (\c -> any (refersTo c) $ keys reconf) .
        toList <$>
        view constraints

    specialize reconf = transform $ \c -> case c of
        FeatConstr ctx -> maybe c
            (BoolConstr . view (from reconfType)) $ reconf^.at ctx
        _              -> c

trnsAssign :: ( Applicative m
              , MonadReader TrnsInfo m
              , MonadWriter Reconfiguration m
              , MonadEither Error m
              )
           => LAssign
           -> m LAssign
trnsAssign asgn = case asgn of
    Assign name e l   -> trnsVarAssign name e l
    Activate   name l -> reconf ReconfActivate   name l
    Deactivate name l -> reconf ReconfDeactivate name l
  where
    reconf rt name l = do
        ctx <- getFeature name
        when (ctx^.this.fsMandatory) $ throw l IllegalMandatoryReconf

        tell $ singleton ctx rt
        return $ reconfAssign rt ctx

    reconfAssign rt ctx =
        let e = case rt of
                    ReconfActivate   -> 1
                    ReconfDeactivate -> 0
        in Assign (activeName ctx) e noLoc

genActionLabel :: LActionLabel
               -> Set Label
               -> StateT LabelSets Trans LActionLabel
genActionLabel action ls = do
    lbl <- case action of
        Action n _      -> Set.singleton . Label <$> getLabelInfo n
        NoAction        -> return Set.empty
        ActActivate l   -> throw l IllegalReconfLabel
        ActDeactivate l -> throw l IllegalReconfLabel

    let ls' = ls `Set.union` lbl
    modify (Set.insert ls')

    return $ labelSetToAction ls'

genActiveVars :: (Functor m, MonadReader TrnsInfo m, MonadEither Error m)
              => m [LVarDecl]
genActiveVars = mapMaybe mkVarDecl . allContexts <$> view rootFeature where
    mkVarDecl ctx
      | ctx^.this.fsMandatory = Nothing
      | otherwise             =
        let ident = activeIdent ctx
            vt    = SimpleVarType $ IntVarType (0, 1)
            e     = Just 0
        in Just $ VarDecl ident vt e noLoc

genSeeding :: (Functor m, MonadReader TrnsInfo m, MonadEither Error m)
           => InitialConstraintSet -> m ([LStmt], Integer)
genSeeding (InitialConstraintSet initConstrs) = do
    rootCtx <- rootContext <$> view rootFeature
    constrs <- view constraints

    return . over _2 _seedLoc . runState (seed rootCtx) $
        seedInfo rootCtx (constrs `Set.union` initConstrs)

seed :: FeatureContext -> State SeedInfo [LStmt]
seed ctx = do
    let fs         = ctx^.this
        childFeats = fs^..fsChildren.traverse.traverse
        childCtxs  = childContexts ctx
        confs      = configurations _fsOptional
                                    (fs^.fsGroupCard)
                                    (fs^..fsChildren.traverse)
        allMand    = all _fsMandatory childFeats

    seedVisited %= Set.union (Set.fromList childCtxs)

    constrs <- applicableConstraints

    stmts <- if (isLeafFeature fs || allMand) && null constrs
        then return []
        else do
            stmts <- for confs (genStmt constrs childCtxs)
            seedLoc += 1
            return stmts

    stmts' <- concat <$> for childCtxs seed

    return $ stmts ++ stmts'
  where
    genStmt constrs childCtxs conf = do
        i <- use seedLoc
        let confCtxs  = fmap (`extendContext` ctx) conf
            constrs'  = fmap (specialize childCtxs confCtxs) constrs
            constrGrd = conjunction $ fmap trnsConstraintExpr constrs'
            locGrd    = identExpr seedVarIdent noLoc `eq` intExpr i
            grd       = if null constrs
                            then locGrd
                            else locGrd `lAnd` constrGrd
            upd   = genUpdate i confCtxs
        return $ Stmt NoAction grd (Repeatable [One upd]) noLoc

    genUpdate i ctxs =
        let ctxs' = filter (not . _fsMandatory . thisFeature) ctxs
            sAsgn = One $ Assign seedVarName (intExpr $ i + 1) noLoc
            asgns = fmap (\c -> One $ Assign (activeName c) 1 noLoc) ctxs'
        in Update Nothing (Repeatable $ sAsgn:asgns) noLoc

    specialize childCtxs chosenCtxs = transform $ \case
        FeatConstr ctx'
          | ctx' `elem` childCtxs -> BoolConstr (ctx' `elem` chosenCtxs)
        c -> c

    applicableConstraints = do
        visited <- use seedVisited
        (appConstrs, constrs') <-
            Set.partition (canEvalConstraint visited) <$> use seedConstraints

        seedConstraints .= constrs'
        return $ toList appConstrs

reconfsToLabelSet :: [Reconfiguration] -> Set Label
reconfsToLabelSet = Set.fromList . fmap (uncurry ReconfLabel) . assocs . unions

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

