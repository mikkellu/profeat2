{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}

module Translator.Controller
  ( trnsController
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

import Symbols
import Syntax

import Translator.Common
import Translator.Constraints
import Translator.Names

data SeedInfo = SeedInfo
  { _seedLoc         :: !Integer
  , _seedVisited     :: Set FeatureContext
  , _seedConstraints :: Set Constraint
  }

makeLenses ''SeedInfo

seedInfo :: FeatureContext -> Set Constraint -> SeedInfo
seedInfo = SeedInfo 0 . Set.singleton

trnsController :: Trans (Maybe LDefinition)
trnsController = local (scope .~ LocalCtrlr) $ do
    (decls, stmts, l) <- view controller >>= \case
        Just cts -> do
            body <- trnsModuleBody $ cts^.ctsBody
            let ModuleBody decls (Repeatable stmts) l = body
            return (decls, stmts, l)
        Nothing -> return ([], [], noLoc)

    actDecls       <- genActiveVars
    (seedStmts, i) <- genSeeding

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

genActiveVars :: Trans [LVarDecl]
genActiveVars = mapMaybe mkVarDecl . allContexts <$> view rootFeature where
    mkVarDecl ctx
      | ctx^.this.fsMandatory = Nothing
      | otherwise             =
        let ident = activeIdent ctx
            vt    = SimpleVarType $ IntVarType (0, 1)
            e     = Just 0
        in Just $ VarDecl ident vt e noLoc

genSeeding :: Trans ([LStmt], Integer)
genSeeding = do
    rootCtx <- rootContext <$> view rootFeature
    constrs <- view constraints

    return . over _2 _seedLoc . runState (seed rootCtx) $
        seedInfo rootCtx constrs

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
            constrGrd = conjunction $ fmap trnsConstraint constrs'
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

