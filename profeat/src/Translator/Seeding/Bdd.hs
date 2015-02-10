{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Generation of seeding code based on Binary Decision Diagrams.
module Translator.Seeding.Bdd
  ( genSeeding
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Bdd
import Data.Bdd.Builder                  ( Builder, Ref )
import qualified Data.Bdd.Builder as Bdd
import Data.Foldable                     ( toList )
import Data.Map                          ( Map, (!) )
import qualified Data.Map         as Map
import Data.Monoid
import Data.Sequence                     ( Seq )
import qualified Data.Sequence    as Seq
import Data.Tuple                        ( swap )

import Error
import Symbols
import Syntax
import Syntax.Util
import Translator.Common
import Translator.Constraints
import Translator.Names
import Translator.Seeding.Common

type Location = Integer

data SeedInfo = SeedInfo
  { _seedLoc     :: !Location
  , _seedVisited :: Map (Int, Bdd, Bdd) Integer
  , _seedStmts   :: Seq LStmt
  }

makeLenses ''SeedInfo

genSeeding :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
           => InitialConstraintSet
           -> m ([LStmt], Integer)
genSeeding initConstrs = do
    (varToFeature, featureToVar) <- seedingBddVars

    bdd <- seedingBdd featureToVar initConstrs
    when (isFalse bdd) $ throw noLoc NoProducts

    return $ genStmts varToFeature bdd

genStmts :: Map Variable FeatureContext -> Bdd -> ([LStmt], Integer)
genStmts varToFeature bdd =
    let initSeedInfo = SeedInfo 0 Map.empty Seq.empty
        SeedInfo locInitConf _ stmts =
            execState (runReaderT (go 0 bdd) locInitConf) initSeedInfo
    in (toList stmts, locInitConf)
  where
    go :: Int -> Bdd -> ReaderT Location (State SeedInfo) (Maybe Location)
    go var node = case viewNode node of
        Terminal False -> return Nothing
        _              -> do
            visited <- use seedVisited
            if var < variable' node
                then case Map.lookup (var, node, node) visited of -- skipped some variables, create pseudo node for var
                    Just loc -> return (Just loc)
                    Nothing  -> do
                        loc              <- freshLoc
                        Just locThenElse <- go (var + 1) node -- "go" cannot return Nothing in this case
                                                              -- since a call to "go locInitConf k node"
                                                              -- never reaches this branch if "node" is
                                                              -- the "Terminal 0".
                        seedVisited %= Map.insert (var, node, node) loc

                        emitStmt loc locThenElse var True
                        emitStmt loc locThenElse var False

                        return (Just loc)
                else case viewNode node of
                    Terminal _     -> Just <$> ask -- Terminal True
                    Decision _ t e -> case Map.lookup (var, t, e) visited of
                        Just loc -> return (Just loc)
                        Nothing
                          | isFalse t -> do -- optimization: if the only transition would assign 0 to var, skip the location
                                Just locElse <- go (var + 1) e -- "go" cannot return Nothing in this case
                                                               -- since we assume that the BDD is reduced.
                                                               -- It follows that not both t and e can be
                                                               -- the "Terminal 0", because then "node"
                                                               -- would be redundant
                                seedVisited %= Map.insert (var, t, e) locElse
                                return (Just locElse)
                          | otherwise -> do
                                loc      <- freshLoc
                                mLocThen <- go (var + 1) t
                                mLocElse <- go (var + 1) e

                                seedVisited %= Map.insert (var, t, e) loc

                                maybeEmitStmt loc mLocThen var True
                                maybeEmitStmt loc mLocElse var False

                                return (Just loc)

    maybeEmitStmt locFrom mLocTo var b = case mLocTo of
        Just locTo -> emitStmt locFrom locTo var b
        Nothing    -> return ()

    emitStmt locFrom locTo var b = seedStmts %=
        (genStmt locFrom locTo (varToFeature ! mkVariable var) b <|)

    variable' n = case viewNode n of
        Terminal _       -> varCount
        Decision var _ _ -> getVariable var

    varCount = Map.size varToFeature

    freshLoc = do
        loc <- use seedLoc
        seedLoc += 1
        return loc

genStmt :: Location -> Location -> FeatureContext -> Bool -> LStmt
genStmt locFrom locTo ctx b =
    let grd     = identExpr seedVarIdent noLoc `eq` intExpr locFrom
        upd     = Update Nothing (Repeatable [One asgnCtx, One asgnLoc]) noLoc
        asgnCtx = Assign (activeName ctx) (if b then 1 else 0) noLoc
        asgnLoc = Assign seedVarName (intExpr locTo) noLoc
    in Stmt NoAction grd (Repeatable [One upd]) noLoc

-- | Generate a 'Bdd' that represents all valid (initial) configurations of
-- the product line.
seedingBdd :: (Applicative m, MonadReader TrnsInfo m)
           => Map FeatureContext Variable
           -> InitialConstraintSet
           -> m Bdd
seedingBdd featureToVar (InitialConstraintSet initConstrs) = do
    root    <- view rootFeature
    constrs <- view constraints

    let bdd = Bdd.runBuilder $ do
                  fm <- featureModelBdd featureToVar (allContexts root)
                  cs <- conjunctionB . fmap (constraintBdd featureToVar) . toList $
                          constrs <> initConstrs
                  Bdd.readRef <$> fm `Bdd.and` cs
    return bdd


-- | A @featureModelBdd fs@ represents all valid configurations of the
-- product line consisting of features @fs@.
featureModelBdd :: Map FeatureContext Variable
                -> [FeatureContext]
                -> Builder s (Ref s Bdd)
featureModelBdd featureToVar = conjunctionB . fmap (featureBdd featureToVar)

-- | A @featureBdd f@ represents all valid configurations of the
-- subfeatures of @f@.
featureBdd :: Map FeatureContext Variable
           -> FeatureContext
           -> Builder s (Ref s Bdd)
featureBdd featureToVar ctx =
    let fs        = ctx^.this
        childCtxs = filterAtomicCtxs $ fs^..fsChildren.traverse.traverse
        confs = configurations (^.fsOptional)
                               (fs^.fsGroupCard)
                               (fs^..fsChildren.traverse)
        atomicCtxs = fmap filterAtomicCtxs confs
    in disjunctionB (fmap (mkClause featureToVar childCtxs) atomicCtxs)
  where
    filterAtomicCtxs = fmap (`extendContext` ctx) . filter (not . _fsMandatory)

mkClause :: Map FeatureContext Variable
         -> [FeatureContext]
         -> [FeatureContext]
         -> Builder s (Ref s Bdd)
mkClause featureToVar childCtxs selectedCtxs =
    conjunctionB (fmap mkLit childCtxs)
  where
    mkLit ctx
      | ctx `elem` selectedCtxs = mkProj ctx
      | otherwise               = Bdd.notB (mkProj ctx)
    mkProj ctx = Bdd.proj (featureToVar ! ctx)

-- | A @constraintBdd c@ represents the 'Constraint' @c@.
constraintBdd :: Map FeatureContext Variable
              -> ConstraintExpr
              -> Builder s (Ref s Bdd)
constraintBdd featureToVar = go . projectToAtomicSets where
    go = \case
        BinaryConstr binOp l r -> binOpB binOp (go l) (go r)
        UnaryConstr  unOp c    -> unOpB unOp (go c)
        FeatConstr   ctx       -> Bdd.proj (featureToVar ! ctx)
        BoolConstr   True      -> Bdd.trueB
        BoolConstr   False     -> Bdd.falseB

binOpB :: LogicBinOp
       -> Builder s (Ref s Bdd)
       -> Builder s (Ref s Bdd)
       -> Builder s (Ref s Bdd)
binOpB = \case
    LImpl -> Bdd.impliesB
    LEq   -> Bdd.xnorB
    LAnd  -> Bdd.andB
    LOr   -> Bdd.orB

unOpB :: LogicUnOp -> Builder s (Ref s Bdd) -> Builder s (Ref s Bdd)
unOpB LNot = Bdd.notB

conjunctionB, disjunctionB :: [Builder s (Ref s Bdd)] -> Builder s (Ref s Bdd)
conjunctionB = foldr Bdd.andB Bdd.trueB
disjunctionB = foldr Bdd.orB  Bdd.falseB

-- | Associate a 'Variable' to each atomic set root.
seedingBddVars :: (Applicative m, MonadReader TrnsInfo m)
               => m (Map Variable FeatureContext, Map FeatureContext Variable)
seedingBddVars = do
    root <- view rootFeature

    let (varAssocs, _) = execState (traverse mkVar (allContexts root)) ([], 0)
        varToFeature   = Map.fromList varAssocs
        featureToVar   = Map.fromList (fmap swap varAssocs)

    return (varToFeature, featureToVar)
  where
    mkVar ctx = unless (ctx^.this.fsMandatory) $ do
        var <- use _2; _2 += 1
        _1 %= ((mkVariable var, ctx):)

