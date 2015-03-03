{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Translator.Controller
  ( trnsControllerDef
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Foldable ( toList )
import Data.List ( genericLength )
import Data.Maybe
import Data.Map ( Map, assocs, keys, singleton, unions )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Traversable

import Error
import Symbols
import Syntax
import Syntax.Util
import Typechecker
import Types

import Translator.Common
import Translator.Initial
import Translator.Invariant
import Translator.Names

type Reconfiguration = Map FeatureContext ReconfType

trnsControllerDef :: InitExprs -> Trans ([LDefinition], LabelSets)
trnsControllerDef initExprs = do
    (defs, lss)    <- trnsController initExprs
    activeFormulas <- genActiveFormulas
    return (defs ++ activeFormulas, lss)

trnsController :: InitExprs -> Trans ([LDefinition], LabelSets)
trnsController initExprs =
    flip runStateT Set.empty . local (scope .~ LocalCtrlr) $ do
        (decls, stmts, l) <- view controller >>= \case
            Just cts -> do
                body <- trnsControllerBody $ cts^.ctsBody
                let ModuleBody decls (Repeatable stmts) l = body
                return (decls, stmts, l)
            Nothing -> return ([], [], noLoc)

        root <- view rootFeature
        invs <- view invariants

        actDecls    <- genActiveVars
        attribDecls <- trnsAttributeVars

        let initDef' = genInit initExprs invs root
            decls'   = actDecls ++ attribDecls ++ decls
            body'    = ModuleBody decls' (Repeatable stmts) l

        confLbl <- genInitConfLabel

        return $ if hasSingleConfiguration root
            then if null decls && null stmts
                     then [ initDef' ]
                     else [ initDef'
                          , ModuleDef (Module controllerIdent [] [] (ModuleBody decls' (Repeatable stmts) l))
                          ]
            else [ initDef'
                 , ModuleDef (Module controllerIdent [] [] body')
                 ] ++ confLbl

trnsControllerBody :: LModuleBody -> StateT LabelSets Trans LModuleBody
trnsControllerBody (ModuleBody decls stmts l) =
    ModuleBody <$> trnsLocalVars decls
               <*> ones trnsStmt stmts
               <*> pure l

trnsLocalVars :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
              => [LVarDecl]
              -> m [LVarDecl]
trnsLocalVars decls = do
    cts <- view controller
    fmap concat . for (sortVarDeclsByLoc decls) $ \decl ->
        let t = cts^?!_Just.ctsVars.at (declIdent decl)._Just.vsType
        in trnsVarDecl t decl

trnsAttributeVars :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
                  => m [LVarDecl]
trnsAttributeVars = do
    globalAttribSyms <- filter _gsIsAttrib . toList <$> view globals
    globalDecls <- fmap concat . for globalAttribSyms $
        \(GlobalSymbol t _ decl) -> local (scope .~ Global) $ trnsVarDecl t decl

    localDecls <- fmap concat . forAllContexts $ \ctx ->
        fmap concat . for (ctx^.this.fsAttributes) $ \decl ->
            let t = ctx^?!this.fsVars.at (declIdent decl)._Just.vsType
            in local (scope .~ Local ctx) $ trnsVarDecl t decl

    return . sortVarDeclsByLoc $ globalDecls ++ localDecls

trnsStmt :: LStmt -> StateT LabelSets Trans LStmt
trnsStmt (Stmt action grd (Repeatable ss) l) = do
    res <- for ss $ \(One upd) -> do
        (upd', reconf) <- runWriterT (trnsUpdate trnsAssign upd)
        return (One upd', reconf)

    let upds'   = Repeatable $ fmap fst res
        reconfs = fmap snd res

    action' <- genActionLabel action (reconfsToLabelSet reconfs)
    grd'    <- trnsExpr isBoolType grd

    invs <- view invariants
    let cardGrd = view conjunction $ fmap cardGuards reconfs
        invGrd  = genInvariantGuard invs (upds'^..ones)

    cardGrd' <- partialEval cardGrd
    invGrd'  <- partialEval invGrd
    let grd''   = grd' `lAnd` cardGrd' `lAnd` invGrd'

    return $ Stmt action' grd'' upds' l
  where
    cardGuards reconf =
        let parentCtxs = toList . Set.fromList .
                         catMaybes . fmap parentContext $ keys reconf
        in view conjunction $ fmap (cardGuard reconf) parentCtxs

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

trnsAssign :: ( Applicative m
              , MonadReader TrnsInfo m
              , MonadWriter Reconfiguration m
              , MonadError Error m
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
               -> Set LabelSymbol
               -> StateT LabelSets Trans LActionLabel
genActionLabel action ls = do
    lbl <- case action of
        Action _ n _    -> Set.singleton . LsAction <$> getLabelInfo n
        NoAction        -> return Set.empty
        ActActivate l   -> throw l IllegalReconfLabel
        ActDeactivate l -> throw l IllegalReconfLabel

    let ls' = ls `Set.union` lbl
    modify (Set.insert ls')

    return $ labelSetToAction ls'

genActiveVars :: (Functor m, MonadReader TrnsInfo m, MonadError Error m)
              => m [LVarDecl]
genActiveVars = mapMaybe mkVarDecl . allContexts <$> view rootFeature where
    mkVarDecl ctx
      | ctx^.this.fsMandatory = Nothing
      | otherwise             =
        let ident = activeIdent ctx
            vt    = SimpleVarType $ IntVarType (0, 1)
            e     = Nothing
        in Just $ VarDecl ident vt e noLoc

reconfsToLabelSet :: [Reconfiguration] -> Set LabelSymbol
reconfsToLabelSet = Set.fromList . fmap (uncurry LsReconf) . assocs . unions

genActiveFormulas :: (Functor m, MonadReader r m, HasSymbolTable r)
                  => m [LDefinition]
genActiveFormulas = fmap genActiveFormula . allContexts <$> view rootFeature

genActiveFormula :: FeatureContext -> LDefinition
genActiveFormula ctx = FormulaDef Formula
  { frmIdent  = activeFormulaIdent ctx
  , frmParams = []
  , frmExpr   = activeExpr ctx
  , frmAnnot  = noLoc
  }

genInitConfLabel :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
                 => m [LDefinition]
genInitConfLabel =
    view initConfLabel >>= \case
        Just e -> do
            e' <- trnsExpr isBoolType e
            return . (:[]) $ LabelDef Label
              { lblIdent = initConfLabelIdent
              , lblExpr  = e'
              , lblAnnot = noLoc
              }
        Nothing -> return []

