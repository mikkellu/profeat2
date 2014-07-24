{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Translator.Common
  ( TrnsInfo(..)
  , trnsInfo
  , constraints

  , Trans
  , Translator

  , trnsModuleBody
  , trnsVarDecl
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Foldable ( toList )
import Data.List ( genericLength )
import Data.Map ( Map, keys, member, singleton )
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Traversable

import Error
import Symbols
import Syntax
import Typechecker
import Types

import Translator.Constraints
import Translator.Names

data TrnsInfo = TrnsInfo
  { _trnsSymbolTable :: SymbolTable
  , _trnsScope       :: !Scope
  , _constraints     :: Set Constraint
  }

makeLenses ''TrnsInfo

instance HasSymbolTable TrnsInfo where
    symbolTable = trnsSymbolTable

instance HasScope TrnsInfo where
    scope = trnsScope

trnsInfo :: SymbolTable -> Set Constraint -> TrnsInfo
trnsInfo symTbl = TrnsInfo symTbl Global

type Trans = ReaderT TrnsInfo (Either Error)

type Translator a = a -> Trans a

type Reconfiguration = Map FeatureContext ReconfType

trnsModuleBody :: Translator LModuleBody
trnsModuleBody (ModuleBody decls stmts l) =
    ModuleBody <$> trnsLocalVars decls
               <*> ones trnsStmt stmts
               <*> pure l

trnsLocalVars :: Translator [LVarDecl]
trnsLocalVars decls = do
    sc <- view scope
    case sc of -- TODO: refactor
        Local ctx -> fmap concat . for decls $ \decl ->
            let t = ctx^?!this.fsVars.at (declIdent decl)._Just.vsType
            in trnsVarDecl t decl
        LocalCtrlr -> fmap concat . for decls $ \decl -> do
            cts <- view controller
            let t = cts^?!_Just.ctsVars.at (declIdent decl)._Just.vsType
            trnsVarDecl t decl
        Global -> error "Translator.trnsLocalVars: called with Global scope"

trnsStmt :: Translator LStmt
trnsStmt (Stmt action grd (Repeatable ss) l) = do
    res <- for ss $ \(One upd) -> do
        (upd', reconf) <- runWriterT (trnsUpdate upd)
        return (One upd', reconf)

    let upds'   = Repeatable $ fmap fst res
        reconfs = fmap snd res

    action' <- trnsActionLabel action
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
        fmap (trnsConstraint . specialize reconf) .
        filter (\c -> any (refersTo c) $ keys reconf) .
        toList <$>
        view constraints

    specialize reconf = transform $ \c -> case c of
        FeatConstr ctx -> maybe c
            (BoolConstr . view (from reconfType)) $ reconf^.at ctx
        _              -> c

trnsUpdate :: LUpdate -> WriterT Reconfiguration Trans LUpdate
trnsUpdate (Update e asgns l) =
    Update <$> _Just (trnsExpr isNumericType) e
           <*> ones trnsAssign asgns
           <*> pure l

trnsAssign :: LAssign -> WriterT Reconfiguration Trans LAssign
trnsAssign asgn = do
    sc <- view scope
    case asgn of
        Assign name e l -> do
            si@(SymbolInfo symSc ident idx _) <- getSymbolInfo name

            unless (symSc == Global || symSc == sc) $
                throw l IllegalWriteAccess

            t <- siType si

            constTbl <- view constants
            when (ident `member` constTbl) $ throw l IllegalConstAssignment

            e' <- trnsExpr (`isAssignableTo` t) e
            i  <- _Just evalInteger idx

            let name' = fullyQualifiedName symSc ident i l
            return $ Assign name' e' l
        Activate   name l -> reconf ReconfActivate   name l
        Deactivate name l -> reconf ReconfDeactivate name l
  where
    reconf rt name l = do
        sc <- view scope
        unless (sc == LocalCtrlr) $ throw l IllegalReconf

        ctx <- getFeature name
        when (ctx^.this.fsMandatory) $ throw l IllegalMandatoryReconf

        tell $ singleton ctx rt
        return $ reconfAssign rt ctx

    reconfAssign rt ctx =
        let e = case rt of
                    ReconfActivate   -> 1
                    ReconfDeactivate -> 0
        in Assign (activeName ctx) e noLoc

trnsVarDecl :: Type -> LVarDecl -> Trans [LVarDecl]
trnsVarDecl t (VarDecl ident vt e l) = do
    sc <- view scope
    let mkIdent = fullyQualifiedIdent sc ident

    void $ _Just (checkInitialization t) e

    return $ case t of
        CompoundType (ArrayType (Just (lower, upper)) _) ->
            let CompoundVarType (ArrayVarType _ svt) = vt
            in flip fmap [lower .. upper] $ \i ->
                VarDecl (mkIdent $ Just i) (SimpleVarType svt) e l
        _ -> [VarDecl (mkIdent Nothing) vt e l]

trnsExpr :: (Applicative m, MonadReader TrnsInfo m, MonadEither Error m)
         => (Type -> Bool)
         -> LExpr
         -> m LExpr
trnsExpr p e = checkIfType_ p e *> go e
  where
    go (NameExpr name l) = do
        si <- getSymbolInfo name

        let ident' = fullyQualifiedIdent (siScope si) (siIdent si) Nothing
        trnsIndex (siSymbolType si) ident' (siIndex si) l
    go (CallExpr (FuncExpr FuncActive _) [NameExpr name _] _) =
        activeExpr . fst <$> getContext name
    go e' = plate go e'

trnsIndex :: (Applicative m, MonadReader TrnsInfo m, MonadEither Error m)
          => Type
          -> Ident
          -> Maybe LExpr
          -> SrcLoc
          -> m LExpr
trnsIndex (CompoundType (ArrayType (Just (lower, upper)) _)) ident (Just e) l = do
    isConst <- isConstExpr e
    if isConst
        then do
            i <- evalInteger e
            return $ identExpr (indexedIdent ident i) l
        else do
            e' <- trnsExpr isIntType e
            return $ foldr (cond e')
                           (identExpr (indexedIdent ident upper) noLoc)
                           [lower .. upper - 1]
  where
    cond idxExpr i elseExpr =
        CondExpr (idxExpr `eq` intExpr i)
                 (identExpr (indexedIdent ident i) noLoc)
                 elseExpr
                 noLoc
trnsIndex _ ident _ l = return $ identExpr ident l

activeExpr :: FeatureContext -> LExpr
activeExpr ctx =
    let ctxs = filter (not . _fsMandatory . thisFeature) $ parentContexts ctx
    in conjunction $ fmap isActive ctxs
  where
    isActive ctx' = let ident = activeIdent ctx' in identExpr ident noLoc `eq` 1

trnsActionLabel :: Translator LActionLabel
trnsActionLabel = return -- TODO: fully qualified label name for local labels

