{-# LANGUAGE RecordWildCards #-}

module Translator.Modules
  ( trnsModules
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Map ( assocs )
import Data.Maybe
import Data.Set ( member )
import Data.Traversable

import Error
import Symbols
import Syntax
import Syntax.Util
import Typechecker
import Types

import Translator.Common
import Translator.Invariant
import Translator.Names

trnsModules :: Trans [LDefinition]
trnsModules =
    fmap concat . forAllContexts $ \ctx -> local (scope .~ Local ctx) $
        for (ctx^.this.fsModules.to assocs) $ \(ident, body) ->
            ModuleDef <$> trnsModule ident body

trnsModule :: Ident -> LModuleBody -> Trans LModule
trnsModule ident body = do
    Local ctx <- view scope
    Module (moduleIdent ctx ident) [] [] <$> trnsModuleBody body

trnsModuleBody :: Translator LModuleBody
trnsModuleBody (ModuleBody decls (Repeatable ss) l) = do
    ss' <- fmap concat . for ss $ \(One stmt) -> do
        stmts' <- trnsStmt stmt
        return $ fmap One stmts'

    decls' <- trnsLocalVars decls

    return $ ModuleBody decls' (Repeatable ss') l

trnsLocalVars :: Translator [LVarDecl]
trnsLocalVars decls = do
    Local ctx <- view scope
    fmap (sortVarDeclsByLoc . concat) . for decls $ \decl ->
        let t = ctx^?!this.fsVars.at (declIdent decl)._Just.vsType
        in trnsVarDecl t decl

trnsStmt :: LStmt -> Trans [LStmt]
trnsStmt (Stmt action grd upds l) = do
    Local ctx <- view scope
    invs      <- view invariants

    upds' <- ones (trnsUpdate trnsAssign) upds

    let invGrd = genInvariantGuard invs (upds'^..ones)
    invGrd'   <- partialEval invGrd
    grd'      <- trnsExpr isBoolType grd

    actions' <- trnsActionLabel action

    let actGrd    = activeGuard ctx
        negActGrd = unaryExpr (LogicUnOp LNot) actGrd

    isNonBlocking <- case action of
        Action n _ -> do
            LabelInfo{..} <- getLabelInfo n
            return $ (liIdent, liIndex) `notElem` ctx^.this.fsBlocking
        _ -> return False

    fmap concat . for actions' $ \(action', labelSet) -> do
        let actGrd' = if localActivateLabel ctx `member` labelSet
                          then negActGrd
                          else actGrd

        return $ Stmt action' (actGrd' `lAnd` invGrd' `lAnd` grd') upds' l :
                [Stmt action' negActGrd (Repeatable []) l |
                 isNonBlocking && isNotMandatory ctx]
  where
    localActivateLabel ctx = LsReconf ctx ReconfActivate
    isNotMandatory         = isJust . atomicSetRoot

trnsAssign :: Translator LAssign
trnsAssign (Assign name e l) = trnsVarAssign name e l
trnsAssign (Activate _ l)    = throw l IllegalReconf
trnsAssign (Deactivate _ l)  = throw l IllegalReconf

