module Translator.Modules
  ( trnsModules
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Map ( assocs )
import Data.Set ( member )
import Data.Traversable

import Error
import Symbols
import Syntax
import Types

import Translator.Common
import Translator.Names

trnsModules :: Trans [LDefinition]
trnsModules = do
    root <- view rootFeature
    fmap concat . for (allContexts root) $ \ctx -> local (scope .~ Local ctx) $
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
    fmap concat . for decls $ \decl ->
        let t = ctx^?!this.fsVars.at (declIdent decl)._Just.vsType
        in trnsVarDecl t decl

trnsStmt :: LStmt -> Trans [LStmt]
trnsStmt (Stmt action grd upds l) = do
    Local ctx <- view scope
    actions'  <- trnsActionLabel action

    for actions' $ \(action', labelSet) -> do
        let actGrd  = activeGuard ctx
            actGrd' = if localActivateLabel ctx `member` labelSet
                          then unaryExpr (LogicUnOp LNot) actGrd
                          else actGrd

        grd'  <- trnsExpr isBoolType grd
        upds' <- ones (trnsUpdate trnsAssign) upds

        return $
            Stmt action' (conjunction [operatingGuard, actGrd', grd']) upds' l
  where
    localActivateLabel ctx = ReconfLabel ctx ReconfActivate

trnsAssign :: Translator LAssign
trnsAssign (Assign name e l) = trnsVarAssign name e l
trnsAssign (Activate _ l)    = throw l IllegalReconf
trnsAssign (Deactivate _ l)  = throw l IllegalReconf

