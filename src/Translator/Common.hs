{-# LANGUAGE FlexibleContexts, LambdaCase, TemplateHaskell #-}

module Translator.Common
  ( TrnsInfo(..)
  , trnsInfo
  , labelSets
  , constraints

  , Label(..)
  , LabelSets

  , Trans
  , Translator

  , trnsVarDecl
  , trnsUpdate
  , trnsVarAssign
  , trnsExpr
  , trnsActionLabel

  , labelSetToAction
  , labelSetName
  , labelIdent
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.Map ( member )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T

import Error
import Symbols
import Syntax
import Typechecker
import Types

import Translator.Constraints
import Translator.Names

data Label
  = Label !LabelInfo
  | ReconfLabel !FeatureContext !ReconfType
  deriving (Eq, Ord)

type LabelSets = Set (Set Label)

data TrnsInfo = TrnsInfo
  { _trnsSymbolTable :: SymbolTable
  , _trnsScope       :: !Scope
  , _labelSets       :: LabelSets
  , _constraints     :: Set ConstraintExpr
  }

makeLenses ''TrnsInfo

instance HasSymbolTable TrnsInfo where
    symbolTable = trnsSymbolTable

instance HasScope TrnsInfo where
    scope = trnsScope

trnsInfo :: SymbolTable -> Set ConstraintExpr -> TrnsInfo
trnsInfo symTbl = TrnsInfo symTbl Global Set.empty

type Trans = ReaderT TrnsInfo (Either Error)

type Translator a = a -> Trans a

trnsVarDecl :: (Applicative m, MonadReader TrnsInfo m, MonadEither Error m)
            => Type
            -> LVarDecl
            -> m [LVarDecl]
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

trnsUpdate :: (Applicative m, MonadReader TrnsInfo m, MonadEither Error m)
           => (LAssign -> m LAssign)
           -> LUpdate
           -> m LUpdate
trnsUpdate trns (Update e asgns l) =
    Update <$> _Just (trnsExpr isNumericType) e
           <*> ones trns asgns
           <*> pure l

trnsVarAssign :: (Applicative m, MonadReader TrnsInfo m, MonadEither Error m)
              => LName
              -> LExpr
              -> SrcLoc
              -> m LAssign
trnsVarAssign name e l = do
    sc <- view scope
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

trnsActionLabel :: LActionLabel -> Trans [LActionLabel]
trnsActionLabel action =
    actionToLabel action >>= \case
        Nothing  -> return [NoAction]
        Just lbl -> do
            actions <- fmap labelSetToAction . toList <$> getLabelSetsFor lbl
            return $ case actions of
                [] -> [Action (labelName lbl) noLoc]
                _  -> actions

labelSetToAction :: Set Label -> LActionLabel
labelSetToAction ls = Action (labelSetName ls) noLoc

getLabelSetsFor :: Label -> Trans LabelSets
getLabelSetsFor lbl = do
    lss <- view labelSets
    return $ Set.filter (Set.member lbl) lss

actionToLabel :: LActionLabel -> Trans (Maybe Label)
actionToLabel action = case action of
    ActActivate l   -> toReconfLabel ReconfActivate l
    ActDeactivate l -> toReconfLabel ReconfDeactivate l
    Action n _      -> Just . Label <$> getLabelInfo n
    NoAction        -> return Nothing
  where
    toReconfLabel rt l = do
        sc <- view scope
        case sc of
            Local ctx -> return . Just $ ReconfLabel ctx rt
            _         -> throw l IllegalReconfLabel

labelSetName :: Set Label -> LName
labelSetName ls = review _Ident (T.concat . fmap labelIdent $ toList ls, noLoc)

labelName :: Label -> LName
labelName lbl = review _Ident (labelIdent lbl, noLoc)

labelIdent :: Label -> Ident
labelIdent (Label li)           = labelInfoIdent li
labelIdent (ReconfLabel ctx rt) =
    contextIdent ctx <> ('_' `cons` reconfIdent rt)

