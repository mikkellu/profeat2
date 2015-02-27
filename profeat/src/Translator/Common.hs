{-# LANGUAGE FlexibleContexts, LambdaCase, TemplateHaskell #-}

module Translator.Common
  ( TrnsInfo(..)
  , trnsInfo
  , labelSets
  , constraints

  , LabelSymbol(..)
  , LabelSets

  , Trans
  , Translator

  , trnsVarDecl
  , trnsUpdate
  , trnsVarAssign
  , trnsExpr
  , trnsActionLabel

  , activeGuard

  , labelSetToAction
  , labelSetName
  , labelIdent
  ) where

import Control.Applicative
import Control.Arrow ( (&&&) )
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
import Syntax.Util
import Typechecker
import Types

import Translator.Constraints
import Translator.Names

data LabelSymbol
  = LsAction !LabelInfo
  | LsReconf !FeatureContext !ReconfType
  deriving (Eq, Ord)

type LabelSets = Set (Set LabelSymbol)

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

trnsVarDecl :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
            => Type
            -> LVarDecl
            -> m [LVarDecl]
trnsVarDecl t (VarDecl ident vt e l) = do
    sc <- view scope
    let mkIdent = fullyQualifiedIdent sc ident

    void $ _Just (checkInitialization t) e
    vt' <- exprs (trnsExpr $ const True) vt

    return $ case t of
        CompoundType (ArrayType (Just (lower, upper)) _) ->
            let CompoundVarType (ArrayVarType _ svt') = vt'
            in flip fmap [lower .. upper] $ \i ->
                VarDecl (mkIdent $ Just i) (SimpleVarType svt') Nothing l
        CompoundType (ArrayType Nothing _) ->
            error "Translator.Common.trnsVarDecl: unevaluated type"
        _ -> [VarDecl (mkIdent Nothing) vt' Nothing l]

trnsUpdate :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
           => (LAssign -> m LAssign)
           -> LUpdate
           -> m LUpdate
trnsUpdate trns (Update e asgns l) =
    Update <$> _Just (trnsExpr isNumericType) e
           <*> ones trns asgns
           <*> pure l

trnsVarAssign :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
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

trnsExpr :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
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
        activeGuard <$> getFeature name
    go e' = plate go e'

trnsIndex :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
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

trnsActionLabel :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
                => LActionLabel
                -> m [(LActionLabel, Set LabelSymbol)]
trnsActionLabel action =
    actionToLabel action >>= \case
        Nothing  -> return [(NoAction, Set.empty)]
        Just lbl -> do
            lss <- toList <$> getLabelSetsFor lbl
            return $ case lss of
                [] -> [(Action NonBlocking (labelName lbl) noLoc, Set.singleton lbl)]
                _  -> fmap (labelSetToAction &&& id) lss

labelSetToAction :: Set LabelSymbol -> LActionLabel
labelSetToAction ls = Action NonBlocking (labelSetName ls) noLoc

getLabelSetsFor :: (MonadReader TrnsInfo m) => LabelSymbol -> m LabelSets
getLabelSetsFor lbl = do
    lss <- view labelSets
    return $ Set.filter (Set.member lbl) lss

actionToLabel :: (Applicative m, MonadReader TrnsInfo m, MonadError Error m)
              => LActionLabel
              -> m (Maybe LabelSymbol)
actionToLabel action = case action of
    ActActivate l   -> toReconfLabel ReconfActivate l
    ActDeactivate l -> toReconfLabel ReconfDeactivate l
    Action _ n _    -> Just . LsAction <$> getLabelInfo n
    NoAction        -> return Nothing
  where
    toReconfLabel rt l = do
        sc <- view scope
        case sc of
            Local ctx -> return . Just $ LsReconf ctx rt
            _         -> throw l IllegalReconfLabel

labelSetName :: Set LabelSymbol -> LName
labelSetName ls = review _Ident (T.concat . fmap labelIdent $ toList ls, noLoc)

labelName :: LabelSymbol -> LName
labelName lbl = review _Ident (labelIdent lbl, noLoc)

labelIdent :: LabelSymbol -> Ident
labelIdent (LsAction li)     = labelInfoIdent li
labelIdent (LsReconf ctx rt) =
    contextIdent ctx <> ('_' `cons` reconfIdent rt)

