{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}

module Translator.Common
  ( TrnsInfo(..)
  , trnsInfo
  , labelSets
  , invariants

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
  , activeExpr

  , partialEval
  , partialEvalExprs

  , labelSetToAction
  , labelSetName
  , labelIdent
  ) where

import Control.Arrow ( (&&&) )
import Control.Lens
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.Map ( (!), member, keysSet )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T

import Error
import Eval
import Symbols
import Syntax
import Syntax.Util
import Typechecker
import Types

import Translator.Invariant
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
  , _invariants      :: Invariants
  }

makeLenses ''TrnsInfo

instance HasSymbolTable TrnsInfo where
    symbolTable = trnsSymbolTable

instance HasScope TrnsInfo where
    scope = trnsScope

trnsInfo :: SymbolTable -> Invariants -> TrnsInfo
trnsInfo symTbl = TrnsInfo symTbl Global Set.empty

type Trans = ReaderT TrnsInfo (Either Error)

type Translator a = a -> Trans a

trnsVarDecl :: (MonadReader TrnsInfo m, MonadError Error m)
            => Type
            -> LVarDecl
            -> m [LVarDecl]
trnsVarDecl t (VarDecl ident vt e l) = do
    sc <- view scope
    let mkIdent = fullyQualifiedIdent sc ident

    void $ _Just (checkInitialization t) e
    vt' <- exprs (substituteParams <=< trnsExpr (const True)) vt

    return $ case t of
        CompoundType (ArrayType (Just (lower, upper)) _) ->
            let CompoundVarType (ArrayVarType _ svt') = vt'
            in flip fmap [lower .. upper] $ \i ->
                VarDecl (mkIdent $ Just i) (SimpleVarType svt') Nothing l
        CompoundType (ArrayType Nothing _) ->
            error "Translator.Common.trnsVarDecl: unevaluated type"
        _ -> [VarDecl (mkIdent Nothing) vt' Nothing l]

trnsUpdate :: (MonadReader TrnsInfo m, MonadError Error m)
           => (LAssign -> m LAssign)
           -> LUpdate
           -> m LUpdate
trnsUpdate trns (Update e asgns l) =
    Update <$> _Just (trnsExpr isNumericType) e
           <*> ones trns asgns
           <*> pure l

trnsVarAssign :: (MonadReader TrnsInfo m, MonadError Error m)
              => LName
              -> LExpr
              -> SrcLoc
              -> m LAssign
trnsVarAssign name e l = do
    sc <- view scope
    si@(SymbolInfo symSc ident idx _) <- getSymbolInfo name

    when (if isAttributeSymbol si
               then sc /= LocalCtrlr
               else symSc /= Global && symSc /= sc) $ throw l IllegalWriteAccess

    t <- siType si

    constTbl <- view constants
    when (ident `member` constTbl) $ throw l IllegalConstAssignment

    e' <- trnsExpr (`isAssignableTo` t) e
    i  <- _Just evalInteger idx

    let name' = fullyQualifiedName symSc ident i l
    return $ Assign name' e' l

trnsExpr :: (MonadReader TrnsInfo m, MonadError Error m)
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

trnsIndex :: (MonadReader TrnsInfo m, MonadError Error m)
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

trnsActionLabel :: (MonadReader TrnsInfo m, MonadError Error m)
                => LActionLabel
                -> m [(LActionLabel, Set LabelSymbol)]
trnsActionLabel action =
    actionToLabel action >>= \case
        Nothing  -> return [(NoAction, Set.empty)]
        Just lbl -> do
            lss <- toList <$> getLabelSetsFor lbl
            return $ case lss of
                [] | isReconfLabel lbl -> [] -- there is no such reconfiguration in controller; remove statement
                   | otherwise ->
                     [(Action (labelName lbl) noLoc, Set.singleton lbl)]
                _  -> fmap (labelSetToAction &&& id) lss
  where
    isReconfLabel = \case
        LsReconf _ _ -> True
        _            -> False

labelSetToAction :: Set LabelSymbol -> LActionLabel
labelSetToAction ls
  | Set.null ls = NoAction
  | otherwise   = Action (labelSetName ls) noLoc

getLabelSetsFor :: (MonadReader TrnsInfo m) => LabelSymbol -> m LabelSets
getLabelSetsFor lbl = do
    lss <- view labelSets
    return $ Set.filter (Set.member lbl) lss

actionToLabel :: (MonadReader TrnsInfo m, MonadError Error m)
              => LActionLabel
              -> m (Maybe LabelSymbol)
actionToLabel action = case action of
    ActActivate l   -> toReconfLabel ReconfActivate l
    ActDeactivate l -> toReconfLabel ReconfDeactivate l
    Action n _      -> Just . LsAction <$> getLabelInfo n
    NoAction        -> return Nothing
  where
    toReconfLabel rt l = do
        sc <- view scope
        case sc of
            Local ctx -> return . Just $ LsReconf ctx rt
            _         -> throw l IllegalReconfLabel

activeGuard :: FeatureContext -> LExpr
activeGuard = flip NameExpr noLoc . activeFormulaName

activeExpr :: FeatureContext -> LExpr
activeExpr ctx =
    let ctxs = filter (not . _fsMandatory . thisFeature) $ parentContexts ctx
    in view conjunction $ fmap isActive ctxs
  where
    isActive ctx' = let ident = activeIdent ctx' in identExpr ident noLoc `eq` 1

partialEval :: (MonadReader r m, MonadError Error m, HasSymbolTable r)
            => LExpr
            -> m LExpr
partialEval e = do
    val <- view constValues
    transformM (f val) e
  where
    f val e'
      | FuncExpr _ _ <- e' = return e'
      | otherwise          = do
        isConst <- isConstExpr e'
        if isConst
            then do
                v <- eval' val e'
                return (valueExpr v)
            else return e'

partialEvalExprs ::
       (MonadReader r m, MonadError Error m, HasSymbolTable r, HasExprs a)
    => a SrcLoc
    -> m (a SrcLoc)
partialEvalExprs = exprs partialEval

substituteParams :: (MonadReader r m, HasSymbolTable r) => LExpr -> m LExpr
substituteParams e = do
    val  <- view constValues
    fams <- view familySym
    return $ case fams of
        Nothing    -> e
        Just fams' -> transform (f (fams'^.famsParameters.to keysSet) val) e
  where
    f params val e' = case e' of
        NameExpr (viewSimpleName -> Just (ident, _, _)) _
          | ident `Set.member` params -> valueExpr (val ! (ident, 0))
        _ -> e'


labelSetName :: Set LabelSymbol -> LName
labelSetName ls = review _Ident (T.concat . fmap labelIdent $ toList ls, noLoc)

labelName :: LabelSymbol -> LName
labelName lbl = review _Ident (labelIdent lbl, noLoc)

labelIdent :: LabelSymbol -> Ident
labelIdent (LsAction li)     = labelInfoIdent li
labelIdent (LsReconf ctx rt) =
    contextIdent ctx <> ('_' `cons` reconfIdent rt)

