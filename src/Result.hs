{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Result
  ( Result(..)

  , StateVec
  , VariablesRaw

  , StateResult(..)
  , srIndex
  , srStateVec
  , srResult

  , ResultCollection(..)
  , rcVariables
  , rcStateResults
  , rcFinalResult
  , rcTrace
  , rcLog
  , rcDdNodes
  , rcBuildingTime
  , rcCheckingTime
  , emptyResultCollection
  , appendResultCollection

  , sortStateResults
  , removeNonConfVars
  , roundStateResults

  , toVarOrder
  , prettyResultCollections
  , prettyVal
  ) where

import Prelude hiding ( (<$>) )
import Control.Lens hiding ( (:<) )
import Control.Applicative          ( (<|>), liftA2 )

import Data.Foldable                ( toList )
import Data.List                    ( elemIndex )
import qualified Data.Map as Map
import Data.Maybe                   ( fromMaybe, mapMaybe )
import Data.Ord                     ( comparing )
import Data.Sequence                ( Seq, ViewL(..), viewl )
import qualified Data.Sequence as Seq
import Data.Strict.Tuple            ( (:!:), Pair(..) )
import Data.Strict.Tuple.Lens       ()
import qualified Data.Strict.Tuple as ST

import Data.Text                    ( Text )
import Data.Text.Lazy               ( fromStrict )
import qualified Data.Text.Lazy as L
import Data.Vector.Generic          ( Vector, (!) )
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV

import Text.PrettyPrint.Leijen.Text

import Syntax hiding ( Range )
import VarOrder

data Result
  = ResultBool !Bool
  | ResultDouble !Double
  | ResultRange !Double !Double
  deriving (Eq, Ord, Show)

instance Semigroup Result where
    ResultBool x <> ResultBool y = ResultBool (x && y)
    ResultDouble x <> ResultDouble y
      | x < y     = ResultRange x y
      | x > y     = ResultRange y x
      | otherwise = ResultDouble x
    ResultRange xl xu <> ResultDouble y = ResultRange (min y xl) (max y xu)
    ResultDouble x <> ResultRange yl yu = ResultRange (min x yl) (max x yu)
    ResultRange xl xu <> ResultRange yl yu = ResultRange (min xl yl) (max xu yu)
    _ <> _ = error "Result.<>: type error"

instance Pretty Result where
    pretty = \case
        ResultBool False        -> "false"
        ResultBool True         -> "true"
        ResultDouble d          -> double d
        ResultRange lower upper ->
            brackets (double lower <> comma <> double upper)

type StateVec = UV.Vector Int

type VariablesRaw = [Text]

data StateResult = StateResult
  { _srIndex    :: !Int
  , _srStateVec :: !StateVec
  , _srResult   :: !Result
  } deriving (Show)

makeLenses ''StateResult

data ResultCollection = ResultCollection
  { _rcVariables           :: VariablesRaw
  , _rcStateResults        :: !(Seq StateResult)
  , _rcFinalResult         :: Maybe Result
  , _rcTrace               :: !(Seq StateVec)
  , _rcLog                 :: !(Seq Text)
  , _rcDdNodes             :: !(Seq (Int :!: Int))
  , _rcBuildingTime        :: !Double
  , _rcCheckingTime        :: !Double
  } deriving (Show)

makeLenses ''ResultCollection

emptyResultCollection :: VariablesRaw -> ResultCollection
emptyResultCollection vars = ResultCollection
  { _rcVariables           = vars
  , _rcStateResults        = Seq.empty
  , _rcFinalResult         = Nothing
  , _rcTrace               = Seq.empty
  , _rcLog                 = Seq.empty
  , _rcDdNodes             = Seq.empty
  , _rcBuildingTime        = 0.0
  , _rcCheckingTime        = 0.0
  }

appendResultCollection :: ResultCollection
                       -> ResultCollection
                       -> ResultCollection
appendResultCollection (ResultCollection xVars xSrs xR xTr xL xNs xBt xCt)
                       (ResultCollection yVars ySrs yR yTr yL yNs yBt yCt) =
    let idxm  = indexMap xVars yVars
        ySrs' = over (traverse.srStateVec) (reorderStateVec idxm) ySrs
        yTr'  = over traverse (reorderStateVec idxm) yTr
        r = liftA2 (<>) xR yR <|> xR <|> yR
    in ResultCollection xVars (xSrs <> ySrs') r (xTr <> yTr') (xL <> yL)
          (xNs <> yNs) (xBt + yBt) (xCt + yCt)

type IndexMap = UV.Vector Int

indexMap :: VariablesRaw -> VariablesRaw -> (Int, IndexMap)
indexMap source target = (len, UV.generate len gen)
  where
    len = length source
    gen i =
        fromMaybe
            (error "Result.reorderingIndices: incompatible list of variables")
            (elemIndex (source !! i) target)


reorderStateVec :: (Int, IndexMap) -> StateVec -> StateVec
reorderStateVec (len, idxs) sv = UV.generate len (\i -> sv ! (idxs ! i))

sortStateResults :: ResultCollection -> ResultCollection
sortStateResults = rcStateResults %~ Seq.sortBy (comparing (view srResult))

roundStateResults :: Int -> ResultCollection -> ResultCollection
roundStateResults precision =
    rcStateResults %~ fmap (over srResult $ roundResult precision)

roundResult :: Int -> Result -> Result
roundResult precision r = case r of
    ResultDouble d -> ResultDouble (round' precision d)
    _              -> r

round' :: Int -> Double -> Double
round' precision d =
    (fromInteger . round $ d * (10^precision)) / (10.0^^precision)

data VarRole
  = VarConf
  | VarNonConf
  deriving (Eq)

removeNonConfVars :: ResultCollection -> ResultCollection
removeNonConfVars rc =
    let srs  = rc^.rcStateResults
        vrs  = findConfVars srs
        vs   = rc^.rcVariables
        vs'  = fmap fst . filter ((VarConf ==) . snd) . zip vs $ vrs
        srs' = filterConfVars vrs srs
    in rc & rcVariables .~ vs'
          & rcStateResults .~ srs'

filterConfVars :: [VarRole]
               -> Seq StateResult
               -> Seq StateResult
filterConfVars vrs = fmap go where
    go            = over srStateVec filterVals
    confIndices   = fmap fst . filter ((VarConf ==) . snd) . zip [0..] $ vrs
    filterVals sv = let confVals = fmap (sv !) confIndices
                    in V.fromList confVals

findConfVars :: Seq StateResult -> [VarRole]
findConfVars (viewl -> StateResult _ sv _ :< srs) =
    fmap go (enumFromTo 0 (V.length sv - 1))
  where
    go :: Int -> VarRole
    go i = let v = sv ! i
           in if all ((v ==) . (! i) . _srStateVec) srs
                  then VarNonConf
                  else VarConf
findConfVars _ = []

-- Pretty Printing

toVarOrder :: VarMap -> VariablesRaw -> VarOrder
toVarOrder vm vars = VarOrder . flip fmap vars $ \var ->
    fromMaybe (text (fromStrict var), Range 0 0) (Map.lookup var vm)

prettyResultCollections
    :: VarMap -> Bool -> Specification a -> [ResultCollection] -> Doc
prettyResultCollections vm includeLog (Specification defs) rcs =
    let props = defs^..traverse._PropertyDef
    in vsep . punctuate separator . fmap p $ zip props rcs
  where
    p (def, rc) = pretty def <> line <> line <>
                  prettyResultCollection vm includeLog rc
    separator = line <> line <> text (L.replicate 80 "-") <> line

prettyResultCollection :: VarMap -> Bool -> ResultCollection -> Doc
prettyResultCollection vm includeLog ResultCollection{..} =
    (if includeLog then prettyLog _rcLog <> line <> line else empty) <>
    maybe empty (("Final result:" <+>) . pretty) _rcFinalResult <$>
    stateResults <$>
    prettyTrace <$>
    line <>
    prettyTrnsNodes <$>
    "Time for model construction:" <+> pretty _rcBuildingTime <$>
    "Time for model checking:" <+> pretty _rcCheckingTime
  where
    stateResults
      | Seq.null _rcStateResults = empty
      | otherwise =
        "Results for initial configurations:" <$>
        indent 4 (prettyStateResults varOrder _rcStateResults)

    prettyTrace
      | Seq.null _rcTrace = empty
      | otherwise =
        "Counterexample/witness:" <$>
        indent 4 (prettyStateVecs varOrder _rcTrace)

    prettyTrnsNodes = case viewl _rcDdNodes of
        EmptyL -> empty
        n :< (viewl -> EmptyL) -> "Transition matrix:" <+> prettyNumDdNodes n
        ns -> "Transition matrices:" <+>
              int (sum (fmap ST.fst ns)) <+> "(sum)" <$>
              (indent 4 . hsep . punctuate comma . fmap (int . ST.fst) $ toList ns)
    prettyNumDdNodes (n :!: nt) =
        int n <+> "nodes" <+> parens (int nt <+> "terminal")
    varOrder = toVarOrder vm _rcVariables

prettyLog :: Seq Text -> Doc
prettyLog = vsep . fmap (text . L.fromStrict) . toList

prettyStateResults :: VarOrder -> Seq StateResult -> Doc
prettyStateResults vo = vsep . fmap (prettyStateResult vo) . toList

prettyStateResult :: VarOrder -> StateResult -> Doc
prettyStateResult vo (StateResult i sv r) = int i <> colon <>
    parens (prettyStateVec vo sv) <> char '=' <> pretty r

prettyStateVecs :: Vector v Int => VarOrder -> Seq (v Int) -> Doc
prettyStateVecs vo = vsep . fmap (parens . prettyStateVec vo) . toList

prettyStateVec :: Vector v Int => VarOrder -> v Int -> Doc
prettyStateVec (VarOrder vo) =
    hsep . punctuate comma . mapMaybe (uncurry prettyVal) . zip vo . V.toList

prettyVal :: (Doc, Range) -> Int -> Maybe Doc
prettyVal (ident, r) v = case r of
    RangeFeature _
      | v == 0    -> Nothing
      | v == 1    -> Just ident
      | otherwise -> error "Result.prettyVal: illegal value for feature variable"
    RangeBool
      | v == 0    -> identDef "false"
      | v == 1    -> identDef "true"
      | otherwise -> error "Result.prettyVal: illegal value for boolean variable"
    Range _ _     -> identDef (int v)
  where
    identDef doc = Just $ ident <> char '=' <> doc
