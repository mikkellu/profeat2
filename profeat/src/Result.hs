{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Result
  ( Result(..)

  , StateVec

  , ResultCollection(..)
  , rcStateResults
  , rcGroupedStateResults
  , rcFinalResult
  , rcTrace
  , rcLog
  , rcBuildingTime
  , rcCheckingTime
  , emptyResultCollection
  , appendResultCollection

  , sortStateResults
  , removeNonConfVars
  , roundStateResults
  , groupStateVecs

  , prettyStateVec
  , prettyResultCollection
  , prettyResultCollections
  ) where

import Control.Lens

import Data.Foldable                ( toList )
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Maybe                   ( mapMaybe)
import Data.Monoid                  ( mappend )
import Data.Ord                     ( comparing )
import Data.Sequence                ( Seq, ViewL(..), viewl )
import qualified Data.Sequence as Seq
import Data.IntSet                  ( IntSet, member, findMin, findMax, singleton, size )
import qualified Data.IntSet as Set
import Data.Strict.Tuple            ( (:!:), Pair(..) )
import Data.Strict.Tuple.Lens       ()
import qualified Data.Strict.Tuple as ST

import Data.Text                    ( Text )
import qualified Data.Text.Lazy as L
import Data.Vector.Generic          ( Vector, (!) )
import qualified Data.Vector.Generic as V
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV

import Text.PrettyPrint.Leijen.Text

import Analysis.VarOrdering
import Syntax hiding ( Range )

data Result
  = ResultBool !Bool
  | ResultDouble !Double
  | ResultRange !Double !Double
  deriving (Eq, Ord, Show)

instance Pretty Result where
    pretty = \case
        ResultBool False        -> "false"
        ResultBool True         -> "true"
        ResultDouble d          -> double d
        ResultRange lower upper ->
            brackets (double lower <> comma <> double upper)

type StateVec = UV.Vector Int

type GroupedStateVec = BV.Vector IntSet

data ResultCollection = ResultCollection
  { _rcVarOrdering         :: !VarOrdering
  , _rcStateResults        :: !(Seq (StateVec :!: Result))
  , _rcGroupedStateResults :: !(Seq (GroupedStateVec :!: Result))
  , _rcFinalResult         :: !Result
  , _rcTrace               :: !(Seq StateVec)
  , _rcLog                 :: !(Seq Text)
  , _rcBuildingTime        :: !Double
  , _rcCheckingTime        :: !Double
  } deriving (Show)

makeLenses ''ResultCollection

emptyResultCollection :: VarOrdering -> ResultCollection
emptyResultCollection vo = ResultCollection
  { _rcVarOrdering         = vo
  , _rcStateResults        = Seq.empty
  , _rcGroupedStateResults = Seq.empty
  , _rcFinalResult         = ResultBool False
  , _rcTrace               = Seq.empty
  , _rcLog                 = Seq.empty
  , _rcBuildingTime        = 0.0
  , _rcCheckingTime        = 0.0
  }

appendResultCollection :: ResultCollection
                       -> ResultCollection
                       -> ResultCollection
appendResultCollection (ResultCollection xVo xSrs xGrs xR xTr xL xBt xCt)
                       (ResultCollection _   ySrs yGrs yR _   yL yBt yCt) =
    let r = case (xR, yR) of
                (ResultBool x, ResultBool y) -> ResultBool (x && y)
                (ResultDouble x, ResultDouble y)
                  | x < y     -> ResultRange x y
                  | x > y     -> ResultRange y x
                  | otherwise -> ResultDouble x
                (ResultRange xl xu, ResultDouble y) ->
                    ResultRange (min y xl) (max y xu)
                (ResultDouble x, ResultRange yl yu) ->
                    ResultRange (min x yl) (max x yu)
                (ResultRange xl xu, ResultRange yl yu) ->
                    ResultRange (min xl yl) (max xu yu)
                _ -> error "Result.appendResultCollection: incompatible collections"
    in ResultCollection xVo (mappend xSrs ySrs) (mappend xGrs yGrs) r xTr
           (mappend xL yL) (xBt + yBt) (xCt + yCt)

sortStateResults :: ResultCollection -> ResultCollection
sortStateResults = rcStateResults %~ Seq.sortBy (comparing (view _2'))

groupStateVecs :: ResultCollection -> ResultCollection
groupStateVecs rc =
    let srs    = rc^.rcStateResults
        groups = F.foldr insert Map.empty (fmap toGroupedStateResult srs)
        gsrs   = mapToSeq groups
    in rc & rcStateResults        .~ Seq.empty
          & rcGroupedStateResults .~ gsrs
  where
    insert (gsv :!: r) = Map.insertWith (V.zipWith mappend) r gsv
    mapToSeq = Seq.fromList . fmap (\(r, gsv) -> gsv :!: r) . Map.assocs

toGroupedStateResult :: (StateVec :!: Result) -> (GroupedStateVec :!: Result)
toGroupedStateResult = over _1' (V.map singleton . V.convert)

roundStateResults :: Int -> ResultCollection -> ResultCollection
roundStateResults precision =
    rcStateResults %~ fmap (over _2' $ roundResult precision)

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
    let srs            = rc^.rcStateResults
        vrs            = findConfVars srs
        VarOrdering vs = rc^.rcVarOrdering
        vs'            = fmap fst . filter ((VarConf ==) . snd) . zip vs $ vrs
        srs'           = filterConfVars vrs srs
    in rc & rcVarOrdering  .~ VarOrdering vs'
          & rcStateResults .~ srs'

filterConfVars :: [VarRole]
               -> Seq (StateVec :!: Result)
               -> Seq (StateVec :!: Result)
filterConfVars vrs = fmap go where
    go            = over _1' filterVals
    confIndices   = fmap fst . filter ((VarConf ==) . snd) . zip [0..] $ vrs
    filterVals sv = let confVals = fmap (sv !) confIndices
                    in V.fromList confVals

findConfVars :: Seq (StateVec :!: Result) -> [VarRole]
findConfVars (viewl -> (sv :!: _) :< srs) =
    fmap go (enumFromTo 0 (V.length sv - 1))
  where
    go :: Int -> VarRole
    go i = let v = sv ! i
           in if F.all ((v ==) . (! i) . ST.fst) srs
                  then VarNonConf
                  else VarConf
findConfVars _ = []

-- Pretty Printing

prettyResultCollections :: Bool -> Specification a -> [ResultCollection] -> Doc
prettyResultCollections includeLog (Specification defs) rcs =
    let props = defs^..traverse._PropertyDef
    in vsep . punctuate separator . fmap p $ zip props rcs
  where
    p (def, rc) = pretty def <> line <> line <>
                  prettyResultCollection includeLog rc
    separator = line <> line <> text (L.replicate 80 "-") <> line

prettyResultCollection :: Bool -> ResultCollection -> Doc
prettyResultCollection includeLog (ResultCollection vo srs gsrs r tr ls bt ct) =
    (if includeLog then prettyLog ls <> line <> line else empty) <>
    "Final result:" <+> pretty r <$>
    stateResults <$>
    groupedStateResults <$>
    prettyTrace vo tr <$>
    "Time for model construction:" <+> pretty bt <$>
    "Time for model checking:" <+> pretty ct
  where
    stateResults | Seq.null srs = empty
                 | otherwise    = "Results for initial configurations:" <$>
                                  prettyStateResults prettyVal vo srs
    groupedStateResults
      | Seq.null gsrs = empty
      | otherwise     = "Results for initial configurations" <+>
                        "(grouped by result)" <$>
                        prettyStateResults prettyValGroup vo gsrs

prettyLog :: Seq Text -> Doc
prettyLog = vsep . fmap (text . L.fromStrict) . toList

prettyTrace :: VarOrdering -> Seq StateVec -> Doc
prettyTrace vo svs
  | Seq.null svs  = empty
  | otherwise     = "Counterexample/witness:" <$>
                    prettyStateVecs prettyVal vo svs

prettyStateResults :: Vector v a
                   => ValPrinter a
                   -> VarOrdering
                   -> Seq (v a :!: Result)
                   -> Doc
prettyStateResults f vo =
    vsep . fmap (ST.uncurry $ prettyStateResult f vo) . toList

prettyStateResult :: Vector v a
                  => ValPrinter a
                  -> VarOrdering
                  -> v a
                  -> Result
                  -> Doc
prettyStateResult f vo sv r =
    parens (prettyStateVec f vo sv) <> char '=' <> pretty r

type ValPrinter a = (Doc, Range) -> a -> Maybe Doc

prettyStateVecs :: Vector v a => ValPrinter a -> VarOrdering -> Seq (v a) -> Doc
prettyStateVecs f vo = vsep . fmap (parens . prettyStateVec f vo) . toList

prettyStateVec :: Vector v a => ValPrinter a -> VarOrdering -> v a -> Doc
prettyStateVec f (VarOrdering vo) =
    hsep . punctuate comma . mapMaybe (uncurry f) . zip vo . V.toList

prettyValGroup :: ValPrinter IntSet
prettyValGroup (ident, r) vals = case r of
    RangeFeature
      | 0 `member` vals && 1 `member` vals -> Nothing
      | 1 `member` vals                    -> Just ident
      | otherwise                          -> Just $ char '!' <> ident
    RangeBool
      | 0 `member` vals && 1 `member` vals -> Nothing
      | 1 `member` vals                    -> identDef "true"
      | otherwise                          -> identDef "false"
    Range lower upper
      | size vals == 1    -> identDef $ int (findMin vals)
      | isContiguous vals && lower `member` vals && upper `member` vals
                          -> Nothing
      | isContiguous vals -> identDef $
            braces (int (findMin vals) <> ".." <> int (findMax vals))
      | otherwise         -> identDef $
            braces (hsep (punctuate comma (fmap int (Set.toAscList vals))))
    RangeInternal -> Nothing
  where
    identDef doc = Just $ ident <> char '=' <> doc

prettyVal :: ValPrinter Int
prettyVal (ident, r) v = case r of
    RangeFeature
      | v == 0    -> Nothing
      | v == 1    -> Just ident
      | otherwise -> error "Result.prettyVal: illegal value for feature variable"
    RangeBool
      | v == 0    -> identDef "false"
      | v == 1    -> identDef "true"
      | otherwise -> error "Result.prettyVal: illegal value for boolean variable"
    Range _ _     -> identDef (int v)
    RangeInternal -> Nothing
  where
    identDef doc = Just $ ident <> char '=' <> doc

isContiguous :: IntSet -> Bool
isContiguous s
  | Set.null s = True
  | otherwise  = let xs = Set.toAscList s
                 in and (zipWith ((==) . succ) xs (tail xs))

