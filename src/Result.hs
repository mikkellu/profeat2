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
  , emptyResultCollection
  , removeNonConfVars
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
import Data.Sequence                ( Seq, ViewL(..), viewl )
import qualified Data.Sequence as Seq
import Data.IntSet                  ( IntSet, member, findMin, findMax, singleton, size )
import qualified Data.IntSet as Set
import Data.Strict.Tuple            ( (:!:), Pair(..) )
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
  deriving (Eq, Ord, Show)

instance Pretty Result where
    pretty = \case
        ResultBool False -> "false"
        ResultBool True  -> "true"
        ResultDouble d   -> double d

type StateVec = UV.Vector Int

type GroupedStateVec = BV.Vector IntSet

data ResultCollection = ResultCollection
  { _rcVarOrdering         :: !VarOrdering
  , _rcStateResults        :: !(Seq (StateVec :!: Result))
  , _rcGroupedStateResults :: !(Seq (GroupedStateVec :!: Result))
  , _rcFinalResult         :: !Result
  , _rcTrace               :: !(Seq StateVec)
  , _rcLog                 :: !(Seq Text)
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
  }

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
toGroupedStateResult (sv :!: r) = V.map singleton (V.convert sv) :!: r

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
    confIndices = fmap fst . filter ((VarConf ==) . snd) . zip [0..] $ vrs

    go (sv :!: r) = let confVals = fmap (sv !) confIndices
                    in V.fromList confVals :!: r

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
prettyResultCollection includeLog (ResultCollection vo srs gsrs r tr ls) =
    "Final result:" <+> pretty r <$>
    stateResults <$>
    groupedStateResults <$>
    prettyTrace vo tr <$>
    if includeLog then prettyLog ls else empty
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

