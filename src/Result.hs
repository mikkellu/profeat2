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

  , ResultCollection(..)
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

  , prettyResultCollections
  ) where

import Prelude hiding ( (<$>) )
import Control.Lens hiding ( (:<) )
import Control.Applicative          ( (<|>), liftA2 )

import Data.Foldable                ( toList )
import Data.Maybe                   ( mapMaybe)
import Data.Ord                     ( comparing )
import Data.Sequence                ( Seq, ViewL(..), viewl )
import qualified Data.Sequence as Seq
import Data.Semigroup
import Data.Strict.Tuple            ( (:!:), Pair(..) )
import Data.Strict.Tuple.Lens       ()
import qualified Data.Strict.Tuple as ST

import Data.Text                    ( Text )
import qualified Data.Text.Lazy as L
import Data.Vector.Generic          ( Vector, (!) )
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV

import Text.PrettyPrint.Leijen.Text hiding ((<>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Analysis.VarOrder
import Syntax hiding ( Range )

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
            brackets (double lower PP.<> comma PP.<> double upper)

type StateVec = UV.Vector Int

data ResultCollection = ResultCollection
  { _rcVarOrder            :: !VarOrder
  , _rcStateResults        :: !(Seq (StateVec :!: Result))
  , _rcFinalResult         :: Maybe Result
  , _rcTrace               :: !(Seq StateVec)
  , _rcLog                 :: !(Seq Text)
  , _rcDdNodes             :: !(Seq (Int :!: Int))
  , _rcBuildingTime        :: !Double
  , _rcCheckingTime        :: !Double
  } deriving (Show)

makeLenses ''ResultCollection

emptyResultCollection :: VarOrder -> ResultCollection
emptyResultCollection vo = ResultCollection
  { _rcVarOrder            = vo
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
appendResultCollection (ResultCollection xVo xSrs xR xTr xL xNs xBt xCt)
                       (ResultCollection _   ySrs yR _   yL yNs yBt yCt) =
    let r = liftA2 (<>) xR yR <|> xR <|> yR
    in ResultCollection xVo (mappend xSrs ySrs) r xTr (mappend xL yL)
          (mappend xNs yNs) (xBt + yBt) (xCt + yCt)

sortStateResults :: ResultCollection -> ResultCollection
sortStateResults = rcStateResults %~ Seq.sortBy (comparing (view _2'))

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
    let srs         = rc^.rcStateResults
        vrs         = findConfVars srs
        VarOrder vs = rc^.rcVarOrder
        vs'         = fmap fst . filter ((VarConf ==) . snd) . zip vs $ vrs
        srs'        = filterConfVars vrs srs
    in rc & rcVarOrder  .~ VarOrder vs'
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
           in if all ((v ==) . (! i) . ST.fst) srs
                  then VarNonConf
                  else VarConf
findConfVars _ = []

-- Pretty Printing

prettyResultCollections :: Bool -> Specification a -> [ResultCollection] -> Doc
prettyResultCollections includeLog (Specification defs) rcs =
    let props = defs^..traverse._PropertyDef
    in vsep . punctuate separator . fmap p $ zip props rcs
  where
    p (def, rc) = pretty def PP.<> line PP.<> line PP.<>
                  prettyResultCollection includeLog rc
    separator = line PP.<> line PP.<> text (L.replicate 80 "-") PP.<> line

prettyResultCollection :: Bool -> ResultCollection -> Doc
prettyResultCollection includeLog ResultCollection{..} =
    (if includeLog then prettyLog _rcLog PP.<> line PP.<> line else empty) PP.<>
    maybe empty (("Final result:" <+>) . pretty) _rcFinalResult <$>
    stateResults <$>
    prettyTrace <$>
    line PP.<>
    prettyTrnsNodes <$>
    "Time for model construction:" <+> pretty _rcBuildingTime <$>
    "Time for model checking:" <+> pretty _rcCheckingTime
  where
    stateResults
      | Seq.null _rcStateResults = empty
      | otherwise =
        "Results for initial configurations:" <$>
        indent 4 (prettyStateResults _rcVarOrder _rcStateResults)

    prettyTrace
      | Seq.null _rcTrace = empty
      | otherwise =
        "Counterexample/witness:" <$>
        indent 4 (prettyStateVecs _rcVarOrder _rcTrace)

    prettyTrnsNodes = case viewl _rcDdNodes of
        EmptyL -> empty
        n :< (viewl -> EmptyL) -> "Transition matrix:" <+> prettyNumDdNodes n
        ns -> "Transition matrices:" <+>
              int (sum (fmap ST.fst ns)) <+> "(sum)" <$>
              (indent 4 . hsep . punctuate comma . fmap (int . ST.fst) $ toList ns)
    prettyNumDdNodes (n :!: nt) =
        int n <+> "nodes" <+> parens (int nt <+> "terminal")

prettyLog :: Seq Text -> Doc
prettyLog = vsep . fmap (text . L.fromStrict) . toList

prettyStateResults :: Vector v Int => VarOrder -> Seq (v Int :!: Result) -> Doc
prettyStateResults vo = vsep . fmap (ST.uncurry $ prettyStateResult vo) . toList

prettyStateResult :: Vector v Int => VarOrder -> v Int -> Result -> Doc
prettyStateResult vo sv r =
    parens (prettyStateVec vo sv) PP.<> char '=' PP.<> pretty r

prettyStateVecs :: Vector v Int => VarOrder -> Seq (v Int) -> Doc
prettyStateVecs vo = vsep . fmap (parens . prettyStateVec vo) . toList

prettyStateVec :: Vector v Int => VarOrder -> v Int -> Doc
prettyStateVec (VarOrder vo) =
    hsep . punctuate comma . mapMaybe (uncurry prettyVal) . zip vo . V.toList
  where
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
        identDef doc = Just $ ident PP.<> char '=' PP.<> doc
