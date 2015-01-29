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
  , rcFinalResult
  , rcTrace
  , rcLog
  , emptyResultCollection
  , removeNonConfVars

  , prettyStateVec
  , prettyResultCollection
  , prettyResultCollections
  ) where

import Prelude hiding ( all )

import Control.Lens

import Data.Array.IArray
import Data.Array.Unboxed           ( UArray)
import Data.Foldable                ( toList, all )
import Data.Maybe                   ( mapMaybe)
import Data.Sequence                ( Seq, ViewL(..), viewl )
import qualified Data.Sequence as Seq
import Data.Strict.Tuple            ( (:!:), Pair(..) )
import qualified Data.Strict.Tuple as ST

import Data.Text                    ( Text )
import qualified Data.Text.Lazy as L

import Text.PrettyPrint.Leijen.Text

import Analysis.VarOrdering
import Syntax hiding ( Range )

data Result
  = ResultBool !Bool
  | ResultDouble !Double
  deriving (Show)

instance Pretty Result where
    pretty = \case
        ResultBool False -> "false"
        ResultBool True  -> "true"
        ResultDouble d   -> double d

type StateVec = UArray Int Int

data ResultCollection = ResultCollection
  { _rcVarOrdering  :: !VarOrdering
  , _rcStateResults :: !(Seq (StateVec :!: Result))
  , _rcFinalResult  :: !Result
  , _rcTrace        :: !(Seq StateVec)
  , _rcLog          :: !(Seq Text)
  } deriving (Show)

makeLenses ''ResultCollection

emptyResultCollection :: VarOrdering -> ResultCollection
emptyResultCollection vo = ResultCollection
  { _rcVarOrdering  = vo
  , _rcStateResults = Seq.empty
  , _rcFinalResult  = ResultBool False
  , _rcTrace        = Seq.empty
  , _rcLog          = Seq.empty
  }

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
    len'        = length (filter (== VarConf) vrs)
    bs'         = (0, len' - 1)
    confIndices = fmap fst . filter ((VarConf ==) . snd) . zip [0..] $ vrs

    go (sv :!: r) = let confVals = fmap (sv !) confIndices
                    in listArray bs' confVals :!: r

findConfVars :: Seq (StateVec :!: Result) -> [VarRole]
findConfVars (viewl -> (sv :!: _) :< srs) =
    fmap go (uncurry enumFromTo (bounds sv))
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
    p (def, rc) = pretty def <> line <> line <>
                  prettyResultCollection includeLog rc
    separator = line <> line <> text (L.replicate 80 "-") <> line

prettyResultCollection :: Bool -> ResultCollection -> Doc
prettyResultCollection includeLog (ResultCollection vo srs r tr ls) =
    "Final result:" <+> pretty r <$>
    "Results for initial configurations:" <$> prettyStateResults vo srs <$>
    prettyTrace vo tr <$>
    if includeLog then prettyLog ls else empty

prettyLog :: Seq Text -> Doc
prettyLog = vsep . fmap (text . L.fromStrict) . toList

prettyTrace :: VarOrdering -> Seq StateVec -> Doc
prettyTrace vo svs
  | Seq.null svs  = empty
  | otherwise     = "Counterexample/witness:" <$>
                    prettyStateVecs vo svs

prettyStateResults :: VarOrdering -> Seq (StateVec :!: Result) -> Doc
prettyStateResults vo =
    vsep . fmap (ST.uncurry $ prettyStateResult vo) . toList

prettyStateResult :: VarOrdering -> StateVec -> Result -> Doc
prettyStateResult vo sv r =
    parens (prettyStateVec vo sv) <> char '=' <> pretty r

prettyStateVecs :: VarOrdering -> Seq StateVec -> Doc
prettyStateVecs vo = vsep . fmap (parens . prettyStateVec vo) . toList

prettyStateVec :: VarOrdering -> StateVec -> Doc
prettyStateVec (VarOrdering vo) =
    hsep . punctuate comma . mapMaybe (uncurry prettyVal) . zip vo . elems

prettyVal :: (Doc, Range) -> Int -> Maybe Doc
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

