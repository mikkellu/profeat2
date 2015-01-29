{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Result
  ( Result(..)

  , StateVec
  , prettyStateVec

  , ResultCollection(..)
  , rcStateResults
  , rcFinalResult
  , rcTrace
  , rcLog
  , prettyResultCollection
  , prettyResultCollections

  , emptyResultCollection
  ) where

import Control.Lens

import Data.Array
import Data.Foldable                ( toList )
import Data.Maybe                   ( mapMaybe)
import Data.Sequence                ( Seq )
import qualified Data.Sequence as Seq
import Data.Strict.Tuple            ( (:!:) )
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

type StateVec = Array Int Int

data ResultCollection = ResultCollection
  { _rcStateResults :: !(Seq (StateVec :!: Result))
  , _rcFinalResult  :: !Result
  , _rcTrace        :: !(Seq StateVec)
  , _rcLog          :: !(Seq Text)
  } deriving (Show)

emptyResultCollection :: ResultCollection
emptyResultCollection = ResultCollection
  { _rcStateResults = Seq.empty
  , _rcFinalResult  = ResultBool False
  , _rcTrace        = Seq.empty
  , _rcLog          = Seq.empty
  }

prettyResultCollections :: Bool
                        -> VarOrdering
                        -> Specification a
                        -> [ResultCollection]
                        -> Doc
prettyResultCollections includeLog vo (Specification defs) rcs =
    let props = defs^..traverse._PropertyDef
    in vsep . punctuate separator . fmap p $ zip props rcs
  where
    p (def, rc) = pretty def <> line <> line <>
                  prettyResultCollection includeLog vo rc
    separator = line <> line <> text (L.replicate 80 "-") <> line

prettyResultCollection :: Bool -> VarOrdering -> ResultCollection -> Doc
prettyResultCollection includeLog vo (ResultCollection srs r tr ls) =
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

makeLenses ''ResultCollection

