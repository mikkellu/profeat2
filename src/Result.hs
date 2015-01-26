{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Result
  ( Result(..)
  , StateVec

  , ResultCollection(..)
  , rcStateResults
  , rcFinalResult
  , rcTrace
  , rcLog

  , emptyResultCollection
  ) where

import Control.Lens

import Data.Array
import Data.Sequence ( Seq, empty )
import Data.Strict.Tuple

import Data.Text ( Text )

data Result
  = ResultBool !Bool
  | ResultDouble !Double
  deriving (Show)

type StateVec = Array Int Int

data ResultCollection = ResultCollection
  { _rcStateResults :: !(Seq (StateVec :!: Result))
  , _rcFinalResult  :: !Result
  , _rcTrace        :: !(Seq StateVec)
  , _rcLog          :: !(Seq Text)
  } deriving (Show)

emptyResultCollection :: ResultCollection
emptyResultCollection = ResultCollection
  { _rcStateResults = empty
  , _rcFinalResult  = ResultBool False
  , _rcTrace        = empty
  , _rcLog          = empty
  }

makeLenses ''ResultCollection

