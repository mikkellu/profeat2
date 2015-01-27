{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module Parser.Results
  ( parseResultCollections
  ) where

import Control.Applicative
import Control.Lens hiding ( noneOf )
import Control.Monad.State

import Data.Array
import Data.Sequence ( Seq, fromList )
import Data.Strict.Tuple
import Data.Text ( Text, pack )
import Data.Traversable

import Text.Parsec hiding ( (<|>), many )
import Text.Parsec.Text ()
import qualified Text.Parsec.Token as P

import Analysis.VarOrdering
import Result

data Log
  = LogStateResults !(Seq (StateVec :!: Result))
  | LogFinalResult  !Result
  | LogTrace        !(Seq StateVec)
  | Log             !Text
  deriving (Show)

type Parser u = Parsec Text u

type Bounds = (Int, Int)

parseResultCollections :: VarOrdering -> Text -> [ResultCollection]
parseResultCollections (VarOrdering vo) output =
    let bs = (0, length vo - 1)
    in case runParser prismOutput bs "output" output of
        Left err  -> error (show err)
        Right lss -> fmap resultCollection lss

resultCollection :: [Log] -> ResultCollection
resultCollection ls = flip execState emptyResultCollection . for ls $ \case
    LogStateResults srs -> rcStateResults .= srs
    LogFinalResult  r   -> rcFinalResult  .= r
    LogTrace        svs -> rcTrace        .= svs
    Log             t   -> rcLog          %= (|> t)

languageDef :: Monad m => P.GenLanguageDef Text u m
languageDef = P.LanguageDef
  { P.commentStart    = ""
  , P.commentEnd      = ""
  , P.commentLine     = ""
  , P.nestedComments  = False
  , P.identStart      = letter <|> char '_'
  , P.identLetter     = alphaNum <|> char '_'
  , P.opStart         = P.opLetter languageDef
  , P.opLetter        = oneOf "="
  , P.reservedNames   = []
  , P.reservedOpNames = []
  , P.caseSensitive   = True
  }

lexer :: Monad m => P.GenTokenParser Text u m
lexer = P.makeTokenParser languageDef

reservedOp = P.reservedOp lexer
integer    = P.integer lexer
float      = P.float lexer
symbol     = P.symbol lexer
lexeme     = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
parens     = P.parens lexer
colon      = P.colon lexer
commaSep   = P.commaSep lexer

int :: Parser u Int
int = fromInteger <$> integer

trySymbol :: String -> Parser u String
trySymbol = try . symbol

line :: Parser u String
line = lexeme ((:) <$> noneOf "-" <*> anyToken `manyTill` newline)

skipLine :: Parser u ()
skipLine = () <$ line

separator :: Parser u ()
separator = () <$ lexeme (try (char '-' *> char '-' `manyTill` newline))

prismOutput :: Parser Bounds [[Log]]
prismOutput = whiteSpace *> (anyToken `manyTill` separator)
                         *> (logs `sepBy` separator) <* eof

logs :: Parser Bounds [Log]
logs = many . choice $
  [ logStateResults
  , logFinalResult
  , logTrace
  , logAny
  ]

logStateResults :: Parser Bounds Log
logStateResults =
    LogStateResults . fromList <$> (start *> skipLine *> many stateResult)
  where
    start       = trySymbol "Satisfying states" <|> trySymbol "Results"
    stateResult = (:!:) <$> (integer *> colon *> stateVec)
                        <*> option (ResultBool True)
                                   (ResultDouble <$> (reservedOp "=" *> float))

logFinalResult :: Parser u Log
logFinalResult = LogFinalResult <$> (start *> result <* skipLine) where
    start = trySymbol "Result:"

logTrace :: Parser Bounds Log
logTrace = LogTrace . fromList <$> (start *> skipLine *> many stateVec) where
    start = trySymbol "Counterexample/witness"

logAny :: Parser u Log
logAny = Log . pack <$> line

result :: Parser u Result
result = choice
  [ ResultBool True  <$  symbol "true"
  , ResultBool False <$  symbol "false"
  , ResultDouble     <$> float
  ]

stateVec :: Parser Bounds StateVec
stateVec = listArray <$> getState <*> parens (commaSep value)

value :: Parser u Int
value = choice
  [ 0 <$ symbol "false"
  , 1 <$ symbol "true"
  , int
  ]

