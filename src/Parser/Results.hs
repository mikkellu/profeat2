{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeOperators             #-}

module Parser.Results
  ( parseResultCollections
  ) where

import Control.Applicative
import Control.Lens hiding ( noneOf )
import Control.Monad.State

import Data.Maybe
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence ( Seq, fromList, singleton )
import Data.Strict.Tuple
import Data.Text ( Text, pack )
import Data.Traversable
import qualified Data.Vector.Generic as V

import Text.Parsec hiding ( (<|>), many )
import Text.Parsec.Text
import qualified Text.Parsec.Token as P

import Result

data Log
  = LogVariables    VariablesRaw
  | LogStateResults !(Seq StateResult)
  | LogFinalResult  !Result
  | LogTrace        !(Seq StateVec)
  | LogDdNodes      !(Int :!: Int)
  | LogTime         !Text !Double
  | Log             !Text
  deriving (Show)

makePrisms ''Log

parseResultCollections :: Text -> [ResultCollection]
parseResultCollections output =
    case parse prismOutput "output" output of
        Left err ->
            error $ "internal error while parsing PRISM output\n:" ++ show err
        Right lss ->
            let varss = fmap extractVariables lss
                vars =
                    fromMaybe
                        (error
                             "could not parse variable order from PRISM output")
                        (getLast (mconcat varss))
            in fmap (resultCollection vars) lss

extractVariables :: [Log] -> Last VariablesRaw
extractVariables =
    mconcat . fmap (Last . Just) . toListOf (traverse . _LogVariables)

resultCollection :: VariablesRaw -> [Log] -> ResultCollection
resultCollection vars ls =
    flip execState (emptyResultCollection vars) . for ls $ \case
        LogVariables    _   -> return () -- ignore, handled in parseResultCollections
        LogStateResults srs -> rcStateResults .= srs
        LogFinalResult  r   -> rcFinalResult  .= Just r
        LogTrace        svs -> rcTrace        .= svs
        LogDdNodes      n   -> rcDdNodes      .= singleton n
        LogTime         n t -> rcTime         %= Map.insertWith (zipWith (+)) n [t]
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

P.TokenParser{..} = lexer

int :: Parser Int
int = fromInteger <$> integer

trySymbol :: String -> Parser String
trySymbol = try . symbol

line :: Parser String
line = lexeme ((:) <$> noneOf "-" <*> anyToken `manyTill` newline)

skipLine :: Parser ()
skipLine = () <$ line

separator :: Parser ()
separator = () <$ lexeme (try (char '-' *> char '-' `manyTill` newline))

prismOutput :: Parser [[Log]]
prismOutput = whiteSpace *> (logs `sepBy` separator) <* eof

logs :: Parser [Log]
logs = many . choice $
  [ logVariables
  , logStateResults
  , logFinalResult
  , logTrace
  , logDdNodes
  , logTime
  , logAny
  ]

logVariables :: Parser Log
logVariables = LogVariables <$>
    (start *> ((pack <$> ident) `endBy` char ' ') <* whiteSpace)
  where
    start = trySymbol "Variables:" <|> trySymbol "Variables (after reordering):"
    ident = many (alphaNum <|> char '_')

logStateResults :: Parser Log
logStateResults =
    LogStateResults . fromList <$> (start *> skipLine *> many stateResult)
  where
    start       = trySymbol "Satisfying states" <|> trySymbol "Results"
    stateResult = StateResult
        <$> (int <* colon)
        <*> stateVec
        <*> option (ResultBool True)
                   (ResultDouble <$> (reservedOp "=" *> float'))

logFinalResult :: Parser Log
logFinalResult = LogFinalResult <$> (start *> result) where
    start = trySymbol "Result:"

logTrace :: Parser Log
logTrace = LogTrace . fromList <$> (start *> skipLine *> many stateVec) where
    start = trySymbol "Counterexample/witness"

logDdNodes :: Parser Log
logDdNodes = LogDdNodes <$>
    ((:!:) <$> (start *> int <* symbol "nodes")
           <*> (parens (int <* symbol "terminal") <* skipLine))
  where
    start = trySymbol "Transition matrix:"

logTime :: Parser Log
logTime = LogTime <$> (start *> name) <*> (whiteSpace *> float <* skipLine)
  where
    start = trySymbol "Time for "
    name  = pack <$> (anyChar `manyTill` colon)

logAny :: Parser Log
logAny = Log . pack <$> line

result :: Parser Result
result = choice
  [ ResultBool True  <$ symbol "true"
  , ResultBool False <$ symbol "false"
  , brackets (ResultRange <$> (float' <* comma) <*> float')
  , ResultDouble <$> float'
  ]

stateVec :: Parser StateVec
stateVec = V.fromList <$> parens (commaSep value)

value :: Parser Int
value = choice
  [ 0 <$ symbol "false"
  , 1 <$ symbol "true"
  , int
  ]

float' :: Parser Double
float' = choice
  [ ( 1.0/0) <$ symbol "Infinity"
  , (-1.0/0) <$ try (symbol "-Infinity")
  , signedFloat
  ]

signedFloat :: Parser Double
signedFloat = do
    f <- lexeme sign
    f <$> float

sign :: Num a => Parser (a -> a)
sign =
    (negate <$ char '-') <|>
    (id     <$ char '+') <|>
    return id
