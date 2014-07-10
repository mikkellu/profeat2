module Main where

import qualified Data.Text.Lazy.IO as LIO

import Text.PrettyPrint.Leijen.Text

import System.Environment

import Parser
import SymbolTable
import Syntax
import Translator

main :: IO ()
main = do
    [srcName] <- getArgs
    src <- LIO.readFile srcName

    let result = do
        Model defs <- parseModel srcName src
        symTbl <- extendSymbolTable emptySymbolTable defs
        translateModel symTbl

    case result of
        Right m -> print $ pretty m
        Left err -> print $ pretty err

