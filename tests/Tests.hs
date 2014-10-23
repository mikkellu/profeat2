-- Tests.hs -- {{
-- vim: set foldmethod=marker foldmarker={{,}} foldtext=foldtext() :
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad

import qualified Data.Text.Lazy.IO as LIO

import System.FilePath
import System.IO

import Test.Tasty
import Test.Tasty.HUnit

import Text.PrettyPrint.Leijen.Text ( pretty )

import Parser.Internal ( Language(..), parseFile' )
import qualified Parser.Internal as Parser
import ProFeat

main = defaultMain tests
-- }}
-- Tests -- {{
tests = testGroup "Tests" [testCases]

-- Test cases -- {{
testCases = testGroup "TestCases" [translatorTestCases]

-- Translator -- {{
translatorTestCases = testGroup "Translator" $ map translatorTestCase
  [ "Constants"
  , "Formulas"
  , "Meta"
  , "Templates"
  ]

-- }}
-- }}
-- }}
-- Configuration -- {{
testCasesDir    = "tests" </> "testcases"
extProFeat      = "profeat"
extPrism        = "prism"
extProFeatProps = "fprops"
extPrismProps   = "props"
-- }}
-- Utils -- {{
translatorTestCase name = testCase name $ do
    let proFeatModelPath = testCasesDir </> name <.> extProFeat
        prismModelPath   = testCasesDir </> name <.> extPrism

    withFile proFeatModelPath ReadMode $ \hProFeatModel ->
        withFile prismModelPath ReadMode $ \hPrismModel -> do
            proFeatModel <- LIO.hGetContents hProFeatModel
            prismModel   <- LIO.hGetContents hPrismModel

            let result = do
                expected    <- parsePrismModel prismModelPath prismModel
                (actual, _) <-
                    parseAndTranslateModel proFeatModelPath proFeatModel

                return (void expected, void actual)

            case result of
                Left err -> assertFailure . show $ pretty err
                Right (expected, actual) -> assertEqual' expected actual
  where
    parsePrismModel = parseFile' PrismLang Parser.model

-- | Like 'assertEqual', but also pretty prints the values.
assertEqual' expected actual = assertBool msg $ expected == actual
  where
    msg = "expected: "  ++ showWithPretty expected ++
          "\nbut got: " ++ showWithPretty actual

-- | Show both the Haskell representation and the 'Doc' representation of
-- @x@.
showWithPretty x = show x ++ "\n\n" ++ show (pretty x)
-- }}

