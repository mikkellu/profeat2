-- Tests.hs -- {{
-- vim: set foldmethod=marker foldmarker={{,}} foldtext=foldtext() :
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Text.Lazy.IO as LIO

import System.Directory
import System.FilePath
import System.IO ( IOMode(..) )

import Test.Tasty
import Test.Tasty.HUnit

import Text.PrettyPrint.Leijen.Text ( pretty )

import Parser.Internal ( Language(..), parseFile' )
import qualified Parser.Internal as Parser

import ProFeat
import Symbols
import Types

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
  , "Controller"
  , "InitConfNone"
  , "InitConfAuto"
  , "InitConfUserDef"
  , "PropertyFilter"
  , "PropertyRewards"
  , "PropertyConditional"
  , "PropertyQuantile"
  , "ConstArrays"
  , "DistributionBinom"
  , "Init"
  , "Invariant"
  , "Blocking"
  , "Attributes"
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
    let expectedModelPath = testCasesDir </> name <.> extPrism
        actualModelPath   = testCasesDir </> name <.> extProFeat
        expectedPropsPath = testCasesDir </> name <.> extPrismProps
        actualPropsPath   = testCasesDir </> name <.> extProFeatProps

    propsExist <- liftA2 (&&) (doesFileExist expectedPropsPath)
                              (doesFileExist actualPropsPath)

    let opts = defaultOptions
                   { proFeatModelPath = actualModelPath
                   , proFeatPropsPath = if propsExist
                                            then Just actualPropsPath
                                            else Nothing
                   }

    flip runTest opts . withTranslatedModel $ \actualModel -> do
        withFile expectedModelPath ReadMode $ \hModel -> do
            modelContents <- liftIO $ LIO.hGetContents hModel
            expected      <- liftEither' $
                parsePrismModel expectedModelPath modelContents

            liftIO $ assertEqual' (void expected) (void actualModel)

        when propsExist . withFile expectedPropsPath ReadMode $ \hProps ->
            withProFeatProps $ \(Just props) -> do
                actualProps <- translateProps props

                propsContents <- liftIO $ LIO.hGetContents hProps
                expected      <- liftEither' $
                    parsePrismProps expectedPropsPath propsContents

                liftIO $ assertEqual' (void expected) (void actualProps)
  where
    parsePrismModel = parseFile' PrismLang Parser.model
    parsePrismProps = parseFile' PrismLang Parser.specification

runTest m opts = do
    result <- run m (emptySymbolTable defaultModelType) opts
    case result of
        Right _  -> return ()
        Left err -> assertFailure . show $ pretty err

-- | Like 'assertEqual', but also pretty prints the values.
assertEqual' expected actual = assertBool msg $ expected == actual
  where
    msg = "expected: "  ++ showWithPretty expected ++
          "\nbut got: " ++ showWithPretty actual

-- | Show both the Haskell representation and the 'Doc' representation of
-- @x@.
showWithPretty x = show x ++ "\n\n" ++ show (pretty x)
-- }}

