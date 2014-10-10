{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | The main module of ProFeat.

module ProFeat
  ( proFeatMain
  , translate
  ) where

import Control.Exception.Lens
import Control.Lens hiding ( argument )

import Data.Maybe
import Data.Monoid
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy.IO as L

import Options.Applicative

import System.Exit
import System.IO
import System.IO.Error.Lens

import Text.PrettyPrint.Leijen.Text ( Pretty, pretty, renderPretty, displayT )

import Error
import Parser
import SymbolTable
import Syntax
import Translator

translate :: String -> Text -> Either Error Text
translate proFeatModelName proFeatModel = do
    Model defs <- parseModel proFeatModelName proFeatModel
    symTbl     <- extendSymbolTable emptySymbolTable defs
    render <$> translateModel symTbl
-- ProFeat CLI
--
-- profeat [options] (<model-file> | -)
--
-- <model-file>
helpModelFile = "The ProFeat model file (if '-' is given, the model is read from stdin)"
--
-- Options:
--  -o <outfile>
helpModelOutput = "Write the generated PRISM model to <outfile>"

-- | Stores the command line arguments.
data ProFeatOptions = ProFeatOptions
  { inModelPath  :: FilePath
  , outModelPath :: Maybe FilePath
  }

-- | Stores the handles to read from and to write to.
data FileHandles = FileHandles
  { hInModel  :: Handle
  , hOutModel :: Handle
  }

proFeatOptions :: Parser ProFeatOptions
proFeatOptions = ProFeatOptions
  <$>           argument str ( metavar "<model-file>"
                            <> help helpModelFile )
  <*> optional (strOption    ( short 'o'
                            <> metavar "<outfile>"
                            <> help helpModelOutput ))

proFeatMain :: IO ()
proFeatMain = handling _IOException ioeHandler $
    execParser options >>= translateWithOptions
  where
    options      = info (helper <*> proFeatOptions) mempty
    ioeHandler e = do
        let file = fromMaybe "<unknown source>" $ e^.fileName
        putStrLn $ e^.description ++ ": " ++ file
        exitWith $ ExitFailure 2

translateWithOptions :: ProFeatOptions -> IO ()
translateWithOptions opts = withHandles opts $
    translateWithHandles (inModelPath opts)

translateWithHandles :: FilePath -> FileHandles -> IO ()
translateWithHandles modelName hs = do
    proFeatModel <- L.hGetContents $ hInModel hs
    case translate modelName proFeatModel of
        Left err -> do
            print $ pretty err
            exitWith $ ExitFailure 1
        Right prismModel ->
            L.hPutStrLn (hOutModel hs) prismModel

withHandles :: ProFeatOptions -> (FileHandles -> IO ()) -> IO ()
withHandles opts m =
    withModel (inModelPath opts)                $ \im ->
    maybeWithFile WriteMode (outModelPath opts) $ \om ->
    m (FileHandles im om)

withModel :: String -> (Handle -> IO ()) -> IO ()
withModel arg = maybeWithFile ReadMode $ case arg of
    "-"  -> Nothing
    name -> Just name

maybeWithFile :: IOMode -> Maybe FilePath -> (Handle -> IO ()) -> IO ()
maybeWithFile mode (Just path) = withFile path mode
maybeWithFile mode Nothing     = case mode of
    ReadMode  -> \m -> m stdin
    WriteMode -> \m -> m stdout
    _         -> error "Spslc.maybeWithFile: illegal IOMode"

render :: (Pretty a) => a -> Text
render = displayT . renderPretty 0.4 80 . pretty

