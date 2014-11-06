{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

-- | The main module of ProFeat.

module ProFeat
  ( ProFeatOptions(..)
  , defaultOptions

  , proFeatMain

  , proFeat
  , withTranslatedModel
  , translateProps

  , ProFeat
  , run
  , withFile
  , liftEither'
  ) where

import Control.Exception.Lens
import Control.Lens hiding ( argument )
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy
import qualified Data.Text.Lazy.IO as L

import Options.Applicative

import System.Exit
import System.IO hiding ( withFile )
import qualified System.IO as S
import System.IO.Error.Lens

import Text.PrettyPrint.Leijen.Text ( Pretty, displayT, pretty, renderPretty )

import Error
import Parser
import SymbolTable
import Syntax
import Translator

-- ProFeat CLI
--
-- profeat [options] (<model-file> | -) <properties-file>
--
-- <model-file>
helpModelFile = "The ProFeat model file (if '-' is given, the model is read from stdin)"
-- <properties-file>
helpPropsFile = "The properties file"
--
-- Options:
--  -o <file>
helpModelOutput = "Write the generated PRISM model to <file>"
--  -p <file>
helpPropsOutput = "Write the translated properties to <file>"

-- | The proFeatMain function is the application's main entry point.
proFeatMain :: IO ()
proFeatMain = handling _IOException ioeHandler $
    execParser options >>= runApp proFeat
  where
    options      = info (helper <*> proFeatOptions) mempty
    ioeHandler e = do
        let file = fromMaybe "<unknown source>" $ e^.fileName
        hPutStrLn stderr $ e^.description ++ ": " ++ file
        exitWith $ ExitFailure 2

-- | Stores the command line arguments.
data ProFeatOptions = ProFeatOptions
  { proFeatModelPath :: FilePath
  , proFeatPropsPath :: Maybe FilePath
  , prismModelPath   :: Maybe FilePath
  , prismPropsPath   :: Maybe FilePath
  } deriving (Show)

defaultOptions :: ProFeatOptions
defaultOptions = ProFeatOptions
  { proFeatModelPath = "-"
  , proFeatPropsPath = Nothing
  , prismModelPath   = Nothing
  , prismPropsPath   = Nothing
  }

proFeatOptions :: Parser ProFeatOptions
proFeatOptions = ProFeatOptions
  <$>           argument str ( metavar "<model-file>"
                            <> help helpModelFile )
  <*> optional (argument str ( metavar "<properties-file>"
                            <> help helpPropsFile ))
  <*> optional (strOption    ( short 'o'
                            <> metavar "<file>"
                            <> help helpModelOutput ))
  <*> optional (strOption    ( short 'p'
                            <> metavar "<file>"
                            <> help helpPropsOutput ))

type ProFeat = StateT SymbolTable (ExceptT Error (ReaderT ProFeatOptions IO))

proFeat :: ProFeat ()
proFeat = withTranslatedModel $ \prismModel -> do
    prismProps <- translateProps

    maybeWriteFile (render prismModel) =<< asks prismModelPath
    for_ prismProps . flip (maybeWriteFile . render) =<< asks prismPropsPath

withTranslatedModel :: (LModel -> ProFeat a) -> ProFeat a
withTranslatedModel m = do
    path <- asks proFeatModelPath
    maybeWithFile ReadMode (pathToMaybe path) $ \hIn -> do
        modelContents <- liftIO $ L.hGetContents hIn

        (prismModel, symTbl) <- liftEither' $ do
            Model defs <- parseModel path modelContents
            symTbl     <- extendSymbolTable emptySymbolTable defs
            prismModel <- translateModel symTbl
            return (prismModel, symTbl)

        put symTbl
        m prismModel

translateProps :: ProFeat (Maybe LSpecification)
translateProps = asks proFeatPropsPath >>= \case
    Nothing   -> return Nothing
    Just path -> withFile path ReadMode $ \hIn -> do
        propsContents <- liftIO $ L.hGetContents hIn

        symTbl <- get
        (prismProps, symTbl') <- liftEither' $ do
            spec@(Specification defs) <- parseSpecification path propsContents
            symTbl'                   <- extendSymbolTable symTbl defs
            spec'                     <- translateSpec symTbl' spec
            return (spec', symTbl')

        put symTbl'
        return $ Just prismProps

runApp :: ProFeat () -> ProFeatOptions -> IO ()
runApp m opts = do
    result <- run m emptySymbolTable opts
    case result of
        Right _  -> return ()
        Left err -> do
            hPrint stderr $ pretty err
            exitWith $ ExitFailure 2

run :: ProFeat a
    -> SymbolTable
    -> ProFeatOptions
    -> IO (Either Error (a, SymbolTable))
run m = runReaderT . runExceptT . runStateT m

-- | Write the given 'Text' to a file if 'Just' a path is given, else write
-- to 'stdout'.
maybeWriteFile :: Text -> Maybe FilePath -> ProFeat ()
maybeWriteFile content path = maybeWithFile WriteMode path $
    liftIO . flip L.hPutStrLn content

-- | @maybeWithFile mode path act@ opens a file if 'Just' given a path and
-- passes the resulting handle to @act@. If @path@ is 'Nothing', the
-- resulting handle is either 'stdin' or 'stdout', depending on the
-- requested 'IOMode'.
maybeWithFile :: IOMode -> Maybe FilePath -> (Handle -> ProFeat a) -> ProFeat a
maybeWithFile mode (Just path) = withFile path mode
maybeWithFile mode Nothing     = case mode of
    ReadMode  -> ($ stdin)
    WriteMode -> ($ stdout)
    _         -> error "ProFeat.maybeWithFile: illegal IOMode"

-- | A version of withFile that works in the ProFeat monad.
withFile :: FilePath -> IOMode -> (Handle -> ProFeat a) -> ProFeat a
withFile file mode m = do
    opts   <- ask
    symTbl <- get

    liftIO (S.withFile file mode (\h -> run (m h) symTbl opts)) >>= \case
        Left err                 -> throwError err
        Right (result, symTbl') -> do
            put symTbl'
            return result

-- | @pathToMaybe path@ returns 'Nothing', if the @path@ is "-", otherwise
-- 'Just' @path@ is returned.
pathToMaybe :: FilePath -> Maybe FilePath
pathToMaybe path = case path of
    "-" -> Nothing
    _   -> Just path

liftEither' :: Either Error a -> ProFeat a
liftEither' = lift . liftEither

liftEither :: (Monad m) => Either e a -> ExceptT e m a
liftEither = ExceptT . return

render :: (Pretty p) => p -> Text
render = displayT . renderPretty 0.4 80 . pretty

