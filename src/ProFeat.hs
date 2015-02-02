{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

-- | The main module of ProFeat.

module ProFeat
  ( ProFeatOptions(..)
  , Verbosity(..)
  , defaultOptions

  , proFeatMain

  , proFeat
  , withProFeatModel
  , withProFeatProps
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

import Data.Maybe
import Data.Monoid
import qualified Data.Text as S
import qualified Data.Text.IO as SIO
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import Data.Traversable

import Options.Applicative

import System.Exit
import System.IO hiding ( withFile )
import qualified System.IO as IO
import System.IO.Error.Lens
import System.Process.Text          ( readProcessWithExitCode )

import Text.PrettyPrint.Leijen.Text ( Pretty, displayT, pretty, renderPretty )

import Analysis.VarOrdering
import Error
import Parser
import Parser.Results
import Result
import SymbolTable
import Syntax
import Translator

-- ProFeat CLI
--
-- profeat [option] (<model-file> | -) <properties-file>
--
-- <model-file>
helpModelFile = "The ProFeat model file (if '-' is given, the model is read from stdin)"
-- <properties-file>
helpPropsFile = "The ProFeat property list"
--
-- Options
-- -o --export-model
helpExportModel = "Export the translated model to <file>"
-- -p --export-properties
helpExportProperties = "Export the translated properties to <file>"
-- -r --export-results
helpExportResults = "Export the results of model checking to <file>"
--    --import-results
helpImportResults = "Import the PRISM results from <file> for postprocessing"
--    --group-results
helpGroupResults = "Group initial configurations by their result"
--    --round-results
helpRoundResults = "Round results to <precision> digits before grouping"
-- -t --translate
helpTranslate = "Translate only, do not model check"
-- -m --model-checking
helpModelChecking = "Translate and model check only, do not postprocess results"
--    --prism-path
helpPrismPath    = "Set the path of the PRISM executable"
defaultPrismPath = "prism"
-- -v --verbose
helpVerbose = "Enable verbose output"


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
  { proFeatModelPath   :: FilePath
  , proFeatPropsPath   :: Maybe FilePath
  , prismModelPath     :: Maybe FilePath
  , prismPropsPath     :: Maybe FilePath
  , proFeatResultsPath :: Maybe FilePath
  , prismResultsPath   :: Maybe FilePath
  , groupResults       :: !Bool
  , roundResults       :: Maybe Int
  , translateOnly      :: !Bool
  , modelCheckOnly     :: !Bool
  , prismExecPath      :: FilePath
  , verbosity          :: !Verbosity
  }

data Verbosity = Normal | Verbose deriving (Eq)

defaultOptions :: ProFeatOptions
defaultOptions = ProFeatOptions
  { proFeatModelPath = "-"
  , proFeatPropsPath   = Nothing
  , prismModelPath     = Nothing
  , prismPropsPath     = Nothing
  , proFeatResultsPath = Nothing
  , prismResultsPath   = Nothing
  , groupResults       = False
  , roundResults       = Nothing
  , translateOnly      = False
  , modelCheckOnly     = False
  , prismExecPath      = defaultPrismPath
  , verbosity          = Normal
  }

proFeatOptions :: Parser ProFeatOptions
proFeatOptions = ProFeatOptions
  <$> strArgument           ( metavar "<model-file>"
                           <> help helpModelFile )
  <*> optional (strArgument ( metavar "<properties-file>"
                           <> help helpPropsFile ))
  <*> optional (strOption   ( long "export-model" <> short 'o'
                           <> metavar "<file>"
                           <> help helpExportModel ))
  <*> optional (strOption   ( long "export-properties" <> short 'p'
                           <> metavar "<file>"
                           <> help helpExportProperties ))
  <*> optional (strOption   ( long "export-results" <> short 'r'
                           <> metavar "<file>"
                           <> hidden
                           <> help helpExportResults ))
  <*> optional (strOption   ( long "import-results"
                           <> metavar "<file>"
                           <> hidden
                           <> help helpImportResults ))
  <*> switch                ( long "group-results"
                           <> hidden
                           <> help helpGroupResults )
  <*> optional (option auto  ( long "round-results"
                           <> metavar "<precision>"
                           <> hidden
                           <> help helpRoundResults ))
  <*> switch                ( long "translate" <> short 't'
                           <> hidden
                           <> help helpTranslate )
  <*> switch                ( long "model-checking" <> short 'm'
                           <> hidden
                           <> help helpModelChecking )
  <*> strOption             ( long "prism-path"
                           <> metavar "<path>"
                           <> value defaultPrismPath
                           <> showDefault
                           <> hidden
                           <> help helpPrismPath )
  <*> flag Normal Verbose   ( long "verbose" <> short 'v'
                           <> hidden
                           <> help helpVerbose )

type ProFeat = StateT SymbolTable (ExceptT Error (ReaderT ProFeatOptions IO))

proFeat :: ProFeat ()
proFeat = withProFeatModel . withProFeatProps $ \proFeatProps ->
    asks prismResultsPath >>= \case
        Just resultsPath -> case proFeatProps of -- only do postprocessing
            Nothing -> liftIO $ do
                hPutStrLn stderr "Could not postprocess PRISM results, no ProFeat properties list given"
                exitWith $ ExitFailure 4
            Just props -> withFile resultsPath ReadMode $ \hIn -> do
                prismOutput <- liftIO $ SIO.hGetContents hIn
                writeProFeatOutput props prismOutput
        Nothing -> do
            vPutStr "Translating..."
            prismModel <- translate
            prismProps <- _Just translateProps proFeatProps
            vPutStrLn "done"

            with prismModelPath $ \p -> do
                vPutStrLn $ "Writing PRISM model to " ++ p
                renderToFile p prismModel
            with prismPropsPath $ \p -> do
                vPutStrLn $ "Writing PRISM properties list to " ++ p
                void . for prismProps $ renderToFile p

            when' (asks translateOnly) $ liftIO exitSuccess

            prismOutput <- callPrism prismModel prismProps

            when' (asks modelCheckOnly) $ liftIO exitSuccess

            case proFeatProps of
                Just props -> writeProFeatOutput props prismOutput
                Nothing    -> return ()

withProFeatModel :: ProFeat a -> ProFeat a
withProFeatModel m = do
    path <- asks proFeatModelPath
    maybeWithFile ReadMode (pathToMaybe path) $ \hIn -> do
        modelContents <- liftIO $ LIO.hGetContents hIn

        symTbl <- liftEither' $ do
            Model defs <- parseModel path modelContents
            extendSymbolTable emptySymbolTable defs

        put symTbl >> m

translate :: ProFeat LModel
translate = liftEither' . translateModel =<< get

withProFeatProps :: (Maybe LSpecification -> ProFeat a) -> ProFeat a
withProFeatProps m = asks proFeatPropsPath >>= \case
    Nothing   -> m Nothing
    Just path -> withFile path ReadMode $ \hIn -> do
        propsContents <- liftIO $ LIO.hGetContents hIn
        symTbl        <- get

        (spec, symTbl') <- liftEither' $ do
            spec@(Specification defs) <- parseSpecification path propsContents
            symTbl'                   <- extendSymbolTable symTbl defs
            return (spec, symTbl')

        put symTbl'
        m (Just spec)

withTranslatedModel :: (LModel -> ProFeat a) -> ProFeat a
withTranslatedModel = withProFeatModel . (translate >>=)

translateProps :: LSpecification -> ProFeat LSpecification
translateProps spec = do
    symTbl <- get
    liftEither' $ translateSpec symTbl spec

callPrism :: LModel -> Maybe LSpecification -> ProFeat S.Text
callPrism prismModel prismProps = do
    vPutStr "Model Checking..."

    modelPath <- getPath prismModelPath "prism" prismModel

    args <- case prismProps of
        Nothing    -> return [modelPath]
        Just props -> do
            propsPath <- getPath prismPropsPath "props" props
            return [modelPath, propsPath]

    prismPath <- asks prismExecPath

    (exitCode, std, err) <- liftIO $
        readProcessWithExitCode prismPath args S.empty

    vPutStrLn "done"

    unless (exitCode == ExitSuccess) . liftIO $ do
        SIO.hPutStrLn stdout std
        SIO.hPutStrLn stderr err
        exitWith exitCode

    return std
  where
    getPath exportOpt ext m = asks exportOpt >>= \case
        Just path -> return path
        Nothing   -> do
            let path = "out." ++ ext
            renderToFile path m
            return path

writeProFeatOutput :: LSpecification -> S.Text -> ProFeat ()
writeProFeatOutput props prismOutput = do
    vPutStr "Processing results..."
    proFeatOutput <- postprocessPrismOutput props prismOutput
    vPutStrLn "done"

    maybeWriteFile proFeatOutput =<< asks proFeatResultsPath

postprocessPrismOutput :: LSpecification -> S.Text -> ProFeat L.Text
postprocessPrismOutput spec = parse
    >=> (return . fmap removeNonConfVars)
    >=> applyRounding
    >=> applyGrouping
    >=> (return . renderResultCollections . prettyResultCollections False spec)
  where
    parse out = do
        symTbl <- get
        return $ parseResultCollections (varOrdering symTbl) out

    applyRounding rcs = asks roundResults <&> \case
        Just precision -> fmap (roundStateResults precision) rcs
        Nothing        -> rcs

    applyGrouping rcs = do
        gr <- asks groupResults
        return $ if gr
            then fmap groupStateVecs rcs
            else rcs

    renderResultCollections = displayT . renderPretty 1.0 300

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
maybeWriteFile :: L.Text -> Maybe FilePath -> ProFeat ()
maybeWriteFile content path = maybeWithFile WriteMode path $
    liftIO . flip LIO.hPutStrLn content

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

    liftIO (IO.withFile file mode (\h -> run (m h) symTbl opts)) >>= \case
        Left err                 -> throwError err
        Right (result, symTbl') -> do
            put symTbl'
            return result

liftEither' :: Either Error a -> ProFeat a
liftEither' = lift . liftEither

liftEither :: (Monad m) => Either e a -> ExceptT e m a
liftEither = ExceptT . return

-- | Like putStrLn, but only prints when 'Verbosity' is 'Verbose'.
vPutStrLn :: String -> ProFeat ()
vPutStrLn = onVerbose . liftIO . putStrLn

vPutStr :: String -> ProFeat ()
vPutStr = onVerbose . liftIO . putStr

onVerbose :: ProFeat () -> ProFeat ()
onVerbose = onVerbosity Verbose

onVerbosity :: Verbosity -> ProFeat () -> ProFeat ()
onVerbosity v = when' (return . (v ==) =<< asks verbosity)

when' :: Monad m => m Bool -> m () -> m ()
when' mb m = mb >>= \b -> when b m

-- | Applies an action to an option, but only when the option has been set.
with :: (ProFeatOptions -> Maybe a) -> (a -> ProFeat ()) -> ProFeat ()
with opt m = asks opt >>= \case
    Just o  -> m o
    Nothing -> return ()

pathToMaybe :: FilePath -> Maybe FilePath
pathToMaybe path = case path of
    "-" -> Nothing
    _   -> Just path

renderToFile :: (Pretty p) => FilePath -> p -> ProFeat ()
renderToFile path = liftIO . LIO.writeFile path . render

render :: (Pretty p) => p -> L.Text
render = displayT . renderPretty 0.4 80 . pretty

