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

import Control.Concurrent
import Control.Exception
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
import System.FilePath
import System.IO hiding ( withFile )
import qualified System.IO as IO
import System.IO.Error.Lens
import System.Process hiding ( readProcessWithExitCode )

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
--    --one-by-one
helpOneByOne = "Check (or export) all configurations one by one"
-- -o --export-model
helpExportModel = "Export the translated model to <file>"
-- -p --export-properties
helpExportProperties = "Export the translated properties to <file>"
-- -r --export-results
helpExportResults = "Export the results of model checking to <file>"
--    --prism-log
helpPrismLog = "Show PRISM log messages"
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
--    --prism-args
helpPrismArgs = "Pass <args> to PRISM"
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
  , oneByOne           :: !Bool
  , prismModelPath     :: Maybe FilePath
  , prismPropsPath     :: Maybe FilePath
  , proFeatResultsPath :: Maybe FilePath
  , showPrismLog       :: !Bool
  , prismResultsPath   :: Maybe FilePath
  , groupResults       :: !Bool
  , roundResults       :: Maybe Int
  , translateOnly      :: !Bool
  , modelCheckOnly     :: !Bool
  , prismExecPath      :: FilePath
  , prismArguments     :: Maybe String
  , verbosity          :: !Verbosity
  }

data Verbosity = Normal | Verbose deriving (Eq)

defaultOptions :: ProFeatOptions
defaultOptions = ProFeatOptions
  { proFeatModelPath   = "-"
  , proFeatPropsPath   = Nothing
  , oneByOne           = False
  , prismModelPath     = Nothing
  , prismPropsPath     = Nothing
  , proFeatResultsPath = Nothing
  , showPrismLog       = False
  , prismResultsPath   = Nothing
  , groupResults       = False
  , roundResults       = Nothing
  , translateOnly      = False
  , modelCheckOnly     = False
  , prismExecPath      = defaultPrismPath
  , prismArguments     = Nothing
  , verbosity          = Normal
  }

proFeatOptions :: Parser ProFeatOptions
proFeatOptions = ProFeatOptions
  <$> strArgument           ( metavar "<model-file>"
                           <> help helpModelFile )
  <*> optional (strArgument ( metavar "<properties-file>"
                           <> help helpPropsFile ))
  <*> switch                ( long "one-by-one"
                           <> help helpOneByOne )
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
  <*> switch                ( long "prism-log"
                           <> hidden
                           <> help helpPrismLog )
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
  <*> optional (strOption   ( long "prism-args"
                           <> metavar "<args>"
                           <> hidden
                           <> help helpPrismArgs ))
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
                writeProFeatOutput props [prismOutput]
        Nothing -> do
            vPutStr "Translating..."
            prismModels <- translate
            prismProps <- _Just translateProps proFeatProps
            vPutStrLn "done"

            with prismModelPath $ \p -> renderToFiles p prismModels
            with prismPropsPath $ void . for prismProps . renderToFile

            when' (asks translateOnly) $ liftIO exitSuccess

            prismOutputs <- for prismModels (callPrism prismProps)

            when' (asks modelCheckOnly) $ liftIO exitSuccess

            case proFeatProps of
                Just props -> writeProFeatOutput props prismOutputs
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

translate :: ProFeat [LModel]
translate = do
    symTbl       <- get
    genInstances <- asks oneByOne
    liftEither' $ if genInstances
        then translateModelInstances symTbl
        else (:[]) <$> translateModel symTbl

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
withTranslatedModel m = withProFeatModel $ do
    symTbl <- get
    m =<< liftEither' (translateModel symTbl)

translateProps :: LSpecification -> ProFeat LSpecification
translateProps spec = do
    symTbl <- get
    liftEither' $ translateSpec symTbl spec

callPrism :: Maybe LSpecification -> LModel -> ProFeat S.Text
callPrism prismProps prismModel = do
    vPutStr "Model Checking..."

    renderToFile modelPath prismModel

    args <- case prismProps of
        Nothing    -> return [modelPath]
        Just props -> do
            renderToFile propsPath props
            return [modelPath, propsPath]

    prismPath <- asks prismExecPath
    prismArgs <- maybe [] words <$> asks prismArguments

    (exitCode, std, err) <- liftIO $
        readProcessWithExitCode prismPath (args ++ prismArgs)

    vPutStrLn "done"

    unless (exitCode == ExitSuccess) . liftIO $ do
        SIO.hPutStrLn stdout std
        SIO.hPutStrLn stderr err
        exitWith exitCode

    return std
  where
    modelPath = "out.prism"
    propsPath = "out.props"

writeProFeatOutput :: LSpecification -> [S.Text] -> ProFeat ()
writeProFeatOutput spec prismOutputs = do
    vPutStr "Processing results..."
    proFeatOutput <- postprocessPrismOutput spec =<<
                     parsePrismOutputs prismOutputs
    vPutStrLn "done"

    maybeWriteFile proFeatOutput =<< asks proFeatResultsPath

parsePrismOutputs :: [S.Text] -> ProFeat [ResultCollection]
parsePrismOutputs []      = return []
parsePrismOutputs outputs = do
    rcs <- traverse parse outputs
    return $ foldr1 (zipWith appendResultCollection) rcs
  where
    parse :: S.Text -> ProFeat [ResultCollection]
    parse out = do
        symTbl <- get
        return $ parseResultCollections (varOrdering symTbl) out

postprocessPrismOutput :: LSpecification -> [ResultCollection] -> ProFeat L.Text
postprocessPrismOutput spec = (return . fmap removeNonConfVars)
    >=> (return . fmap sortStateResults)
    >=> applyRounding
    >=> applyGrouping
    >=> renderResultCollections
  where
    applyRounding rcs = asks roundResults <&> \case
        Just precision -> fmap (roundStateResults precision) rcs
        Nothing        -> rcs

    applyGrouping rcs = do
        gr <- asks groupResults
        return $ if gr
            then fmap groupStateVecs rcs
            else rcs

    renderResultCollections rcs = do
        showLog <- asks showPrismLog
        return . displayT . renderPretty 1.0 300 $
            prettyResultCollections showLog spec rcs

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

renderToFiles :: (Pretty p) => FilePath -> [p] -> ProFeat ()
renderToFiles path = \case
    []  -> return ()
    [x] -> renderToFile path x
    xs  -> void . for (zip xs [0 :: Integer ..]) $ \(x, i) ->
               renderToFile (path `addIndex` i) x
  where
    addIndex p i =
        let (file, ext) = splitExtensions p
        in (file ++ "_" ++ show i) `addExtension` ext

renderToFile :: (Pretty p) => FilePath -> p -> ProFeat ()
renderToFile path = liftIO . LIO.writeFile path . render

render :: (Pretty p) => p -> L.Text
render = displayT . renderPretty 0.4 80 . pretty

readProcessWithExitCode :: FilePath -> [String] -> IO (ExitCode, S.Text, S.Text)
readProcessWithExitCode cmd args = mask $ \restore -> do
    let p = (proc cmd args)
              { std_out = CreatePipe
              , std_err = CreatePipe
              }
    (_, Just hOut, Just hErr, pid) <- createProcess p

    flip onException (terminateProcess pid >> release hOut hErr pid) $ restore $ do
        mOut <- newEmptyMVar
        mErr <- newEmptyMVar

        _ <- forkIO (SIO.hGetContents hOut >>= putMVar mOut)
        _ <- forkIO (SIO.hGetContents hErr >>= putMVar mErr)

        out <- readMVar mOut
        err <- readMVar mErr

        exitCode <- release hOut hErr pid

        return (exitCode, out, err)
  where
    release hOut hErr pid = do
        hClose hOut
        hClose hErr
        waitForProcess pid

