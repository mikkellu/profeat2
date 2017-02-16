{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

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

import Data.Foldable ( for_ )
import Data.Maybe
import Data.Monoid
import qualified Data.Text as S
import qualified Data.Text.IO as SIO
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import Data.Traversable
import Data.Version ( showVersion )

import Options.Applicative

import System.Exit
import System.FilePath
import System.IO hiding ( withFile )
import qualified System.IO as IO
import System.IO.Error.Lens
import System.Process hiding ( readProcessWithExitCode )

import Text.PrettyPrint.Leijen.Text ( Pretty, displayT, pretty, renderPretty )

import Analysis.VarOrder
import Error
import Parser
import Parser.Results
import Result
import Result.Csv
import Result.Diagram
import SymbolTable
import Syntax
import Translator
import Types

import Paths_profeat ( version )

proFeatVersion :: String
proFeatVersion = showVersion version


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
helpExportResults = "Export the results of model checking as CSV to <file>"
--    --export-diagram
helpExportDiagram = "Export a decision diagram representing the results to <file> (in dot format)"
--    --full-diagram
helpFullDiagram = "Export the full decision diagram. The full diagram also encodes the set of initial configurations."
--    --reorder-diagram
helpReorderDiagram = "Try to reduce the size of the diagram exported by the export-diagram option"
--    --prism-log
helpPrismLog = "Show PRISM log messages"
--    --import-results
helpImportResults = "Import the PRISM results from <file> for postprocessing"
--    --round-results
helpRoundResults = "Round results to <precision> digits"
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
    options      = info (helper <*> versionOpt <*> proFeatOptions) mempty
    versionOpt   = infoOption proFeatVersion
                       ( long "version"
                      <> hidden
                      <> help "Display version information")
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
  , resultDiagramPath  :: Maybe FilePath
  , fullDiagram        :: !ReduceOpts
  , reorderDiagram     :: !ReorderOpts
  , showPrismLog       :: !Bool
  , prismResultsPath   :: Maybe FilePath
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
  , resultDiagramPath  = Nothing
  , fullDiagram        = ReducedDiagram
  , reorderDiagram     = NoReordering
  , showPrismLog       = False
  , prismResultsPath   = Nothing
  , roundResults       = Nothing
  , translateOnly      = False
  , modelCheckOnly     = False
  , prismExecPath      = defaultPrismPath
  , prismArguments     = Nothing
  , verbosity          = Normal
  }

proFeatOptions :: Parser ProFeatOptions
proFeatOptions = ProFeatOptions
  <$> strArgument               ( metavar "<model-file>"
                               <> help helpModelFile )
  <*> optional (strArgument     ( metavar "<properties-file>"
                               <> help helpPropsFile ))
  <*> switch                    ( long "one-by-one"
                               <> help helpOneByOne )
  <*> optional (strOption       ( long "export-model" <> short 'o'
                               <> metavar "<file>"
                               <> help helpExportModel ))
  <*> optional (strOption       ( long "export-properties" <> short 'p'
                               <> metavar "<file>"
                               <> help helpExportProperties ))
  <*> optional (strOption       ( long "export-results" <> short 'r'
                               <> metavar "<file>"
                               <> hidden
                               <> help helpExportResults ))
  <*> optional (strOption       ( long "export-diagram"
                               <> metavar "<file>"
                               <> hidden
                               <> help helpExportDiagram ))
  <*> flag ReducedDiagram FullDiagram
                                ( long "full-diagram"
                               <> hidden
                               <> help helpFullDiagram )
  <*> flag NoReordering Reorder ( long "reorder-diagram"
                               <> hidden
                               <> help helpReorderDiagram )
  <*> switch                    ( long "prism-log"
                               <> hidden
                               <> help helpPrismLog )
  <*> optional (strOption       ( long "import-results"
                               <> metavar "<file>"
                               <> hidden
                               <> help helpImportResults ))
  <*> optional (option auto      ( long "round-results"
                               <> metavar "<precision>"
                               <> hidden
                               <> help helpRoundResults ))
  <*> switch                    ( long "translate" <> short 't'
                               <> hidden
                               <> help helpTranslate )
  <*> switch                    ( long "model-checking" <> short 'm'
                               <> hidden
                               <> help helpModelChecking )
  <*> strOption                 ( long "prism-path"
                               <> metavar "<path>"
                               <> value defaultPrismPath
                               <> showDefault
                               <> hidden
                               <> help helpPrismPath )
  <*> optional (strOption       ( long "prism-args"
                               <> metavar "<args>"
                               <> hidden
                               <> help helpPrismArgs ))
  <*> flag Normal Verbose       ( long "verbose" <> short 'v'
                               <> hidden
                               <> help helpVerbose )

type ProFeat = StateT SymbolTable (ExceptT Error (ReaderT ProFeatOptions IO))

proFeat :: ProFeat ()
proFeat = withProFeatModel $ \model -> withProFeatProps $ \proFeatProps ->
    asks prismResultsPath >>= \case
        Just resultsPath -> case proFeatProps of -- only do postprocessing
            Nothing -> liftIO $ do
                hPutStrLn stderr "Could not postprocess PRISM results, no ProFeat properties list given"
                exitWith $ ExitFailure 4
            Just props -> withFile resultsPath ReadMode $ \hIn -> do
                (_, symTbl) <- liftEither' (translateModel model)
                put symTbl

                prismOutput <- liftIO $ SIO.hGetContents hIn
                writeProFeatOutput props [prismOutput]
        Nothing -> do
            vPutStr "Translating..."
            prismModels <- translate model
            prismProps <- _Just translateProps proFeatProps
            vPutStrLn "done"

            withDefault "out.prism" prismModelPath $ \p ->
                renderToFiles p prismModels
            withDefault "out.props" prismPropsPath $
                for_ prismProps . renderToFile

            when' (asks translateOnly) $ liftIO exitSuccess

            prismOutputs <- callPrism prismModels prismProps

            when' (asks modelCheckOnly) $ liftIO exitSuccess

            case proFeatProps of
                Just props -> writeProFeatOutput props prismOutputs
                Nothing    -> return ()

withProFeatModel :: (LModel -> ProFeat a) -> ProFeat a
withProFeatModel m = do
    path <- asks proFeatModelPath
    maybeWithFile ReadMode (pathToMaybe path) $ \hIn -> do
        modelContents <- liftIO $ LIO.hGetContents hIn
        m =<< liftEither' (parseModel path modelContents)

translate :: LModel -> ProFeat [LModel]
translate model = do
    genInstances <- asks oneByOne
    if genInstances
        then do
            (models', symTbl) <- liftEither' (translateModelInstances model)
            put symTbl
            return models'
        else do
            (model', symTbl) <- liftEither' (translateModel model)
            put symTbl
            return [model']

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
withTranslatedModel m = withProFeatModel $ \model -> do
    (model', symTbl) <- liftEither' (translateModel model)
    put symTbl
    m model'

translateProps :: LSpecification -> ProFeat LSpecification
translateProps spec = do
    symTbl <- get
    liftEither' $ translateSpec symTbl spec

callPrism :: [LModel] -> Maybe LSpecification -> ProFeat [S.Text]
callPrism prismModels prismProps = do
    ps <- modelPaths
    let numModels = length ps

    propsArg <- if isJust prismProps
                    then (:[]) <$> lookupPath "out.props" prismPropsPath
                    else return []
    prismPath <- asks prismExecPath
    prismArgs <- maybe [] words <$> asks prismArguments

    for (zip [1 :: Integer ..] ps) $ \(i, modelPath) -> do
        vPutStr $ "Model Checking (" ++ show i ++ "/" ++ show numModels ++ ") "

        let args = (modelPath:propsArg) ++ prismArgs
        showLog <- asks showPrismLog

        (exitCode, std, err) <- liftIO $
            readProcessWithExitCode showLog prismPath args

        vPutStrLn "done"

        unless (exitCode == ExitSuccess) . liftIO $ do
            SIO.hPutStrLn stdout std
            SIO.hPutStrLn stderr err
            exitWith exitCode

        return std
  where
    modelPaths :: ProFeat [FilePath]
    modelPaths = do
        path <- lookupPath "out.prism" prismModelPath
        return $ if length prismModels == 1
            then [path]
            else fmap (path `addFileIndex`) [0..length prismModels - 1]

writeProFeatOutput :: LSpecification -> [S.Text] -> ProFeat ()
writeProFeatOutput spec prismOutputs = do
    vPutStr "Processing results..."
    proFeatOutput <- postprocessPrismOutput spec =<<
                     parsePrismOutputs prismOutputs
    vPutStrLn "done"

    liftIO (LIO.putStrLn proFeatOutput)

parsePrismOutputs :: [S.Text] -> ProFeat [ResultCollection]
parsePrismOutputs []      = return []
parsePrismOutputs outputs = do
    rcs <- traverse parse outputs
    return $ foldr1 (zipWith appendResultCollection) rcs
  where
    parse :: S.Text -> ProFeat [ResultCollection]
    parse out = do
        symTbl <- get
        return $ parseResultCollections (varOrder symTbl) out

postprocessPrismOutput :: LSpecification -> [ResultCollection] -> ProFeat L.Text
postprocessPrismOutput spec rcs = do
    let filteredRcs = filter (isJust . _rcFinalResult) rcs
        rcs'        = fmap (sortStateResults . removeNonConfVars) filteredRcs
    rcs'' <- applyRounding rcs'

    writeCsvFiles rcs''
    writeDiagramFiles rcs''

    showLog <- asks showPrismLog
    let doc = if null filteredRcs
                  then prettyResultCollections (not showLog) spec rcs -- only show PRISM log in case it hasn't been shown beforehand
                  else prettyResultCollections False spec rcs''
    return (displayT (renderPretty 1.0 300 doc))
  where
    applyRounding rcs' = asks roundResults <&> \case
        Just precision -> fmap (roundStateResults precision) rcs'
        Nothing        -> rcs'

writeCsvFiles :: [ResultCollection] -> ProFeat ()
writeCsvFiles rcs = asks proFeatResultsPath >>= \case
    Nothing   -> return ()
    Just path -> do
        let (name, ext) = splitExtension path

        for_ (zip rcs [1 :: Integer ..]) $ \(rc, idx) -> do
            let path' = addExtension (name ++ "_" ++ show idx) ext
                csv   = displayT (renderPretty 1.0 300 (toCsv rc))
            liftIO $ LIO.writeFile path' csv

writeDiagramFiles :: [ResultCollection] -> ProFeat ()
writeDiagramFiles rcs = asks resultDiagramPath >>= \case
    Nothing   -> return ()
    Just path -> do
        let (name, ext) = splitExtension path

        symTbl <- get
        let vo = varOrder symTbl

        opts <- DiagramOpts <$> asks fullDiagram <*> asks reorderDiagram

        for_ (zip rcs [1 :: Integer ..]) $ \(rc, idx) -> do
            let path' = addExtension (name ++ "_" ++ show idx) ext
            liftIO $ writeDiagram opts vo path' (rc^.rcStateResults)

runApp :: ProFeat () -> ProFeatOptions -> IO ()
runApp m opts = do
    result <- run m (emptySymbolTable defaultModelType) opts
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
onVerbosity v = when' (fmap (v ==) (asks verbosity))

when' :: Monad m => m Bool -> m () -> m ()
when' mb m = mb >>= \b -> when b m

withDefault
    :: FilePath
    -> (ProFeatOptions -> Maybe FilePath)
    -> (FilePath -> ProFeat ())
    -> ProFeat ()
withDefault def opt m = lookupPath def opt >>= m

lookupPath :: FilePath -> (ProFeatOptions -> Maybe FilePath) -> ProFeat FilePath
lookupPath def opt = flip fmap (asks opt) $ \case
    Just path -> path
    Nothing   -> def

pathToMaybe :: FilePath -> Maybe FilePath
pathToMaybe path = case path of
    "-" -> Nothing
    _   -> Just path

renderToFiles :: (Pretty p) => FilePath -> [p] -> ProFeat ()
renderToFiles path = \case
    []  -> return ()
    [x] -> renderToFile path x
    xs  -> void . for (zip xs [0..]) $ \(x, i) ->
               renderToFile (path `addFileIndex` i) x

addFileIndex :: FilePath -> Int -> FilePath
addFileIndex p i =
    let (file, ext) = splitExtensions p
    in (file ++ "_" ++ show i) `addExtension` ext

renderToFile :: (Pretty p) => FilePath -> p -> ProFeat ()
renderToFile path = liftIO . LIO.writeFile path . render

render :: (Pretty p) => p -> L.Text
render = displayT . renderPretty 0.4 80 . pretty

readProcessWithExitCode
    :: Bool -> FilePath -> [String] -> IO (ExitCode, S.Text, S.Text)
readProcessWithExitCode echo cmd args = mask $ \restore -> do
    let p = (proc cmd args)
              { std_out = CreatePipe
              , std_err = CreatePipe
              }
    (_, Just hOut, Just hErr, pid) <- createProcess p

    flip onException (terminateProcess pid >> release hOut hErr pid) $ restore $ do
        mOut <- newEmptyMVar
        mErr <- newEmptyMVar

        _ <- forkIO (getLog hOut >>= putMVar mOut)
        _ <- forkIO (getLog hErr >>= putMVar mErr)

        out <- readMVar mOut
        err <- readMVar mErr

        exitCode <- release hOut hErr pid

        return (exitCode, out, err)
  where
    getLog
      | echo      = tee
      | otherwise = SIO.hGetContents
    release hOut hErr pid = do
        hClose hOut
        hClose hErr
        waitForProcess pid
    tee h = loop [] where
        loop ls = do
            eof <- hIsEOF h
            if eof
                then return $ S.unlines (reverse ls)
                else do
                    l <- SIO.hGetLine h
                    SIO.putStrLn l
                    loop (l:ls)
