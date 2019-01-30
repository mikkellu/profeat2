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
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable ( for_ )
import qualified Data.Map as Map
import Data.Maybe
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

import Error
import FeatureDiagram
import qualified FeatureVars
import Parser
import Parser.Results
import Result
import Result.Constraint
import Result.Csv
import Result.Mtbdd
import SymbolTable
import Syntax
import Translator
import Types
import VarOrder

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
--    --constant-folding
helpConstantFolding = "Evaluate constant subexpressions"
-- -o --export-model
helpExportModel = "Export the translated model to <file>"
-- -p --export-properties
helpExportProperties = "Export the translated properties to <file>"
--    --export-fd
helpExportFeatureDiagram = "Export a feature diagram to <file> (in dot format)"
--    --export-vars
helpExportVars = "Export the mapping of feature names to feature variables as CSV to <file>"
-- -r --export-results
helpExportResults = "Export the results of model checking as CSV to <file>"
--    --above-threshold
helpAboveThreshold = "Compute a constraint such that the result is above the given threshold for all products"
--    --export-mtbdd
helpExportMtbdd = "Export a decision diagram representing the results to <file> (in dot format)"
--    --full-mtbdd
helpFullMtbdd = "Export the full decision diagram. The full diagram also encodes the set of initial configurations."
--    --reorder-mtbdd
helpReorderMtbdd = "Try to reduce the size of the diagram exported by the export-mtbdd option"
--    --prism-log
helpPrismLog = "Show PRISM log messages"
--    --import-results
helpImportResults = "Import the PRISM results from <path> for postprocessing"
--    --round-results
helpRoundResults = "Round results to <precision> digits"
--    --sort-results
helpSortResults = "Sort configurations by their result (descending)"
-- -t --translate
helpTranslate = "Translate only, do not model check"
-- -m --model-checking
helpModelChecking = "Translate and model check only, do not postprocess results"
--    --prism-path
helpPrismPath    = "Set the path of the PRISM executable"
defaultPrismPath = "prism"
--    --prism-args
helpPrismArgs = "Pass <args> to PRISM"
--    --prism-calls
helpPrismCalls = "Write the list of PRISM calls to <file>"
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
  { proFeatModelPath      :: FilePath
  , proFeatPropsPath      :: Maybe FilePath
  , oneByOne              :: !Bool
  , constantFolding       :: !Bool
  , prismModelPath        :: Maybe FilePath
  , prismPropsPath        :: Maybe FilePath
  , featureDiagramPath    :: Maybe FilePath
  , featureVarsPath       :: Maybe FilePath
  , proFeatResultsPath    :: Maybe FilePath
  , resultsAboveThreshold :: Maybe Double
  , resultMtbddPath       :: Maybe FilePath
  , fullMtbdd             :: !ReduceOpts
  , reorderMtbdd          :: !ReorderOpts
  , showPrismLog          :: !Bool
  , prismResultsPath      :: Maybe FilePath
  , roundResults          :: Maybe Int
  , sortResults           :: !Bool
  , translateOnly         :: !Bool
  , modelCheckOnly        :: !Bool
  , prismExecPath         :: FilePath
  , prismArguments        :: Maybe String
  , prismCallsPath        :: Maybe FilePath
  , verbosity             :: !Verbosity
  }

data Verbosity = Normal | Verbose deriving (Eq)

defaultOptions :: ProFeatOptions
defaultOptions = ProFeatOptions
  { proFeatModelPath      = "-"
  , proFeatPropsPath      = Nothing
  , oneByOne              = False
  , constantFolding       = False
  , prismModelPath        = Nothing
  , prismPropsPath        = Nothing
  , featureDiagramPath    = Nothing
  , featureVarsPath       = Nothing
  , proFeatResultsPath    = Nothing
  , resultsAboveThreshold = Nothing
  , resultMtbddPath       = Nothing
  , fullMtbdd             = ReducedMtbdd
  , reorderMtbdd          = NoReordering
  , showPrismLog          = False
  , prismResultsPath      = Nothing
  , roundResults          = Nothing
  , sortResults           = False
  , translateOnly         = False
  , modelCheckOnly        = False
  , prismExecPath         = defaultPrismPath
  , prismArguments        = Nothing
  , prismCallsPath        = Nothing
  , verbosity             = Normal
  }

proFeatOptions :: Parser ProFeatOptions
proFeatOptions = ProFeatOptions
  <$> strArgument               ( metavar "<model-file>"
                               <> help helpModelFile )
  <*> optional (strArgument     ( metavar "<properties-file>"
                               <> help helpPropsFile ))
  <*> switch                    ( long "one-by-one"
                               <> help helpOneByOne )
  <*> switch                    ( long "constant-folding"
                               <> help helpConstantFolding )
  <*> optional (strOption       ( long "export-model" <> short 'o'
                               <> metavar "<file>"
                               <> help helpExportModel ))
  <*> optional (strOption       ( long "export-properties" <> short 'p'
                               <> metavar "<file>"
                               <> help helpExportProperties ))
  <*> optional (strOption       ( long "export-fd"
                               <> metavar "<file>"
                               <> hidden
                               <> help helpExportFeatureDiagram ))
  <*> optional (strOption       ( long "export-vars"
                               <> metavar "<file>"
                               <> hidden
                               <> help helpExportVars ))
  <*> optional (strOption       ( long "export-results" <> short 'r'
                               <> metavar "<file>"
                               <> hidden
                               <> help helpExportResults ))
  <*> optional (option auto     ( long "above-threshold"
                               <> metavar "<threshold>"
                               <> hidden
                               <> help helpAboveThreshold ))
  <*> optional (strOption       ( long "export-mtbdd"
                               <> metavar "<file>"
                               <> hidden
                               <> help helpExportMtbdd ))
  <*> flag ReducedMtbdd FullMtbdd
                                ( long "full-mtbdd"
                               <> hidden
                               <> help helpFullMtbdd )
  <*> flag NoReordering Reorder ( long "reorder-mtbdd"
                               <> hidden
                               <> help helpReorderMtbdd )
  <*> switch                    ( long "prism-log"
                               <> hidden
                               <> help helpPrismLog )
  <*> optional (strOption       ( long "import-results"
                               <> metavar "<path>"
                               <> hidden
                               <> help helpImportResults ))
  <*> optional (option auto      ( long "round-results"
                               <> metavar "<precision>"
                               <> hidden
                               <> help helpRoundResults ))
  <*> switch                    ( long "sort-results"
                               <> hidden
                               <> help helpSortResults
                                )
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
  <*> optional (strOption       ( long "prism-calls"
                               <> metavar "<path>"
                               <> hidden
                               <> help helpPrismCalls ))
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
            Just props -> do
                (infos, paramVarMap) <- instanceInfos model
                let vals = fmap infoParamValuation infos
                case infos of
                    [] -> return ()
                    [_] -> withFile resultsPath ReadMode $ \hIn -> do
                        prismOutput <- liftIO $ SIO.hGetContents hIn
                        writeProFeatOutput paramVarMap props vals [prismOutput]
                    _ -> do
                        prismOutputs <- for (zip infos [0..]) $ \(_, k) ->
                            liftIO $ SIO.readFile (resultsPath `addFileIndex` k)
                        writeProFeatOutput paramVarMap props vals prismOutputs
        Nothing -> do
            liftIO $ hSetBuffering stdout LineBuffering

            vPutStr "Translating..."
            (infos, paramVarMap) <- instanceInfos model
            cf <- asks constantFolding

            withDefault "out.prism" prismModelPath $ \p -> case infos of
                [] -> return ()
                [i] -> do
                    model' <- liftEither' (translateModel cf i)
                    renderToFile p model'
                _ -> for_ (zip infos [0..]) $ \(i, k) -> do
                    model' <- liftEither' (translateModel cf i)
                    renderToFile (p `addFileIndex` k) model'

            prismProps <- _Just translateProps proFeatProps
            withDefault "out.props" prismPropsPath $
                for_ prismProps . renderToFile

            vPutStrLn "done"

            writePrismCallsFile (length infos) prismProps
            void $ asks featureDiagramPath >>= _Just writeFeatureDiagramFile
            writeFeatureVarsFile

            when' (asks translateOnly) $ liftIO exitSuccess

            prismOutputs <- callPrism (length infos) prismProps

            when' (asks modelCheckOnly) $ liftIO exitSuccess

            case proFeatProps of
                Just props ->
                    let vals = fmap infoParamValuation infos
                    in writeProFeatOutput paramVarMap props vals prismOutputs
                Nothing    -> return ()

withProFeatModel :: (LModel -> ProFeat a) -> ProFeat a
withProFeatModel m = do
    path <- asks proFeatModelPath
    maybeWithFile ReadMode (pathToMaybe path) $ \hIn -> do
        modelContents <- liftIO $ LIO.hGetContents hIn
        m =<< liftEither' (parseModel path modelContents)

instanceInfos :: LModel -> ProFeat ([InstanceInfo], VarMap)
instanceInfos model = do
    genInstances <- asks oneByOne
    if genInstances
        then do
            (infos, paramVarMap) <- liftEither' (infosOneByOne model)
            unless (null infos) $ do
                let InstanceInfo symTbl' _ _ _ = last infos
                put symTbl'
            return (infos, paramVarMap)
        else do
            i <- liftEither' (infoAllInOne model)
            put (infoSymbolTable i)
            return ([i], Map.empty)

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
    i <- liftEither' (infoAllInOne model)
    put (infoSymbolTable i)
    model' <- liftEither' (translateModel False i)
    m model'

translateProps :: LSpecification -> ProFeat LSpecification
translateProps spec = do
    symTbl <- get
    liftEither' $ translateSpec symTbl spec

callPrism :: Int -> Maybe LSpecification -> ProFeat [S.Text]
callPrism numModels prismProps = do
    ps <- modelPaths numModels

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

modelPaths :: Int -> ProFeat [FilePath]
modelPaths numModels = do
    path <- lookupPath "out.prism" prismModelPath
    return $ if numModels == 1
        then [path]
        else fmap (path `addFileIndex`) [0..numModels - 1]

writeProFeatOutput
    :: VarMap -> LSpecification -> [Valuation] -> [S.Text] -> ProFeat ()
writeProFeatOutput paramVarMap spec vals prismOutputs = do
    vPutStr "Processing results..."
    postprocessPrismOutput paramVarMap
                           spec
                           (parsePrismOutputs vals prismOutputs)

parsePrismOutputs :: [Valuation] -> [S.Text] -> [ResultCollection]
parsePrismOutputs _ [] = []
parsePrismOutputs vals outputs =
    let rcs = fmap
            (\(output, val) -> fmap (addParameterValues val)
                                    (parseResultCollections output)
            )
            (zip outputs vals)
    in  foldr1 (zipWith appendResultCollection) rcs

postprocessPrismOutput
    :: VarMap -> LSpecification -> [ResultCollection] -> ProFeat ()
postprocessPrismOutput paramVarMap spec rcs = do
    bsort <- asks sortResults
    let fsort = if bsort then sortStateResults else id

    let filteredRcs = filter (isJust . _rcFinalResult) rcs
        rcs'        = fmap (fsort . removeNonConfVars) filteredRcs
    rcs'' <- applyRounding rcs'

    writeCsvFiles spec rcs''
    writeMtbddFiles rcs''

    vm <- gets varMap
    let vm' = Map.union vm paramVarMap

    showLog <- asks showPrismLog
    let doc = if null filteredRcs
                  then prettyResultCollections vm' (not showLog) spec rcs -- only show PRISM log in case it hasn't been shown beforehand
                  else prettyResultCollections vm' False spec rcs''
        proFeatOutput = displayT (renderPretty 1.0 300 doc)

    liftIO (LIO.putStrLn proFeatOutput)

    showThresholdConstraints rcs''
  where
    applyRounding rcs' = asks roundResults <&> \case
        Just precision -> fmap (roundStateResults precision) rcs'
        Nothing        -> rcs'

writePrismCallsFile :: Int -> Maybe LSpecification -> ProFeat ()
writePrismCallsFile numModels prismProps = asks prismCallsPath >>= \case
    Nothing -> return ()
    Just path -> withFile path WriteMode $ \hOut -> do
        propsArg <- if isJust prismProps
                        then (:[]) <$> lookupPath "out.props" prismPropsPath
                        else return []
        prismPath <- asks prismExecPath
        prismArgs <- maybe [] words <$> asks prismArguments

        ps <- modelPaths numModels

        for_ (zip [0 ..] ps) $ \(i, modelPath) -> do
            let logArg = "-mainlog " ++ ("out.log" `addFileIndex` i)
                args = (modelPath:propsArg) ++ prismArgs ++ [logArg]
            liftIO $ hPutStrLn hOut (unwords (prismPath:args))

writeFeatureDiagramFile :: FilePath -> ProFeat ()
writeFeatureDiagramFile path = do
    symTbl <- get
    liftIO $ writeFeatureDiagram path (symTbl^.rootFeature)

writeCsvFiles :: LSpecification -> [ResultCollection] -> ProFeat ()
writeCsvFiles (Specification defs) rcs = asks proFeatResultsPath >>= \case
    Nothing   -> return ()
    Just path -> do
        let props = defs^..traverse._PropertyDef
        let (name, ext) = splitExtension path

        vm <- gets varMap
        root <- use rootFeature

        for_ (zip3 rcs props [1 :: Integer ..]) $ \(rc, prop, idx) -> do
            let path' = addExtension (name ++ "_" ++ show idx) ext
                csv   = displayT (renderPretty 1.0 300 (toCsv root vm prop rc))
            liftIO $ LIO.writeFile path' csv

writeFeatureVarsFile :: ProFeat ()
writeFeatureVarsFile = asks featureVarsPath >>= \case
    Nothing -> return ()
    Just path -> do
        root <- use rootFeature
        let csv = displayT (renderPretty 1.0 300 (FeatureVars.toCsv root))
        liftIO $ LIO.writeFile path csv

showThresholdConstraints :: [ResultCollection] -> ProFeat ()
showThresholdConstraints rcs = asks resultsAboveThreshold >>= \case
    Nothing -> return ()
    Just threshold -> do
        vm <- gets varMap
        for_ rcs $ \rc -> do
            let vo = toVarOrder vm $ rc^.rcVariables
            liftIO . putStrLn $ "Constraint for threshold >= " ++ show threshold
            liftIO $ print (constraintsFor vo rc (prop threshold))
  where
    prop t = \case
        ResultDouble r | r >= t -> True
        _                       -> False

writeMtbddFiles :: [ResultCollection] -> ProFeat ()
writeMtbddFiles rcs = asks resultMtbddPath >>= \case
    Nothing   -> return ()
    Just path -> do
        let (name, ext) = splitExtension path

        vm <- gets varMap

        tPred <- flip fmap (asks resultsAboveThreshold) $ \case
            Just threshold -> thresholdFunc threshold
            Nothing        -> id
        opts <- MtbddOpts <$> asks fullMtbdd
                          <*> asks reorderMtbdd
                          <*> pure tPred

        for_ (zip rcs [1 :: Integer ..]) $ \(rc, idx) -> do
            let vo = toVarOrder vm $ rc^.rcVariables
            let path' = addExtension (name ++ "_" ++ show idx) ext
            liftIO $ writeMtbdd opts vo path' (rc^.rcStateResults)
  where
    thresholdFunc threshold = \case
        Just d | d >= threshold -> Just 1.0
               | otherwise      -> Just 0.0
        Nothing -> Nothing

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
