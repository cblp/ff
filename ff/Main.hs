{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad (forever, guard)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           CRDT.LamportClock (getRealLocalTime)
import           Data.Either.Extra (fromEither)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Data.Text.IO (hPutStrLn)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as Text
import qualified System.Console.Terminal.Size as Terminal
import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   getHomeDirectory)
import           System.FilePath (FilePath, normalise, splitDirectories, (</>))
import           System.IO (hPutChar, hPutStr, stderr)
import           Text.PrettyPrint.Mainland (prettyLazyText)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF (cmdDelete, cmdDone, cmdEdit, cmdNew, cmdPostpone,
                     cmdSearch, cmdServe, cmdUnarchive, getSamples, getUtcToday,
                     newTrackedNote)
import           FF.Config (Config (..), ConfigUI (..), appName, loadConfig,
                            printConfig, saveConfig)
import           FF.Github (getIssueSamples, getIssueViews)
import           FF.Options (Cmd (..), CmdAction (..), CmdTrack (..),
                             DataDir (..), Search (..), Shuffle (..),
                             parseOptions)
import qualified FF.Options as Options
import           FF.Storage (Storage, runStorage)
import           FF.UI (withHeader)
import qualified FF.UI as UI
import           System.Pager (printOrPage)

import           Data.Version (showVersion)
import           Development.GitRev (gitDirty, gitHash)
import           Paths_ff (version)

main :: IO ()
main = do
    cfg@Config { ui } <- loadConfig
    cmd               <- parseOptions
    case cmd of
        CmdConfig param  -> runCmdConfig cfg param
        CmdAction action -> do
            timeVar <- newTVarIO =<< getRealLocalTime
            dataDir <- getDataDir cfg
            runStorage dataDir timeVar $ runCmdAction ui action
        CmdVersion -> runCmdVersion

getDataDir :: Config -> IO FilePath
getDataDir cfg = do
    cur <- getCurrentDirectory
    mDataDirFromVcs <- findVcs $ parents cur
    case mDataDirFromVcs of
        Just dataDir -> pure dataDir
        Nothing      -> checkDataDir cfg
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise
    findVcs []         = pure Nothing
    findVcs (dir:dirs) = do
        isDirVcs <- doesDirectoryExist (dir </> ".git")
        if isDirVcs then
            pure . Just $ dir </> ".ff"
        else
            findVcs dirs

runCmdConfig :: Config -> Maybe Options.Config -> IO ()
runCmdConfig cfg@Config { dataDir, ui } = \case
    Nothing                           -> printConfig cfg
    Just (Options.ConfigDataDir mDir) -> do
        dir <- case mDir of
            Nothing                -> pure dataDir
            Just (DataDirJust dir) -> saveDataDir dir
            Just DataDirYandexDisk -> do
                home <- getHomeDirectory
                asum
                    [ trySaveDataDir $ home </> "Yandex.Disk"
                    , trySaveDataDir $ home </> "Yandex.Disk.localized"
                    , fail "Cant't detect Yandex.Disk directory"
                    ]
        printConfig dir
    Just (Options.ConfigUI mShuffle) -> do
        ui' <- case mShuffle of
            Nothing      -> pure ui
            Just Shuffle -> saveShuffle True
            Just Sort    -> saveShuffle False
        printConfig ui'
  where
    trySaveDataDir baseDir = do
        guard =<< doesDirectoryExist baseDir
        saveDataDir $ baseDir </> "Apps" </> appName
    saveDataDir dir = saveConfig cfg { dataDir = Just dir } $> Just dir
    saveShuffle shuffle' = saveConfig cfg { ui = ui' } $> ui'
        where ui' = ConfigUI {shuffle = shuffle'}

checkDataDir :: Monad m => Config -> m FilePath
checkDataDir Config { dataDir } = case dataDir of
    Just dir -> pure dir
    Nothing  -> fail "Data directory isn't set, run `ff config dataDir --help`"

runCmdAction :: ConfigUI -> CmdAction -> Storage ()
runCmdAction ui cmd = do
    today <- getUtcToday
    case cmd of
        CmdAgenda mlimit -> do
            nvs <- getSamples ui mlimit today
            pprint $ UI.prettySamplesBySections nvs
        CmdDelete noteId -> do
            nv <- cmdDelete noteId
            pprint $ withHeader "deleted:" $ UI.noteView nv
        CmdDone noteId -> do
            nv <- cmdDone noteId
            pprint $ withHeader "archived:" $ UI.noteView nv
        CmdEdit edit -> do
            nv <- cmdEdit edit
            pprint $ withHeader "edited:" $ UI.noteView nv
        CmdTrack (Track True address limit ) -> liftIO $ do
            hPutStr stderr "fetching"
            possibleIssues <- fromEither <$> race
                (runExceptT $ getIssueSamples address limit today)
                (forever $ hPutChar stderr '.' >> threadDelay 500000)
            hPutStrLn stderr ""
            case possibleIssues of
                Left err      -> hPutStrLn stderr err
                Right samples -> pprint $ UI.prettySamplesBySections samples
        CmdTrack (Track False address limit) -> do
            nvs <- liftIO $ runExceptT $ getIssueViews address limit
            case nvs of
                Left err   -> liftIO $ hPutStrLn stderr err
                Right nvs' -> do
                    mapM_ newTrackedNote nvs'
                    let nvsLength = show $ length nvs'
                    liftIO $ putStrLn $ nvsLength ++ " issues copied to local base"
        CmdNew new -> do
            nv <- cmdNew new today
            pprint $ withHeader "added:" $ UI.noteView nv
        CmdPostpone noteId -> do
            nv <- cmdPostpone noteId
            pprint $ withHeader "postponed:" $ UI.noteView nv
        CmdSearch (Search text mlimit) -> do
            nvs <- cmdSearch text mlimit today
            pprint $ UI.prettySamplesBySections nvs
        CmdUnarchive noteId -> do
            nv <- cmdUnarchive noteId
            pprint . withHeader "unarchived:" $ UI.noteView nv
        CmdServe -> cmdServe

-- Template taken from stack:
-- "Version 1.7.1, Git revision 681c800873816c022739ca7ed14755e8 (5807 commits)"
runCmdVersion :: IO ()
runCmdVersion = putStrLn $ concat
    [ "Version ", showVersion version
    , ", Git revision ", $(gitHash)
    , if $(gitDirty) then ", dirty" else ""
    ]

pprint :: (Pretty a, MonadIO io) => a -> io ()
pprint a = liftIO $ do
    width <- maybe 80 Terminal.width <$> Terminal.size
    printOrPage . toStrict . (`Text.snoc` '\n') . prettyLazyText width $ ppr a
