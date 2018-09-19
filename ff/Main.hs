{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Monad (forever, guard)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either.Extra (fromEither)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Data.Proxy (Proxy (..))
import           Data.Text.IO (hPutStrLn)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as Text
import           Data.Time (Day)
import           RON.Storage.IO (Storage, runStorage)
import qualified RON.Storage.IO as Storage
import qualified System.Console.Terminal.Size as Terminal
import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   getHomeDirectory)
import           System.Exit (exitFailure)
import           System.FilePath (FilePath, normalise, splitDirectories, (</>))
import           System.IO (hPutChar, hPutStr, stderr)
import           System.Pager (printOrPage)
import           Text.PrettyPrint.Mainland (prettyLazyText)
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF (cmdDeleteContact, cmdDeleteNote, cmdDone, cmdEdit,
                     cmdNewContact, cmdNewNote, cmdPostpone, cmdSearch,
                     cmdUnarchive, getContactSamples, getNoteSamples,
                     getUtcToday, getWikiSamples, updateTrackedNotes)
import           FF.Config (Config (..), ConfigUI (..), appName, loadConfig,
                            printConfig, saveConfig)
import           FF.Github (getIssueViews, getOpenIssueSamples)
import           FF.Options (Cmd (..), CmdAction (..), Contact (..),
                             DataDir (..), Options (..), Search (..),
                             Shuffle (..), Track (..), parseOptions)
import qualified FF.Options as Options
import           FF.Serve (cmdServe)
import           FF.Types (EntityF (..))
import           FF.UI (sampleFmap, withHeader)
import qualified FF.UI as UI
import           FF.Upgrade (upgradeDatabase)

import           Data.Version (showVersion)
import           Development.GitRev (gitDirty, gitHash)
import           Paths_ff (version)

main :: IO ()
main = do
    cfg@Config{ui} <- loadConfig
    dataDir <- getDataDir cfg
    h <- Storage.newHandle dataDir
    Options {..}      <- parseOptions h
    case optionCmd of
        CmdConfig param  -> runCmdConfig cfg param
        CmdAction action -> runStorage h $ runCmdAction h ui action optionBrief
        CmdVersion       -> runCmdVersion

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

runCmdAction :: Storage.Handle -> ConfigUI -> CmdAction -> Bool -> Storage ()
runCmdAction h ui cmd brief = do
    today <- getUtcToday
    case cmd of
        CmdAgenda mlimit -> do
            notes <- getNoteSamples ui mlimit today
            pprint $ UI.prettySamplesBySections brief notes
        CmdContact contact -> cmdContact brief contact
        CmdDelete noteId -> do
            note <- cmdDeleteNote noteId
            pprint $ withHeader "deleted:" $ UI.noteViewFull note
        CmdDone noteId -> do
            note <- cmdDone noteId
            pprint $ withHeader "archived:" $ UI.noteViewFull note
        CmdEdit edit -> do
            note <- cmdEdit edit
            pprint $ withHeader "edited:" $ UI.noteViewFull note
        CmdNew new -> do
            note <- cmdNewNote new today
            pprint $ withHeader "added:" $ UI.noteViewFull note
        CmdPostpone noteId -> do
            note <- cmdPostpone noteId
            pprint $ withHeader "postponed:" $ UI.noteViewFull note
        CmdSearch Search {..} -> do
            (notes, wiki, contacts) <- cmdSearch searchText searchLimit today
            pprint $ UI.prettyNotesWikiContacts
                brief notes wiki contacts searchTasks searchWiki searchContacts
        CmdServe -> cmdServe h ui
        CmdTrack track ->
            cmdTrack track today brief
        CmdUnarchive noteId -> do
            note <- cmdUnarchive noteId
            pprint . withHeader "unarchived:" $ UI.noteViewFull note
        CmdUpgrade -> do
            upgradeDatabase
            liftIO $ putStrLn "upgraded"
        CmdWiki mlimit -> do
            notes <- getWikiSamples ui mlimit today
            pprint $ UI.prettyWikiSamplesOmitted brief notes

cmdTrack :: Track -> Day -> Bool -> Storage ()
cmdTrack Track {..} today brief =
    if trackDryrun then liftIO $ do
        samples <- run $ getOpenIssueSamples trackAddress trackLimit today
        pprint $
            UI.prettySamplesBySections brief $
            fmap (sampleFmap $ EntityF Proxy) samples
    else do
        notes <- liftIO $ run $ getIssueViews trackAddress trackLimit
        updateTrackedNotes notes
        liftIO
            $   putStrLn
            $   show (length notes)
                ++ " issues synchronized with the local database"
  where
    run getter = do
        hPutStr stderr "fetching"
        eIssues <- fromEither <$> race
            (runExceptT getter)
            (forever $ hPutChar stderr '.' >> threadDelay 500000)
        hPutStrLn stderr ""
        case eIssues of
            Left err     -> do
                hPutStrLn stderr err
                exitFailure
            Right issues -> pure issues

cmdContact :: Bool -> Maybe Contact -> Storage ()
cmdContact brief = \case
    Just (Add name) -> do
        contact <- cmdNewContact name
        pprint $ withHeader "added:" $ UI.contactViewFull contact
    Just (Delete cid) -> do
        contact <- cmdDeleteContact cid
        pprint $ withHeader "deleted:" $ UI.contactViewFull contact
    Nothing -> do
        contacts <- getContactSamples
        pprint $ UI.prettyContactSamplesOmitted brief contacts

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
