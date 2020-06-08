{-# LANGUAGE DuplicateRecordFields, LambdaCase, NamedFieldPuns,
             OverloadedStrings, RecordWildCards, TemplateHaskell,
             TupleSections #-}

module FF.CLI where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Monad (forever, guard, when)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable (asum, for_, toList)
import           Data.Functor (($>))
import           Data.Maybe (isNothing)
import           Data.Text (snoc)
import           Data.Text.IO (hPutStrLn)
import           Data.Text.Prettyprint.Doc (Doc, PageWidth (AvailablePerLine),
                                            defaultLayoutOptions,
                                            layoutPageWidth, layoutSmart,
                                            pretty, vsep)
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle,
                                                            renderStrict)
import           Data.Time (Day)
import           Data.Traversable (for)
import           Data.Version (Version, showVersion)
import           Development.GitRev (gitDirty, gitHash)
import           FF (cmdDeleteContact, cmdDeleteNote, cmdDone, cmdEdit,
                     cmdNewContact, cmdNewNote, cmdPostpone, cmdSearch,
                     cmdUnarchive, getContactSamples, getDataDir, getUtcToday,
                     loadAllNotes, loadAllTagTexts, noDataDirectoryMessage,
                     sponsors, updateTrackedNotes, viewNote, viewTaskSamples,
                     viewWikiSamples)
import           FF.Config (Config (..), ConfigUI (..), appName, loadConfig,
                            printConfig, saveConfig)
import           FF.Github (getIssueViews, getOpenIssueSamples)
import           FF.Options (ActionOptions (..), Agenda (..), Cmd (..),
                             CmdAction (..), Contact (..), DataDir (..),
                             Options (..), Search (..), Shuffle (..),
                             Tags (Tags), Track (..), parseOptions)
import qualified FF.Options as Options
import           FF.Types (Status (Active), entitiesToJson, loadNote)
import qualified FF.Types as Sample (Sample (items))
import           FF.UI (prettyContact, prettyContactSample, prettyNote,
                        prettyNoteList, prettyPath, prettyTagsList,
                        prettyTaskSections, prettyTasksWikisContacts,
                        prettyWikiSample, withHeader, (<//>))
import           FF.Upgrade (upgradeDatabase)
import           RON.Storage.Backend (MonadStorage)
import           RON.Storage.FS (runStorage)
import qualified RON.Storage.FS as StorageFS
import qualified System.Console.Terminal.Size as Terminal
import           System.Directory (doesDirectoryExist, getHomeDirectory)
import           System.Environment (lookupEnv, setEnv)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutChar, hPutStr, stderr)
import           System.Pager (printOrPage)

cli :: Version -> IO ()
cli version = do
  cfg@Config{ui} <- loadConfig
  dataDir <- getDataDir cfg
  handle' <- traverse StorageFS.newHandle dataDir
  Options{customDir, cmd, actionOptions} <- parseOptions handle'
  (handle, dataPath) <-
    case customDir of
      Nothing -> pure (handle', dataDir)
      Just path -> do
        h <- StorageFS.newHandle path
        pure (Just h, customDir)
  case cmd of
    CmdConfig param -> runCmdConfig cfg param
    CmdVersion -> runCmdVersion version
    CmdAction action -> case handle of
      Nothing -> fail noDataDirectoryMessage
      Just h -> runStorage h $ runCmdAction ui action actionOptions dataPath

runCmdConfig :: Config -> Maybe Options.Config -> IO ()
runCmdConfig cfg@Config{dataDir, externalEditor, ui} = \case
  Nothing -> printConfig cfg
  Just (Options.ConfigDataDir mDir) -> do
    dir <-
      case mDir of
        Nothing -> pure dataDir
        Just (DataDirJust dir) -> saveDataDir dir
        Just DataDirYandexDisk -> do
          home <- getHomeDirectory
          asum
            [ trySaveDataDir $ home </> "Yandex.Disk"
            , trySaveDataDir $ home </> "Yandex.Disk.localized"
            , fail "Cant't detect Yandex.Disk directory"
            ]
    printConfig dir
  Just (Options.ConfigExternalEditor mPath) -> do
    path <-
      case mPath of
        Nothing -> pure externalEditor
        Just path -> saveExternalEditor path
    printConfig path
  Just (Options.ConfigUI mShuffle) -> do
    ui' <-
      case mShuffle of
        Nothing -> pure ui
        Just Shuffle -> saveShuffle True
        Just Sort -> saveShuffle False
    printConfig ui'
  where
    trySaveDataDir baseDir = do
      guard =<< doesDirectoryExist baseDir
      saveDataDir $ baseDir </> "Apps" </> appName
    saveDataDir dir = saveConfig cfg {dataDir = Just dir} $> Just dir
    saveExternalEditor path =
      saveConfig cfg {externalEditor = Just path} $> Just path
    saveShuffle shuffle' = saveConfig cfg {ui = ui'} $> ui'
      where
        ui' = ConfigUI {shuffle = shuffle'}

runCmdAction ::
  (MonadIO m, MonadStorage m) =>
  ConfigUI -> CmdAction -> ActionOptions -> Maybe FilePath -> m ()
runCmdAction ui cmd ActionOptions{brief, json} path = do
  today <- getUtcToday
  case cmd of
    CmdAgenda Agenda{limit, tags, withoutTags} -> do
      notes   <- loadAllNotes
      samples <- viewTaskSamples Active ui limit today tags withoutTags notes
      if json then
        jprint $
          JSON.object
            [ "notes"    .= entitiesToJson (foldMap Sample.items samples)
            , "database" .= path
            ]
      else
        pprint $ prettyTaskSections brief tags samples <//> prettyPath path
    CmdContact contact -> cmdContact brief path contact
    CmdDelete notes ->
      for_ notes $ \noteId -> do
        note <- cmdDeleteNote noteId
        noteview <- viewNote note
        pprint $
                withHeader "Deleted:" (prettyNote brief noteview)
          <//>  prettyPath path
    CmdDone notes ->
      for_ notes $ \noteId -> do
        note <- cmdDone noteId
        noteview <- viewNote note
        pprint $
                withHeader "Archived:" (prettyNote brief noteview)
          <//>  prettyPath path
    CmdEdit edit -> do
      notes <- cmdEdit edit
      notes' <- traverse viewNote notes
      pprint $
              withHeader "Edited:" (prettyNoteList brief notes')
        <//>  prettyPath path
    CmdNew new -> do
      note <- cmdNewNote new today
      noteview <- viewNote note
      pprint $
        withHeader "Added:" (prettyNote brief noteview) <//> prettyPath path
    CmdPostpone notes ->
      for_ notes $ \noteId -> do
        note <- cmdPostpone noteId
        noteview <- viewNote note
        pprint $
                withHeader "Postponed:" (prettyNote brief noteview)
          <//>  prettyPath path
    CmdSearch Search {..} -> do
      (tasks, wikis, contacts) <-
        cmdSearch text status ui limit today tags withoutTags
      pprint $
              prettyTasksWikisContacts
                brief
                tasks
                wikis
                contacts
                inTasks
                inWikis
                inContacts
                tags
        <//>  prettyPath path
    CmdShow noteIds -> do
      notes <- for noteIds loadNote
      notes' <- traverse viewNote notes
      pprint $ prettyNoteList brief (toList notes') <//> prettyPath path
    CmdTags -> do
      allTags <- loadAllTagTexts
      pprint $ prettyTagsList allTags <//> prettyPath path
    CmdSponsors -> pprint $ withHeader "Sponsors" $ vsep $ map pretty sponsors
    CmdTrack track ->
      cmdTrack track today brief
    CmdUnarchive tasks ->
      for_ tasks $ \taskId -> do
        task <- cmdUnarchive taskId
        noteview <- viewNote task
        pprint $
                withHeader "Unarchived:" (prettyNote brief noteview)
          <//>  prettyPath path
    CmdUpgrade -> do
      upgradeDatabase
      liftIO $ putStrLn "Upgraded"
    CmdWiki mlimit -> do
      notes <- loadAllNotes
      wikis <- viewWikiSamples ui mlimit today notes
      pprint $ prettyWikiSample brief wikis <//> prettyPath path

cmdTrack :: (MonadIO m, MonadStorage m) => Track -> Day -> Bool -> m ()
cmdTrack Track {dryRun, address, limit} today brief
  | dryRun =
    liftIO $ do
      samples <- run $ getOpenIssueSamples address limit today
      pprint $ prettyTaskSections brief (Tags mempty) samples
  | otherwise =
    do
      notes <- liftIO $ run $ getIssueViews address limit
      updateTrackedNotes notes
      liftIO $ putStrLn
        $ show (length notes)
          ++ " issues synchronized with the local database"
  where
    run getter = do
      hPutStr stderr "fetching"
      eIssues <-
        fromEither
          <$> race
                (runExceptT getter)
                (forever $ hPutChar stderr '.' >> threadDelay 500000)
      hPutStrLn stderr ""
      case eIssues of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right issues -> pure issues

cmdContact ::
  (MonadIO m, MonadStorage m) => Bool -> Maybe FilePath -> Maybe Contact -> m ()
cmdContact brief path= \case
  Just (Add name) -> do
    contact <- cmdNewContact name
    pprint $
      withHeader "Added:" (prettyContact brief contact) <//> prettyPath path
  Just (Delete cid) -> do
    contact <- cmdDeleteContact cid
    pprint $
      withHeader "Deleted:" (prettyContact brief contact) <//> prettyPath path
  Nothing -> do
    contacts <- getContactSamples Active
    pprint $ prettyContactSample brief contacts <//> prettyPath path

-- | Template taken from stack:
-- "Version 1.7.1, Git revision 681c800873816c022739ca7ed14755e8 (5807 commits)"
runCmdVersion :: Version -> IO ()
runCmdVersion version =
  putStrLn
    $ concat
        [ "Version ",
          showVersion version,
          ", Git revision ",
          $(gitHash),
          if $(gitDirty) then ", dirty" else ""
        ]

pprint :: MonadIO io => Doc AnsiStyle -> io ()
pprint doc = liftIO $ do
  -- enable colors in `less`
  lessConf <- lookupEnv "LESS"
  when (isNothing lessConf) $ setEnv "LESS" "-R"
  width <- maybe 80 Terminal.width <$> Terminal.size
  let layoutOptions =
        defaultLayoutOptions {layoutPageWidth = AvailablePerLine width 1}
  printOrPage . (`snoc` '\n') . renderStrict $ layoutSmart layoutOptions doc

fromEither :: Either a a -> a
fromEither = either id id

jprint :: (ToJSON a, MonadIO io) => a -> io ()
jprint = liftIO . BSL.putStrLn . JSON.encodePretty
