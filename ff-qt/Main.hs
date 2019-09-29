{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO)
import Data.Foldable (for_)
import Data.Typeable (cast)
import Data.Version (showVersion)
import FF (getDataDir, loadTasks, noDataDirectoryMessage)
import FF.Config (loadConfig)
import qualified FF.Qt.TaskListWidget as TaskListWidget
import FF.Qt.TaskListWidget (TaskListWidget)
import FF.Types
  ( Entity,
    Note,
    NoteId,
    NoteView (note),
    loadNote,
  )
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QTabWidget as QTabWidget
import qualified Graphics.UI.Qtah.Widgets.QTreeView as QTreeView
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Paths_ff_qt (version)
import qualified RON.Storage.FS as Storage
import RON.Storage.FS
  ( CollectionDocId (CollectionDocId),
    runStorage,
    subscribeForever,
  )
import System.Environment (getArgs)

type MainWindow = QMainWindow

data UI = UI {window :: MainWindow, agenda :: TaskListWidget}

main :: IO ()
main = do
  path <- getDataDirOrFail
  storage <- Storage.newHandle path
  withApp $ \_ -> do
    setupApp
    ui@UI {window} <- setupUI
    QWidget.show window
    -- load current data to the view, asynchronously
    _ <-
      forkIO $ do
        activeTasks <- runStorage storage (loadTasks False)
        for_ activeTasks $ upsertTask ui . note
    -- update the view with future changes
    _ <- forkIO $ subscribeForever storage $ upsertDocument storage ui
    -- run UI
    QCoreApplication.exec

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ do
  args <- getArgs
  QApplication.new args

setupApp :: IO ()
setupApp = do
  QCoreApplication.setOrganizationDomain "ff.cblp.su"
  QCoreApplication.setOrganizationName "ff"
  QCoreApplication.setApplicationName "ff"
  QCoreApplication.setApplicationVersion $ showVersion version

setupUI :: IO UI
setupUI = do
  -- window
  window <- QMainWindow.new
  QWidget.setWindowTitle window "ff"
  -- agenda
  agenda <- QTreeView.new
  QMainWindow.setCentralWidget window =<< do
    tabs <- QTabWidget.new
    _ <- QTabWidget.addTab tabs agenda "Agenda"
    pure tabs
  QWidget.setFocus agenda
  --
  pure UI {window, agenda}

getDataDirOrFail :: IO FilePath
getDataDirOrFail = do
  cfg <- loadConfig
  dataDir <- getDataDir cfg
  case dataDir of
    Nothing -> fail noDataDirectoryMessage
    Just path -> pure path

upsertDocument :: Storage.Handle -> UI -> CollectionDocId -> IO ()
upsertDocument storage mainWindow (CollectionDocId docid) = case docid of
  (cast -> Just (noteId :: NoteId)) -> do
    note <- runStorage storage $ loadNote noteId
    upsertTask mainWindow note
  _ -> pure ()

upsertTask :: UI -> Entity Note -> IO ()
upsertTask UI {agenda} = Agenda.upsertTask agenda
