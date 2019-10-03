{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, atomically, tryReadTChan)
import Data.Foldable (for_)
import Data.Version (showVersion)
import FF (getDataDir, loadTasks, noDataDirectoryMessage, toNoteView)
import FF.Config (loadConfig)
import qualified FF.Qt.TaskListWidget as TaskListWidget
import FF.Qt.TaskListWidget (TaskListWidget (TaskListWidget, view))
import FF.Types
  ( Entity,
    EntityView,
    Note,
    Status (Active),
    View (note),
    loadNote,
  )
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QSettings as QSettings
import qualified Graphics.UI.Qtah.Core.QTimer as QTimer
import qualified Graphics.UI.Qtah.Core.QVariant as QVariant
import Graphics.UI.Qtah.Event (onEvent)
import Graphics.UI.Qtah.Gui.QCloseEvent (QCloseEvent)
import Graphics.UI.Qtah.Gui.QShowEvent (QShowEvent)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow, QMainWindowPtr)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QTabWidget as QTabWidget
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Paths_ff_qt (version)
import RON.Storage.Backend (CollectionName, DocId (DocId), RawDocId)
import qualified RON.Storage.FS as Storage
import RON.Storage.FS (runStorage)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

type MainWindow = QMainWindow

data UI = UI {window :: MainWindow, agenda :: TaskListWidget}

main :: IO ()
main = do
  path <- getDataDirOrFail
  storage <- Storage.newHandle path
  changedDocs <- Storage.subscribe storage
  withApp $ \_ -> do
    setupApp
    ui@UI {window} <- setupUI
    QWidget.show window
    -- load current data to the view, asynchronously
    _ <-
      forkIO $ do
        activeTasks <-
          runStorage storage (loadTasks Active >>= traverse toNoteView)
        for_ activeTasks $ upsertTask ui . note
    -- update the view with future changes
    whenUIIsIdle $ receiveDocChanges storage ui changedDocs
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
  saveWindowSizeState window
  QWidget.setWindowTitle window "ff"
  -- agenda
  agenda@TaskListWidget {view = agendaView} <- TaskListWidget.new
  QMainWindow.setCentralWidget window =<< do
    tabs <- QTabWidget.new
    _ <- QTabWidget.addTab tabs agendaView "Agenda"
    pure tabs
  QWidget.setFocus agendaView
  --
  pure UI {window, agenda}

getDataDirOrFail :: IO FilePath
getDataDirOrFail = do
  cfg <- loadConfig
  dataDir <- getDataDir cfg
  case dataDir of
    Nothing -> fail noDataDirectoryMessage
    Just path -> pure path

receiveDocChanges
  :: Storage.Handle -> UI -> TChan (CollectionName, RawDocId) -> IO ()
receiveDocChanges storage mainWindow changes =
  atomically (tryReadTChan changes) >>= \case
    Just ("note", noteId) -> do
      note <- runStorage storage $ loadNote $ DocId noteId
      upsertTask mainWindow note
    Just (collection, _) ->
      hPutStrLn stderr $ "unknown collection " <> show collection
    Nothing -> pure ()

upsertTask :: UI -> EntityView Note -> IO ()
upsertTask UI {agenda} = TaskListWidget.upsertTask agenda

whenUIIsIdle :: IO () -> IO ()
whenUIIsIdle action = do
  t <- QTimer.new
  connect_ t QTimer.timeoutSignal action
  QTimer.start t 0

-- https://wiki.qt.io/Saving_Window_Size_State
saveWindowSizeState :: QMainWindowPtr window => window -> IO ()
saveWindowSizeState this = do
  _ <-
    onEvent this $ \(_ :: QShowEvent) -> do
      withScopedPtr QSettings.new $ \settings -> do
        _ <-
          QSettings.value settings "mainWindowGeometry"
            >>= QVariant.toByteArray
            >>= QWidget.restoreGeometry this
        _ <-
          QSettings.value settings "mainWindowState"
            >>= QVariant.toByteArray
            >>= QMainWindow.restoreState this
        pure ()
      pure False
  _ <-
    onEvent this $ \(_ :: QCloseEvent) -> do
      withScopedPtr QSettings.new $ \settings -> do
        QSettings.setValue settings "mainWindowGeometry"
          =<< QVariant.newWithByteArray
          =<< QWidget.saveGeometry this
        QSettings.setValue settings "mainWindowState"
          =<< QVariant.newWithByteArray
          =<< QMainWindow.saveState this
      pure False
  pure ()
