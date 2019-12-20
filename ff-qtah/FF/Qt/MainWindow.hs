{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Qt.MainWindow (
  MainWindow, new, printChildrenTree, upsertNote
) where

-- global
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.Foldable (fold, for_)
import           Data.Traversable (for)
import           Foreign (castPtr)
import           Foreign.Hoppy.Runtime (CppPtr, nullptr, toPtr, touchCppPtr,
                                        withCppPtr, withScopedPtr)
import           GHC.Stack (callStack, prettyCallStack)
import qualified Graphics.UI.Qtah.Core.QMetaClassInfo as QMetaClassInfo
import qualified Graphics.UI.Qtah.Core.QMetaObject as QMetaObject
import           Graphics.UI.Qtah.Core.QObject (QObjectConstPtr, QObjectPtr,
                                                toQObject, toQObjectConst)
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Core.QSettings as QSettings
import qualified Graphics.UI.Qtah.Core.QVariant as QVariant
import           Graphics.UI.Qtah.Event (onEvent)
import           Graphics.UI.Qtah.Gui.QCloseEvent (QCloseEvent)
import           Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Widgets.QAction as QAction
import           Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow,
                                                       QMainWindowPtr)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMenuBar as QMenuBar
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidgetItem (QTreeWidgetItem)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import           Graphics.UI.Qtah.Widgets.QWidget (QWidgetConstPtr, QWidgetPtr,
                                                   toQWidget, toQWidgetConst)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.IO (hPutStrLn, stderr)

-- organization
import           RON.Storage.Backend (DocId (DocId))
import qualified RON.Storage.FS as Storage

-- project
import           FF.Types (EntityView, Note)

-- package
import           FF.Qt.TaskListWidget (ItemType (ModeGroup, Task),
                                       TaskListWidget)
import qualified FF.Qt.TaskListWidget as TaskListWidget
import           FF.Qt.TaskWidget (TaskWidget)
import qualified FF.Qt.TaskWidget as TaskWidget

data MainWindow = MainWindow
  { super       :: QMainWindow
  , agendaTasks :: TaskListWidget
  , taskWidget  :: TaskWidget
  }

instance CppPtr MainWindow where
  nullptr =
    MainWindow{super = nullptr, agendaTasks = nullptr, taskWidget = nullptr}
  withCppPtr MainWindow{super} proc = withCppPtr super $ proc . castPtr
  toPtr = castPtr . toPtr . super
  touchCppPtr = touchCppPtr . super

instance QObjectConstPtr MainWindow where
  toQObjectConst = toQObjectConst . super

instance QObjectPtr MainWindow where
  toQObject = toQObject . super

instance QWidgetConstPtr MainWindow where
  toQWidgetConst = toQWidgetConst . super

instance QWidgetPtr MainWindow where
  toQWidget = toQWidget . super

new :: String -> Storage.Handle -> IO MainWindow
new progName storage = do
  super <- QMainWindow.new
  QWidget.setWindowTitle super progName

  restoreGeometry super -- must be before widgets creation

  -- UI setup and widgets creation
  agendaSplitter <- QSplitter.new
  QSplitter.setChildrenCollapsible agendaSplitter False
  QMainWindow.setCentralWidget super agendaSplitter

  agendaTasks <- TaskListWidget.new
  QSplitter.addWidget agendaSplitter agendaTasks

  taskWidget <- TaskWidget.new storage
  QWidget.hide taskWidget
  QSplitter.addWidget agendaSplitter taskWidget

  do
    menuBar <- QMainWindow.menuBar super
    do
      debugMenu <- QMenuBar.addNewMenu menuBar "&Debug"
      showUuidsAction <-
        QMenu.addNewAction debugMenu "&Show UUIDs and internal keys"
      QAction.setCheckable showUuidsAction True
      connect_ showUuidsAction QAction.toggledSignal $
        TaskListWidget.setDebugInfoVisible agendaTasks
    do
      helpMenu <- QMenuBar.addNewMenu menuBar "&Help"
      aboutProgramAction <- QMenu.addNewAction helpMenu "&About ff"
      connect_ aboutProgramAction QAction.triggeredSignal $ const $
        showAboutProgram super progName

  restoreState super -- must be after widgets creation

  let mainWindow = MainWindow{super, agendaTasks, taskWidget}

  -- handling events
  void $ onEvent super $ \(_ :: QCloseEvent) -> saveGeometryAndState super
  -- TODO
  -- connect_ editor QTextEdit.textChangedSignal $ saveTheText storage editor
  connect_ agendaTasks QTreeWidget.itemSelectionChangedSignal $
    resetTaskView mainWindow

  pure mainWindow

-- | Only task notes are supported. TODO support wiki notes too
upsertNote :: MainWindow -> EntityView Note -> IO ()
upsertNote MainWindow{agendaTasks} = TaskListWidget.upsertTask agendaTasks

-- https://wiki.qt.io/Saving_Window_Size_State
saveGeometryAndState :: QMainWindowPtr window => window -> IO Bool
saveGeometryAndState window =
  withScopedPtr QSettings.new $ \settings -> do
    let saveSetting name value =
          QVariant.newWithByteArray value >>= QSettings.setValue settings name
    QWidget.saveGeometry  window >>= saveSetting "mainWindowGeometry"
    QMainWindow.saveState window >>= saveSetting "mainWindowState"
    pure True

restoreGeometry :: QWidgetPtr widget => widget -> IO ()
restoreGeometry widget =
  void $ loadSetting "mainWindowGeometry" >>= QWidget.restoreGeometry widget

restoreState :: QMainWindowPtr window => window -> IO ()
restoreState window =
  void $ loadSetting "mainWindowState" >>= QMainWindow.restoreState window

loadSetting :: String -> IO ByteString
loadSetting name =
  withScopedPtr QSettings.new $ \settings ->
    QSettings.value settings name >>= QVariant.toByteArray

resetTaskView :: MainWindow -> IO ()
resetTaskView MainWindow{agendaTasks, taskWidget} = do
  items <- QTreeWidget.selectedItems agendaTasks
  taskItems <- fmap fold . for items $ \item -> do
    itemType <- toEnum <$> QTreeWidgetItem.getType item
    pure $ case itemType of
      Task      -> [item]
      ModeGroup -> []
  case taskItems of
    []     -> QWidget.hide taskWidget
    [item] -> setTaskView taskWidget item
    _:_:_  -> print "TODO open/replace group actions view"

setTaskView :: TaskWidget -> QTreeWidgetItem -> IO ()
setTaskView taskWidget item = do
  itemType <- toEnum <$> QTreeWidgetItem.getType item
  case itemType of
    ModeGroup ->
      hPutStrLn stderr $ "internal error" ++ prettyCallStack callStack
    Task -> do
      noteId <- DocId @Note <$> TaskListWidget.getId item
      TaskWidget.update taskWidget noteId
      QWidget.show taskWidget

showAboutProgram :: QWidgetPtr mainWindow => mainWindow -> String -> IO ()
showAboutProgram mainWindow progName =
  QMessageBox.about mainWindow progName "A note taker and task tracker"

printChildrenTree :: QObjectPtr object => object -> IO ()
printChildrenTree = go 0 . toQObject where
  go level object = do
    name <- QObject.objectName object
    meta <- QObject.metaObject object
    classInfo <- QMetaObject.classInfo meta 0
    className <- QMetaClassInfo.name classInfo
    putStrLn $
      unwords $ replicate level "| " ++ [show name, ":", show className]
    children <- QObject.children object
    for_ children $ go (level + 1)
