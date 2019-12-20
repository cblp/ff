{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.TaskListWidget (
  ItemType (..), TaskListWidget,
  getId, getTitle, new, setDebugInfoVisible, upsertTask
) where

-- global
import           Control.Monad (void)
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time (getCurrentTime, utctDay)
import           Foreign (castPtr)
import           Foreign.Hoppy.Runtime (CppPtr, nullptr, toPtr, touchCppPtr,
                                        withCppPtr, withScopedPtr)
import           Graphics.UI.Qtah.Core.QObject (QObjectConstPtr, QObjectPtr,
                                                toQObject, toQObjectConst)
import           Graphics.UI.Qtah.Gui.QFont (QFont)
import qualified Graphics.UI.Qtah.Gui.QFont as QFont
import           Graphics.UI.Qtah.Widgets.QAbstractItemView (QAbstractItemViewConstPtr,
                                                             QAbstractItemViewPtr,
                                                             toQAbstractItemView,
                                                             toQAbstractItemViewConst)
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView
import           Graphics.UI.Qtah.Widgets.QAbstractScrollArea (QAbstractScrollAreaConstPtr,
                                                               QAbstractScrollAreaPtr,
                                                               toQAbstractScrollArea,
                                                               toQAbstractScrollAreaConst)
import           Graphics.UI.Qtah.Widgets.QFrame (QFrameConstPtr, QFramePtr,
                                                  toQFrame, toQFrameConst)
import           Graphics.UI.Qtah.Widgets.QTreeView (QTreeViewConstPtr,
                                                     QTreeViewPtr, toQTreeView,
                                                     toQTreeViewConst)
import qualified Graphics.UI.Qtah.Widgets.QTreeView as QTreeView
import           Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget,
                                                       QTreeWidgetConstPtr,
                                                       QTreeWidgetPtr,
                                                       toQTreeWidget,
                                                       toQTreeWidgetConst)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidgetItem (QTreeWidgetItem)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import           Graphics.UI.Qtah.Widgets.QWidget (QWidgetConstPtr, QWidgetPtr,
                                                   toQWidget, toQWidgetConst)
import           RON.Storage.Backend (DocId (DocId))

-- project
import           FF (fromRgaM)
import           FF.Types (Entity (..), EntityView, Note (..), TaskMode,
                           View (NoteView, note), taskMode)
import           FF.UI (sampleLabel)

data TaskListWidget = TaskListWidget
  {super :: QTreeWidget, modeItems :: IORef (Map TaskMode QTreeWidgetItem)}

instance CppPtr TaskListWidget where
  nullptr = TaskListWidget{super = nullptr, modeItems = undefined}
  withCppPtr TaskListWidget{super} proc = withCppPtr super $ proc . castPtr
  toPtr = castPtr . toPtr . super
  touchCppPtr = touchCppPtr . super

instance QObjectConstPtr TaskListWidget where
  toQObjectConst = toQObjectConst . super

instance QObjectPtr TaskListWidget where
  toQObject = toQObject . super

instance QWidgetConstPtr TaskListWidget where
  toQWidgetConst = toQWidgetConst . super

instance QWidgetPtr TaskListWidget where
  toQWidget = toQWidget . super

instance QFrameConstPtr TaskListWidget where
  toQFrameConst = toQFrameConst . super

instance QFramePtr TaskListWidget where
  toQFrame = toQFrame . super

instance QAbstractScrollAreaConstPtr TaskListWidget where
  toQAbstractScrollAreaConst = toQAbstractScrollAreaConst . super

instance QAbstractScrollAreaPtr TaskListWidget where
  toQAbstractScrollArea = toQAbstractScrollArea . super

instance QAbstractItemViewConstPtr TaskListWidget where
  toQAbstractItemViewConst = toQAbstractItemViewConst . super

instance QAbstractItemViewPtr TaskListWidget where
  toQAbstractItemView = toQAbstractItemView . super

instance QTreeViewConstPtr TaskListWidget where
  toQTreeViewConst = toQTreeViewConst . super

instance QTreeViewPtr TaskListWidget where
  toQTreeView = toQTreeView . super

instance QTreeWidgetConstPtr TaskListWidget where
  toQTreeWidgetConst = toQTreeWidgetConst . super

instance QTreeWidgetPtr TaskListWidget where
  toQTreeWidget = toQTreeWidget . super

-- | Value order in this enumeration defines the field order in the tree widget.
-- 0th column mustn't be hideable, because when 0th column is hidden,
-- the tree strcuture, alternating row color, and child indicators
-- are hidden too.
data Field = TitleField | IdField deriving (Bounded, Enum)

fieldCount :: Int
fieldCount = fromEnum (maxBound :: Field) + 1

fieldsToStrings :: (Field -> String) -> [String]
fieldsToStrings f = map f [minBound .. maxBound]

data ItemType = ModeGroup | Task

instance Enum ItemType where
  toEnum i = case i - fromEnum QTreeWidgetItem.UserType of
    0 -> ModeGroup
    1 -> Task
    _ -> error $ "toEnum @ItemType " <> show i
  fromEnum = (+ fromEnum QTreeWidgetItem.UserType) . \case
    ModeGroup -> 0
    Task      -> 1

getId, getTitle :: QTreeWidgetItem -> IO String
getId    item = QTreeWidgetItem.text item $ fromEnum IdField
getTitle item = QTreeWidgetItem.text item $ fromEnum TitleField

new :: IO TaskListWidget
new = do
  super <- QTreeWidget.new
  QAbstractItemView.setAlternatingRowColors super True
  QTreeView.setHeaderHidden                 super True
  QTreeWidget.setColumnCount                super fieldCount

  modeItems <- newIORef mempty

  let this = TaskListWidget{super, modeItems}

  setDebugInfoVisible this False

  pure this

setDebugInfoVisible :: TaskListWidget -> Bool -> IO ()
setDebugInfoVisible this =
  QTreeView.setColumnHidden this (fromEnum IdField) . not

-- Only insertion is implemeted. TODO implement update.
upsertTask :: TaskListWidget -> EntityView Note -> IO ()
upsertTask TaskListWidget{super, modeItems} Entity{entityId, entityVal} = do
  today <- utctDay <$> getCurrentTime
  let mode = taskMode today note
  mModeItem <- Map.lookup mode <$> readIORef modeItems
  modeItem <- case mModeItem of
    Just item ->
      pure item
    Nothing -> do
      item <-
        QTreeWidgetItem.newWithParentTreeAndStringsAndType
          super
          (fieldsToStrings $ \case
            IdField    -> show mode
            TitleField -> Text.unpack $ sampleLabel mode)
          (fromEnum ModeGroup)
      QTreeWidgetItem.setExpanded item True
      withScopedPtr newBoldFont $ \boldFont ->
        QTreeWidgetItem.setFont item (fromEnum TitleField) boldFont
      modifyIORef modeItems $ Map.insert mode item
      pure item
  void $
    QTreeWidgetItem.newWithParentItemAndStringsAndType
      modeItem
      (fieldsToStrings $ \case IdField -> noteId; TitleField -> title)
      (fromEnum Task)
  where
    DocId noteId = entityId
    NoteView{note} = entityVal
    Note{note_text} = note
    text = fromRgaM note_text
    title = concat $ take 1 $ lines text

newBoldFont :: IO QFont
newBoldFont = do
  font <- QFont.new
  QFont.setBold font True
  pure font
