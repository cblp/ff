{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.TaskListModel
  ( TaskListModel,
    newWithView,
    upsertTask,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import FF.Types
  ( Entity (Entity, entityId, entityVal),
    Note (Note, note_status),
    NoteId,
    NoteStatus (TaskStatus),
    Status (Active),
  )
import Graphics.UI.Qtah.Gui.QStandardItem (QStandardItem)
import qualified Graphics.UI.Qtah.Gui.QStandardItem as QStandardItem
import Graphics.UI.Qtah.Gui.QStandardItemModel (QStandardItemModel)
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import Graphics.UI.Qtah.Widgets.QAbstractItemView (QAbstractItemViewPtr)
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView

data TaskListModel
  = TaskListModel
      { model :: QStandardItemModel,
        itemIndex :: IORef (HashMap NoteId QStandardItem)
      }

newWithView :: QAbstractItemViewPtr view => view -> IO TaskListModel
newWithView view = do
  model <- QStandardItemModel.new
  QAbstractItemView.setModel view model
  itemIndex <- newIORef mempty
  pure TaskListModel {model, itemIndex}

upsertTask :: TaskListModel -> Entity Note -> IO ()
upsertTask this@TaskListModel {itemIndex} task@Entity {entityId, entityVal} = do
  mItem <- HashMap.lookup entityId <$> readIORef itemIndex
  case mItem of
    Nothing
      | taskIsActive -> insert this task
      | otherwise -> pure ()
    Just item
      | taskIsActive -> update item task
      | otherwise -> remove item
  where
    Note {note_status} = entityVal
    taskIsActive = note_status == Just (TaskStatus Active)

insert :: TaskListModel -> Entity Note -> IO ()
insert TaskListModel {model, itemIndex} Entity {entityId} = do
  item <- QStandardItem.new
  QStandardItemModel.appendRowItem model item
  modifyIORef' itemIndex $ HashMap.insert entityId item

update :: QStandardItem -> Entity Note -> IO ()
update = undefined

remove :: QStandardItem -> IO ()
remove = undefined
