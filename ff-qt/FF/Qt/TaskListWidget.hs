{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.TaskListWidget
  ( TaskListWidget,
    upsertTask,
  )
where

import qualified FF.Qt.TaskListModel as TaskListModel
import FF.Qt.TaskListModel (TaskListModel)
import FF.Types (Entity, Note)
import Graphics.UI.Qtah.Widgets.QTreeView (QTreeView)

data TaskListWidget = TaskListWidget {view :: QTreeView, model :: TaskListModel}

upsertTask :: TaskListWidget -> Entity Note -> IO ()
upsertTask TaskListWidget {model} = TaskListModel.upsertTask model
