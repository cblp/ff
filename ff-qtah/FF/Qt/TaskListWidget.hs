{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.TaskListWidget
  ( TaskListWidget (..),
    new,
    upsertTask,
  )
where

import qualified FF.Qt.TaskListModel as TaskListModel
import FF.Qt.TaskListModel (TaskListModel)
import FF.Types (EntityView, Note)
import Graphics.UI.Qtah.Widgets.QTreeView (QTreeView)
import qualified Graphics.UI.Qtah.Widgets.QTreeView as QTreeView

data TaskListWidget = TaskListWidget {view :: QTreeView, model :: TaskListModel}

new :: IO TaskListWidget
new = do
  view <- QTreeView.new
  model <- TaskListModel.newWithView view
  pure TaskListWidget {view, model}

upsertTask :: TaskListWidget -> EntityView Note -> IO ()
upsertTask TaskListWidget {model} = TaskListModel.upsertTask model
