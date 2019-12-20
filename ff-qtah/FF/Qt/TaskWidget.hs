{-# LANGUAGE NamedFieldPuns #-}

module FF.Qt.TaskWidget (
  TaskWidget, new, update
) where

-- global
import           Foreign (castPtr)
import           Foreign.Hoppy.Runtime (CppPtr, nullptr, toPtr, touchCppPtr,
                                        withCppPtr)
import           Graphics.UI.Qtah.Core.QObject (QObjectConstPtr, QObjectPtr,
                                                toQObject, toQObjectConst)
import           Graphics.UI.Qtah.Core.Types (QtAlignmentFlag (AlignTop))
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QFrame as QFrame
import           Graphics.UI.Qtah.Widgets.QLabel (QLabel)
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import           Graphics.UI.Qtah.Widgets.QScrollArea (QScrollArea)
import qualified Graphics.UI.Qtah.Widgets.QScrollArea as QScrollArea
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import           Graphics.UI.Qtah.Widgets.QWidget (QWidgetConstPtr, QWidgetPtr,
                                                   toQWidget, toQWidgetConst)
import           RON.Storage.FS (runStorage)
import qualified RON.Storage.FS as Storage

-- project
import           FF (fromRgaM, viewNote)
import           FF.Types (Entity (..), Note (Note, note_text), NoteId,
                           View (NoteView, note), loadNote)

-- package
import qualified FF.Qt.DateComponent as DateComponent

data TaskWidget =
  TaskWidget{super :: QScrollArea, label :: QLabel, storage :: Storage.Handle}

instance CppPtr TaskWidget where
  nullptr = TaskWidget{super = nullptr, label = nullptr, storage = undefined}
  withCppPtr TaskWidget{super} proc = withCppPtr super $ proc . castPtr
  toPtr = castPtr . toPtr . super
  touchCppPtr = touchCppPtr . super

instance QObjectConstPtr TaskWidget where
  toQObjectConst = toQObjectConst . super

instance QObjectPtr TaskWidget where
  toQObject = toQObject . super

instance QWidgetConstPtr TaskWidget where
  toQWidgetConst = toQWidgetConst . super

instance QWidgetPtr TaskWidget where
  toQWidget = toQWidget . super

new :: Storage.Handle -> IO TaskWidget
new storage = do
  super <- QScrollArea.new
  QFrame.setFrameShadow super QFrame.Sunken
  QFrame.setFrameShape  super QFrame.Box
  QFrame.setLineWidth   super 10

  frame <- QFrame.new
  QFrame.setFrameShadow frame QFrame.Sunken
  QFrame.setFrameShape  frame QFrame.Box
  QFrame.setLineWidth   frame 10
  QScrollArea.setWidget super frame

  label <- QLabel.new
  QLabel.setAlignment label AlignTop
  QLabel.setWordWrap  label True

  start <- DateComponent.new "Start:"
  end   <- DateComponent.new "Deadline:"

  box <- QVBoxLayout.newWithParent frame
  QBoxLayout.addWidget box label
  QBoxLayout.addLayout box start
  QBoxLayout.addLayout box end

  pure TaskWidget{super, label, storage}

update :: TaskWidget -> NoteId -> IO ()
update TaskWidget{label, storage} noteId = do
  Entity{entityVal} <- runStorage storage $ loadNote noteId >>= viewNote
  let NoteView{note} = entityVal
  let Note{note_text} = note
  QLabel.setText label $ fromRgaM note_text
