{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad (forever)
import Cpp (MainWindow, ffCtx, includeDependent)
import Data.Foldable (for_)
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, toGregorian)
import Data.Version (showVersion)
import FF
  ( filterTasksByStatus,
    fromRgaM,
    getDataDir,
    loadAllNotes,
    noDataDirectoryMessage,
    viewNote,
  )
import FF.Config (loadConfig)
import FF.Types
  ( Entity (Entity, entityId, entityVal),
    EntityView,
    Note (Note, note_end, note_start, note_status, note_text, note_track),
    NoteStatus (TaskStatus),
    Status (Active),
    Track (track_externalId, track_provider, track_source, track_url),
    View (NoteView, note),
    loadNote,
  )
import Foreign (Ptr)
import Foreign.C (CInt)
import Foreign.StablePtr (newStablePtr)
import qualified Language.C.Inline.Cpp as Cpp
import Paths_ff_qt (version)
import RON.Storage.Backend
  ( CollectionName,
    DocId (DocId),
    RawDocId,
    collectionName,
  )
import qualified RON.Storage.FS as Storage
import RON.Storage.FS (runStorage, subscribe)

Cpp.context $ Cpp.cppCtx <> Cpp.bsCtx <> ffCtx

Cpp.include "<QtWidgets/QApplication>"

includeDependent "FFI/Cxx.hxx"

includeDependent "MainWindow.hxx"

main :: IO ()
main = do
  let version' = encodeUtf8 . Text.pack $ showVersion version
  path <- getDataDirOrFail
  storage <- Storage.newHandle path
  storagePtr <- newStablePtr storage
  -- set up UI
  mainWindow <-
    [Cpp.block| MainWindow * {
      int argc = 0;
      char argv0[] = "ff-qt";
      char * argv[] = {argv0, NULL};

      auto app = new QApplication(argc, argv);
      app->setOrganizationDomain("ff.cblp.su");
      app->setOrganizationName("ff");
      app->setApplicationName("ff");
      app->setApplicationVersion(QString::fromStdString($bs-cstr:version'));

      auto window = new MainWindow($(StorageHandle storagePtr));
      window->show();
      return window;
    } |]
  -- load current data to the view, asynchronously
  _ <-
    forkIO $ do
      activeTasks <-
        runStorage storage $ do
          notes <- loadAllNotes
          let filtered = filterTasksByStatus Active notes
          traverse viewNote filtered
      for_ activeTasks $ upsertTask mainWindow
  -- update the view with future changes
  _ <-
    forkIO $ do
      changes <- subscribe storage
      forever
        $ atomically (tryReadTChan changes) >>= \case
          Nothing -> pure ()
          Just (collection, docid) ->
            upsertDocument storage mainWindow collection docid
  -- run UI
  [Cpp.block| void { qApp->exec(); } |]

getDataDirOrFail :: IO FilePath
getDataDirOrFail = do
  cfg <- loadConfig
  dataDir <- getDataDir cfg
  case dataDir of
    Nothing -> fail noDataDirectoryMessage
    Just path -> pure path

upsertDocument
  :: Storage.Handle -> Ptr MainWindow -> CollectionName -> RawDocId -> IO ()
upsertDocument storage mainWindow collection docid
  | collection == collectionName @Note = do
    note <- runStorage storage $ loadNote (DocId docid) >>= viewNote
    upsertTask mainWindow note
  | otherwise = pure ()

upsertTask :: Ptr MainWindow -> EntityView Note -> IO ()
upsertTask mainWindow Entity {entityId = DocId nid, entityVal = noteView} = do
  let nid' = encodeUtf8 $ Text.pack nid
      Note {note_text, note_start, note_end, note_track, note_status} = note
      NoteView {note} = noteView
      isActive = note_status == Just (TaskStatus Active)
      noteText = fromRgaM note_text
      text = encodeUtf8 $ Text.pack noteText
      (startYear, startMonth, startDay) = toGregorianC $ fromJust note_start
      (endYear, endMonth, endDay) = maybe (0, 0, 0) toGregorianC note_end
      isTracking = isJust note_track
      provider = encodeUtf8 $ fromMaybe "" $ note_track >>= track_provider
      source = encodeUtf8 $ fromMaybe "" $ note_track >>= track_source
      externalId = encodeUtf8 $ fromMaybe "" $ note_track >>= track_externalId
      url = encodeUtf8 $ fromMaybe "" $ note_track >>= track_url
  [Cpp.block| void {
    $(MainWindow * mainWindow)->upsertTask({
      .id = $bs-cstr:nid',
      .isActive = $(bool isActive),
      .text = $bs-cstr:text,
      .start = {$(int startYear), $(int startMonth), $(int startDay)},
      .end   = {$(int   endYear), $(int   endMonth), $(int   endDay)},
      .isTracking = $(bool isTracking),
      .track = {
        .provider   = $bs-cstr:provider,
        .source     = $bs-cstr:source,
        .externalId = $bs-cstr:externalId,
        .url        = $bs-cstr:url,
      },
    });
  } |]

toGregorianC :: Day -> (CInt, CInt, CInt)
toGregorianC day = (y, m, d)
  where
    (fromIntegral -> y, fromIntegral -> m, fromIntegral -> d) = toGregorian day
