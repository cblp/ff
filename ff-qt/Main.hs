{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Prelude hiding (id)

import           Control.Concurrent (forkIO)
import           Data.Foldable (for_)
import           Data.Maybe (isJust)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (Day, toGregorian)
import           Data.Typeable (cast)
import           Data.Version (showVersion)
import           Foreign (Ptr)
import           Foreign.C (CInt)
import           Foreign.StablePtr (newStablePtr)
import qualified Language.C.Inline.Cpp as Cpp
import           RON.Storage.IO (CollectionDocId (CollectionDocId),
                                 DocId (DocId), runStorage, subscribeForever)
import qualified RON.Storage.IO as Storage

import           FF (getDataDir, load, loadTasks)
import           FF.Config (loadConfig)
import           FF.Types (Entity (Entity), Note (Note), NoteId,
                           NoteStatus (TaskStatus), Status (Active), entityId,
                           entityVal, note_end, note_start, note_status,
                           note_text, note_track, track_externalId,
                           track_provider, track_source, track_url)

import           Cpp (MainWindow, ffCtx, includeDependent)
import           Paths_ff_qt (version)

Cpp.context $ Cpp.cppCtx <> Cpp.bsCtx <> ffCtx
includeDependent "FFI/Cxx.hxx"
includeDependent "MainWindow.hxx"

main :: IO ()
main = do
    cfg     <- loadConfig
    dataDir <- getDataDir cfg
    storage <- Storage.newHandle dataDir

    let version' = encodeUtf8 . Text.pack $ showVersion version
    storagePtr <- newStablePtr storage

    -- set up UI
    mainWindow <- [Cpp.block| MainWindow * {
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
    _ <- forkIO $ do
        activeTasks <- runStorage storage (loadTasks False)
        for_ activeTasks $ upsertTask mainWindow

    -- update the view with future changes
    _ <- forkIO $ subscribeForever storage $ upsertDocument storage mainWindow

    -- run UI
    [Cpp.block| void { qApp->exec(); } |]

upsertDocument :: Storage.Handle -> Ptr MainWindow -> CollectionDocId -> IO ()
upsertDocument storage mainWindow (CollectionDocId docid) = case docid of
    (cast -> Just (noteId :: NoteId)) -> do
        note <- runStorage storage $ load noteId
        upsertTask mainWindow note
    _ -> pure ()

upsertTask :: Ptr MainWindow -> Entity Note -> IO ()
upsertTask mainWindow Entity{entityId = DocId id, entityVal = note} = do
    let id' = encodeUtf8 $ Text.pack id
        isActive = note_status == TaskStatus Active
        Note{note_text, note_start, note_end, note_track, note_status} = note
        text = encodeUtf8 $ Text.pack note_text
        (startYear, startMonth, startDay) = toGregorianC note_start
        (endYear, endMonth, endDay) = maybe (0, 0, 0) toGregorianC note_end
        isTracking = isJust note_track
        provider   = encodeUtf8 $ maybe "" track_provider   note_track
        source     = encodeUtf8 $ maybe "" track_source     note_track
        externalId = encodeUtf8 $ maybe "" track_externalId note_track
        url        = encodeUtf8 $ maybe "" track_url        note_track
    [Cpp.block| void {
        $(MainWindow * mainWindow)->upsertTask({
            .id = $bs-cstr:id',
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
toGregorianC day = (y, m, d) where
    (fromIntegral -> y, fromIntegral -> m, fromIntegral -> d) = toGregorian day
