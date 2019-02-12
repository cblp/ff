{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Prelude hiding (id)

import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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

import           FF (getDataDir, load, loadActiveTasks)
import           FF.Config (loadConfig)
import           FF.Types (Entity (Entity), Note (Note), NoteId, entityId,
                           entityVal, note_end, note_start, note_text)

import           Cpp (MainWindow, ffCtx, includeDependent)
import           Paths_ff_qt (version)

Cpp.context $ Cpp.cppCtx <> Cpp.bsCtx <> ffCtx
includeDependent "proxy.hpp"

main :: IO ()
main = do
    cfg     <- loadConfig
    dataDir <- getDataDir cfg
    storage <- Storage.newHandle dataDir

    let version' = stringZ $ showVersion version
    storagePtr <- newStablePtr storage

    mainWindow <- [Cpp.exp| MainWindow * {
        proxy_main($bs-ptr:version', $(StorageHandle storagePtr))
    }|]

    activeTasks <- runStorage storage loadActiveTasks
    for_ activeTasks $ upsertTask mainWindow

    void $ forkIO $
        subscribeForever storage $
            \(CollectionDocId docid) -> case docid of
                (cast -> Just (noteId :: NoteId)) -> do
                    note <- runStorage storage $ load noteId
                    upsertTask mainWindow note
                _ -> pure ()

    [Cpp.exp| void { qApp_exec() }|]

upsertTask :: Ptr MainWindow -> Entity Note -> IO ()
upsertTask mainWindow Entity{entityId = DocId id, entityVal = note} = do
    let docidBS = stringZ id
        Note{note_text, note_start, note_end} = note
        noteTextBS = stringZ note_text
        (startYear, startMonth, startDay) = toGregorianC note_start
        (endYear, endMonth, endDay) = maybe (0, 0, 0) toGregorianC note_end
    [Cpp.block| void {
        MainWindow_upsertTask(
            $(MainWindow * mainWindow),
            (Note){
                .id = (NoteId){$bs-ptr:docidBS},
                .text = $bs-ptr:noteTextBS,
                .start = (Date){
                    $(int startYear), $(int startMonth), $(int startDay)
                },
                .end = (Date){$(int endYear), $(int endMonth), $(int endDay)},
            }
        );
    }|]

toGregorianC :: Day -> (CInt, CInt, CInt)
toGregorianC day = (y, m, d) where
    (fromIntegral -> y, fromIntegral -> m, fromIntegral -> d) = toGregorian day

stringZ :: String -> ByteString
stringZ = (`BS.snoc` 0) . Text.encodeUtf8 . Text.pack
