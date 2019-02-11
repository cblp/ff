{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Prelude hiding (id)

import           Control.Concurrent (forkIO)
import           Control.Monad.Extra (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.Functor (($>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time (Day, toGregorian)
import           Data.Typeable (cast)
import           Data.Version (showVersion)
import           Foreign (Ptr)
import           Foreign.C (CInt, CString, peekCAString)
import           Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified Language.C.Types as C
import           Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (addDependentFile)
import           RON.Storage.IO (CollectionDocId (CollectionDocId),
                                 DocId (DocId), runStorage, subscribeForever)
import qualified RON.Storage.IO as Storage

import           FF (cmdPostpone, getDataDir, load, loadActiveTasks)
import           FF.Config (loadConfig)
import           FF.Types (Entity (Entity), Note (Note), NoteId, entityId,
                           entityVal, note_end, note_start, note_text)

import           Paths_ff_qt (version)

data MainWindow

$(let
    myCtx = mempty
        { C.ctxTypesTable =
            Map.fromList
                [ (C.TypeName "bool", TH.conT ''Bool)
                , (C.TypeName "MainWindow", TH.conT ''MainWindow)
                , (C.TypeName "StorageHandle", [t| StablePtr Storage.Handle |])
                ]
        }
    in Cpp.context $ Cpp.cppCtx <> Cpp.bsCtx <> myCtx)
Cpp.include "<experimental/optional>"
Cpp.using   "std::experimental::optional"
Cpp.include "<QtWidgets>"
Cpp.include "MainWindow.hpp"; addDependentFile "MainWindow.hpp" $> []

main :: IO ()
main = do
    cfg     <- loadConfig
    dataDir <- getDataDir cfg
    storage <- Storage.newHandle dataDir

    let versionBS = stringZ $ showVersion version
    storagePtr <- newStablePtr storage

    -- TODO(2019-02-10, cblp) minimize inline C/C++ code with cxx-sources
    mainWindow <- [Cpp.block| MainWindow * {
        int argc = 0;
        char * argv[] = {const_cast<char*>("ff-qt"), NULL};

        auto app = new QApplication(argc, argv);
        app->setOrganizationDomain("ff.cblp.su");
        app->setOrganizationName("ff");
        app->setApplicationName("ff");
        app->setApplicationVersion($bs-ptr:versionBS);

        auto window = new MainWindow($(StorageHandle storagePtr));
        window->show();
        return window;
    }|]

    activeTasks <- runStorage storage loadActiveTasks
    for_ activeTasks $ addTask mainWindow

    void $ forkIO $
        subscribeForever storage $
            \(CollectionDocId docid) -> case docid of
                (cast -> Just (noteId :: NoteId)) -> do
                    note <- runStorage storage $ load noteId
                    addTask mainWindow note
                _ -> pure ()

    [Cpp.block| void {
        qApp->exec();
    }|]

addTask :: Ptr MainWindow -> Entity Note -> IO ()
addTask mainWindow Entity{entityId = DocId id, entityVal = note} = do
    let docidBS = stringZ id
        Note{note_text, note_start, note_end} = note
        noteTextBS = stringZ note_text
        (startYear, startMonth, startDay) = toGregorianC note_start
        endIsJust = isJust note_end
        (endYear, endMonth, endDay) = maybe (0, 0, 0) toGregorianC note_end
    [Cpp.block| void {
        Note note = {
            .id = NoteId{$bs-ptr:docidBS},
            .text = $bs-ptr:noteTextBS,
            .start =
                QDate($(int startYear), $(int startMonth), $(int startDay)),
            .end =
                $(bool endIsJust)
                    ? QDate($(int endYear), $(int endMonth), $(int endDay))
                    : optional<QDate>()
        };
        $(MainWindow * mainWindow)->addTask(note);
    }|]

toGregorianC :: Day -> (CInt, CInt, CInt)
toGregorianC day = (y, m, d) where
    (fromIntegral -> y, fromIntegral -> m, fromIntegral -> d) = toGregorian day

stringZ :: String -> ByteString
stringZ = (`BS.snoc` 0) . Text.encodeUtf8 . Text.pack

foreign export ccall ff_postpone :: StablePtr Storage.Handle -> CString -> IO ()
ff_postpone :: StablePtr Storage.Handle -> CString -> IO ()
ff_postpone storagePtr noteIdStr = do
    storageHandle <- deRefStablePtr storagePtr
    noteId <- peekCAString noteIdStr
    void $ runStorage storageHandle $ cmdPostpone $ DocId noteId
{-# ANN ff_postpone ("HLint: ignore Use camelCase" :: String) #-}
