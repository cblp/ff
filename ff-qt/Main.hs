{-# OPTIONS -Wno-unused-imports #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Monad.Extra (void, whenJust, (<=<))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (for_)
import           Data.Functor (($>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time (Day, toGregorian)
import           Data.Version (showVersion)
import           Foreign.StablePtr (StablePtr, newStablePtr)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified Language.C.Types as C
import           Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (addDependentFile)
import           RON.Storage.IO (Collection, CollectionDocId (CollectionDocId),
                                 DocId (DocId), runStorage, subscribeForever)
import qualified RON.Storage.IO as Storage

import           FF (cmdPostpone, getDataDir, getUtcToday, load,
                     loadActiveTasks)
import           FF.Config (loadConfig)
import           FF.Types (Entity (Entity), Note (Note), NoteId, TaskMode (Actual, EndSoon, EndToday, Overdue, Starting),
                           entityId, entityVal, note_end, note_start, note_text,
                           taskMode)

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
    for_ activeTasks $ \Entity{entityId, entityVal} -> do
        let DocId docid = entityId
            docidBS = stringZ docid
            Note{note_text, note_start, note_end} = entityVal
            noteTextBS = stringZ note_text
            (     fromIntegral -> startYear
                , fromIntegral -> startMonth
                , fromIntegral -> startDay
                ) =
                    toGregorian note_start
            endIsJust = isJust note_end
            (     fromIntegral -> endYear
                , fromIntegral -> endMonth
                , fromIntegral -> endDay
                ) =
                    maybe (0, 0, 0) toGregorian note_end
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

    void $ forkIO $
        subscribeForever storage $
            \(CollectionDocId _docId) -> pure () -- updateView app docId

    [Cpp.block| void {
        qApp->exec();
    }|]

-- updateView :: (HasCallStack, Collection a) => App -> DocId a -> IO ()
-- updateView mainWindow docid = case docid of
--     (cast -> Just noteId) -> updateTask mainWindow noteId
--     _ -> error $ show (typeRep docid, docid)

-- updateTask :: App -> NoteId -> IO ()
-- updateTask mainWindow noteId = do
--     note <- runStorage h $ load noteId
--     addTask   mainWindow note
--   where
--     App{storage = h} = mainWindow

stringZ :: String -> ByteString
stringZ = (`BS.snoc` 0) . Text.encodeUtf8 . Text.pack
