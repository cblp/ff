{-# LANGUAGE NamedFieldPuns #-}

module FFI.Haskell where

import           Control.Monad (void)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import           Data.Time (fromGregorian)
import           Foreign.C (CInt (CInt), CString, peekCAString)
import           Foreign.StablePtr (StablePtr, deRefStablePtr)
import           RON.Storage.Backend (DocId (DocId))
import           RON.Storage.FS (runStorage)
import qualified RON.Storage.FS as StorageFS

import           FF (cmdDone, cmdEdit, cmdPostpone)
import           FF.Options (Assign (Clear, Set),
                             Edit (Edit, end, ids, start, text, addTags, deleteTags))

{-# ANN module "HLint: ignore Use camelCase" #-}

foreign export ccall hs_assignStart
    :: StablePtr StorageFS.Handle -> CString -> CInt -> CInt -> CInt -> IO ()
hs_assignStart
    :: StablePtr StorageFS.Handle -> CString -> CInt -> CInt -> CInt -> IO ()
hs_assignStart storagePtr noteIdStr year month day = do
    storageHandle <- deRefStablePtr storagePtr
    noteId <- peekCAString noteIdStr
    let start =
            Just $
            fromGregorian
                (fromIntegral year) (fromIntegral month) (fromIntegral day)
    void $ runStorage storageHandle $
        cmdEdit Edit
            { ids = DocId noteId :| []
            , text = Nothing
            , start
            , end = Nothing
            , addTags = Set.empty
            , deleteTags = Set.empty
            }

foreign export ccall hs_assignEnd
    :: StablePtr StorageFS.Handle -> CString -> CInt -> CInt -> CInt -> IO ()
hs_assignEnd
    :: StablePtr StorageFS.Handle -> CString -> CInt -> CInt -> CInt -> IO ()
hs_assignEnd storagePtr noteIdStr year month day = do
    storageHandle <- deRefStablePtr storagePtr
    noteId <- peekCAString noteIdStr
    let end = Just $ case (year, month, day) of
            (0, 0, 0) -> Clear
            _         ->
                Set $
                fromGregorian
                    (fromIntegral year) (fromIntegral month) (fromIntegral day)
    void $ runStorage storageHandle $
        cmdEdit Edit
            { ids = DocId noteId :| []
            , text = Nothing
            , end
            , start = Nothing
            , addTags = Set.empty
            , deleteTags = Set.empty
            }

foreign export ccall hs_done :: StablePtr StorageFS.Handle -> CString -> IO ()
hs_done :: StablePtr StorageFS.Handle -> CString -> IO ()
hs_done storagePtr noteIdStr = do
    storageHandle <- deRefStablePtr storagePtr
    noteId <- peekCAString noteIdStr
    void $ runStorage storageHandle $ cmdDone $ DocId noteId

foreign export ccall hs_postpone
    :: StablePtr StorageFS.Handle -> CString -> IO ()
hs_postpone :: StablePtr StorageFS.Handle -> CString -> IO ()
hs_postpone storagePtr noteIdStr = do
    storageHandle <- deRefStablePtr storagePtr
    noteId <- peekCAString noteIdStr
    void $ runStorage storageHandle $ cmdPostpone $ DocId noteId
