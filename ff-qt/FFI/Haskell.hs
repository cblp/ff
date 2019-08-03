{-# LANGUAGE NamedFieldPuns #-}

module FFI.Haskell where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time (fromGregorian)
import FF (cmdDone, cmdEdit, cmdPostpone)
import FF.Options (Edit (Edit, end, ids, start, text), MaybeClear (Clear, Set))
import Foreign.C (CInt (CInt), CString, peekCAString)
import Foreign.StablePtr (StablePtr, deRefStablePtr)
import RON.Storage.Backend (DocId (DocId))
import RON.Storage.FS (runStorage)
import qualified RON.Storage.FS as StorageFS

{-# ANN module "HLint: ignore Use camelCase" #-}

foreign export ccall
  c_assignStart
    :: StablePtr StorageFS.Handle
    -> CString
    -> CInt
    -> CInt
    -> CInt
    -> IO ()

c_assignStart
  :: StablePtr StorageFS.Handle -> CString -> CInt -> CInt -> CInt -> IO ()
c_assignStart storagePtr noteIdStr year month day = do
  storageHandle <- deRefStablePtr storagePtr
  noteId <- peekCAString noteIdStr
  let start =
        Just
          $ fromGregorian
              (fromIntegral year)
              (fromIntegral month)
              (fromIntegral day)
  void $ runStorage storageHandle
    $ cmdEdit
        Edit {ids = DocId noteId :| [], text = Nothing, start, end = Nothing}

foreign export ccall
  c_assignEnd :: StablePtr StorageFS.Handle -> CString -> CInt -> CInt -> CInt -> IO ()

c_assignEnd
  :: StablePtr StorageFS.Handle -> CString -> CInt -> CInt -> CInt -> IO ()
c_assignEnd storagePtr noteIdStr year month day = do
  storageHandle <- deRefStablePtr storagePtr
  noteId <- peekCAString noteIdStr
  let end = Just $ case (year, month, day) of
        (0, 0, 0) -> Clear
        _ ->
          Set
            $ fromGregorian
                (fromIntegral year)
                (fromIntegral month)
                (fromIntegral day)
  void $ runStorage storageHandle
    $ cmdEdit
        Edit {ids = DocId noteId :| [], text = Nothing, end, start = Nothing}

foreign export ccall c_done :: StablePtr StorageFS.Handle -> CString -> IO ()

c_done :: StablePtr StorageFS.Handle -> CString -> IO ()
c_done storagePtr noteIdStr = do
  storageHandle <- deRefStablePtr storagePtr
  noteId <- peekCAString noteIdStr
  void $ runStorage storageHandle $ cmdDone $ DocId noteId

foreign export ccall
  c_postpone :: StablePtr StorageFS.Handle -> CString -> IO ()

c_postpone :: StablePtr StorageFS.Handle -> CString -> IO ()
c_postpone storagePtr noteIdStr = do
  storageHandle <- deRefStablePtr storagePtr
  noteId <- peekCAString noteIdStr
  void $ runStorage storageHandle $ cmdPostpone $ DocId noteId
