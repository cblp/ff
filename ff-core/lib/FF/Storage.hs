{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FF.Storage
  ( FFStorage,
    Handle,
    newHandle,
    runFFStorage,
    subscribeForever
    )
where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import RON.Error (Error)
import RON.Event (ReplicaClock)
import RON.Storage.Backend (MonadStorage (..))
import qualified RON.Storage.FS as FS
import RON.Storage.FS (CollectionDocId)

newtype Handle = Handle FS.Handle

newHandle :: FilePath -> IO Handle
newHandle = fmap Handle . FS.newHandle

newtype FFStorage a = FFStorage (FS.Storage a)
  deriving
    (Applicative, Functor, Monad, MonadError Error, MonadIO, ReplicaClock)

runFFStorage :: Handle -> FFStorage a -> IO a
runFFStorage (Handle h) (FFStorage action) = FS.runStorage h action

instance MonadStorage FFStorage where

  getCollections = FFStorage getCollections

  getDocuments = FFStorage getDocuments

  getDocumentVersions = FFStorage . getDocumentVersions

  saveVersionContent docid version content =
    FFStorage $ saveVersionContent docid version content

  loadVersionContent docid version =
    FFStorage $ loadVersionContent docid version

  deleteVersion docid version = FFStorage $ deleteVersion docid version

  changeDocId old new = FFStorage $ changeDocId old new

subscribeForever :: Handle -> (CollectionDocId -> IO ()) -> IO ()
subscribeForever (Handle h) = FS.subscribeForever h
