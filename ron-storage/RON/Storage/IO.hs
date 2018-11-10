{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Storage.IO (
    module X,
    Handle,
    Storage,
    newHandle,
    runStorage,
    runStorageT,
) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (filterM, unless, when)
import           Control.Monad.Except (ExceptT (ExceptT), MonadError,
                                       runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT (ReaderT), ask, runReaderT)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef (IORef, newIORef)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Word (Word64)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import           RON.Event (EpochTime, ReplicaClock, ReplicaId, advance,
                            applicationSpecific, getEvents, getPid)
import           System.Directory (canonicalizePath, createDirectoryIfMissing,
                                   doesDirectoryExist, doesPathExist,
                                   listDirectory, removeFile, renameDirectory)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)

import           RON.Storage as X

-- | Environment is the dataDir
newtype StorageT clock a = Storage (ExceptT String (ReaderT FilePath clock) a)
    deriving (Applicative, Functor, Monad, MonadError String, MonadIO)

runStorageT :: FilePath -> StorageT m a -> m (Either String a)
runStorageT dataDir (Storage except) =
    (`runReaderT` dataDir) $ runExceptT except

instance MonadTrans StorageT where
    lift = Storage . lift . lift

instance ReplicaClock m => ReplicaClock (StorageT m) where
    getPid    = lift getPid
    getEvents = lift . getEvents
    advance   = lift . advance

instance (ReplicaClock m, MonadIO m) => MonadStorage (StorageT m) where
    listCollections = Storage $ do
        dataDir <- ask
        liftIO $
            listDirectory dataDir
            >>= filterM (doesDirectoryExist . (dataDir </>))

    listDocuments :: forall doc. Collection doc => StorageT m [DocId doc]
    listDocuments = map DocId <$> listDirectoryIfExists (collectionName @doc)

    listVersions = listDirectoryIfExists . docDir

    saveVersionContent docid version content =
        Storage $ do
            dataDir <- ask
            let docdir = dataDir </> docDir docid
            liftIO $ do
                createDirectoryIfMissing True docdir
                BSL.writeFile (docdir </> version) content

    loadVersionContent docid version = Storage $ do
        dataDir <- ask
        liftIO $ BSL.readFile $ dataDir </> docDir docid </> version

    deleteVersion docid version = Storage $ do
        dataDir <- ask
        liftIO $ do
            let file = dataDir </> docDir docid </> version
            removeFile file
            `catch` \e ->
                unless (isDoesNotExistError e) $ throwIO e

    changeDocId old new = Storage $ do
        db <- ask
        let oldPath = db </> docDir old
            newPath = db </> docDir new
        oldPathCanon <- liftIO $ canonicalizePath oldPath
        newPathCanon <- liftIO $ canonicalizePath newPath
        when (newPathCanon /= oldPathCanon) $ do
            newPathExists <- liftIO $ doesPathExist newPath
            when newPathExists $
                throwError $ unwords
                    [ "changeDocId"
                    , show old, "[", oldPath, "->", oldPathCanon, "]"
                    , show new, "[", newPath, "->", newPathCanon, "]"
                    , ": internal error: new document id is already taken"
                    ]
        when (newPath /= oldPath) $
            liftIO $ renameDirectory oldPath newPath

data Handle = Handle
    { hClock    :: IORef EpochTime
    , hDataDir  :: FilePath
    , hReplica  :: ReplicaId
    }

type Storage = StorageT EpochClock

newHandle :: FilePath -> IO Handle
newHandle hDataDir = do
    time <- getCurrentEpochTime
    hClock <- newIORef time
    hReplica <- applicationSpecific <$> getMacAddress
    pure Handle{hDataDir, hClock, hReplica}

runStorage :: Handle -> Storage a -> IO a
runStorage Handle{hReplica, hDataDir, hClock} action = do
    res <- runEpochClock hReplica hClock $ runStorageT hDataDir action
    either fail pure res

listDirectoryIfExists :: MonadIO m => FilePath -> StorageT m [FilePath]
listDirectoryIfExists relpath = Storage $ do
    dataDir <- ask
    let dir = dataDir </> relpath
    liftIO $ do
        exists <- doesDirectoryExist dir
        if exists then listDirectory dir else pure []

docDir :: forall a . Collection a => DocId a -> FilePath
docDir (DocId dir) = collectionName @a </> dir

-- MAC address

getMacAddress :: IO Word64
getMacAddress = decodeMac <$> getMac where
    getMac
        =   fromMaybe
                (error "Can't get any non-zero MAC address of this machine")
        .   listToMaybe
        .   filter (/= minBound)
        .   map mac
        <$> getNetworkInterfaces
    decodeMac (MAC b5 b4 b3 b2 b1 b0)
        = fromIntegral b5 `shiftL` 40
        + fromIntegral b4 `shiftL` 32
        + fromIntegral b3 `shiftL` 24
        + fromIntegral b2 `shiftL` 16
        + fromIntegral b1 `shiftL` 8
        + fromIntegral b0
