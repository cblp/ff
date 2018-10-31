{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Storage.IO
    ( Handle
    , Storage
    , newHandle
    , runStorage
    , runStorageT
    ) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (filterM, unless)
import           Control.Monad.Except (ExceptT (ExceptT), MonadError,
                                       liftEither, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT (ReaderT), ask, runReaderT)
import           Control.Monad.Trans (lift)
import           Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.IORef (IORef, newIORef)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Word (Word64)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           RON.Event (Clock, EpochClock, EpochTime, Replica, ReplicaId,
                            advance, applicationSpecific, getCurrentEpochTime,
                            getEventUuid, getEvents, getPid, runEpochClock)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (Object (Object), objectFrame, objectId)
import qualified RON.UUID as UUID
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory, removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)

import           RON.Storage (Collection, DocId (DocId), MonadStorage,
                              collectionName, createVersion, deleteVersion,
                              fallbackParse, listCollections, listDocuments,
                              listVersions, readVersion)

-- | Environment is the dataDir
newtype StorageT clock a = Storage (ExceptT String (ReaderT FilePath clock) a)
    deriving (Applicative, Functor, Monad, MonadError String, MonadIO)

runStorageT :: FilePath -> StorageT m a -> m (Either String a)
runStorageT dataDir (Storage except) =
    (`runReaderT` dataDir) $ runExceptT except

instance Replica m => Replica (StorageT m) where
    getPid = Storage $ lift $ lift getPid

instance Clock m => Clock (StorageT m) where
    getEvents = Storage . lift . lift . getEvents
    advance   = Storage . lift . lift . advance

instance (Clock m, MonadIO m) => MonadStorage (StorageT m) where
    listCollections = Storage $ do
        dataDir <- ask
        liftIO $
            listDirectory dataDir
            >>= filterM (doesDirectoryExist . (dataDir </>))

    listDocuments :: forall doc. Collection doc => StorageT m [DocId doc]
    listDocuments = map DocId <$> listDirectoryIfExists (collectionName @doc)

    listVersions = listDirectoryIfExists . docDir

    createVersion obj@Object{objectFrame} = do
        version <- getEventUuid
        Storage $ do
            dataDir <- ask
            let docdir = dataDir </> objectDir obj
            let file = docdir </> UUID.encodeBase32 version
            liftIO $ do
                createDirectoryIfMissing True docdir
                BSL.writeFile file $ serializeStateFrame objectFrame

    readVersion docId@(DocId dir) version = Storage $ do
        dataDir <- ask
        contents <- liftIO $ BSL.readFile $ dataDir </> docDir docId </> version
        objectId <-
            liftEither $
            maybe (Left $ "Bad Base32 UUID " ++ show dir) Right $
            UUID.decodeBase32 dir
        case parseStateFrame contents of
            Right objectFrame -> pure Object{objectId, objectFrame}
            Left ronError     -> case fallbackParse objectId contents of
                Right object       -> pure object
                Left fallbackError -> throwError $ case BSLC.head contents of
                    '{' -> fallbackError
                    _   -> ronError

    deleteVersion docId version = Storage $ do
        dataDir <- ask
        liftIO $ do
            let file = dataDir </> docDir docId </> version
            removeFile file
            `catch` \e ->
                unless (isDoesNotExistError e) $ throwIO e

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

objectDir :: forall a . Collection a => Object a -> FilePath
objectDir Object{objectId} =
    docDir (DocId $ UUID.encodeBase32 objectId :: DocId a)

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
