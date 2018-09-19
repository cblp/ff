{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RON.Storage.IO
    ( Handle
    , Storage
    , newHandle
    , runStorage
    , runStorageT
    ) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (filterM, unless)
import           Control.Monad.Except (ExceptT (..), MonadError (..),
                                       liftEither, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT (..), ask)
import           Control.Monad.Trans (lift)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce (coerce)
import           Data.IORef (IORef, newIORef)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Traversable (for)
import           Data.Word (Word64)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           RON.Event (Clock (..), EpochClock, EpochTime, Replica (..),
                            ReplicaId, applicationSpecific, getCurrentEpochTime,
                            getEventUuid, runEpochClock)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (Object (..))
import qualified RON.UUID as UUID
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory, removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)

import           RON.Storage (Collection (..), DocId (..), MonadStorage (..))

-- | Environment is the dataDir
newtype StorageT clock a = Storage (ExceptT String (ReaderT FilePath clock) a)
    deriving (Applicative, Functor, Monad, MonadIO)

runStorageT :: FilePath -> StorageT m a -> m (Either String a)
runStorageT dataDir (Storage except) =
    (`runReaderT` dataDir) $ runExceptT except

instance Replica m => Replica (StorageT m) where
    getPid = Storage $ lift $ lift getPid

instance Clock m => Clock (StorageT m) where
    getEvents = Storage . lift . lift . getEvents
    advance   = Storage . lift . lift . advance

instance Monad m => MonadError String (StorageT m) where
    throwError = Storage . throwError
    catchError = coerce . catchError

instance (Clock m, MonadIO m) => MonadStorage (StorageT m) where
    listCollections = Storage $ do
        dataDir <- ask
        liftIO $
            listDirectory dataDir
            >>= filterM (doesDirectoryExist . (dataDir </>))

    listDocuments :: forall doc. Collection doc => StorageT m [DocId doc]
    listDocuments = do
        docdirs <- listDirectoryIfExists (collectionName @doc)
        for docdirs $
            fmap DocId . liftEither . maybe (Left "Bad UUID") Right .
            UUID.decodeBase32

    listVersions (docId :: DocId doc) = do
        files <- listDirectoryIfExists $ docDir docId
        Storage $
            for files $ \file -> do
                let path = docDir docId </> file
                case UUID.decodeBase32 file of
                    Just uuid -> pure uuid
                    Nothing ->
                        throwError $
                        "Directory name " ++ path ++ " is not a UUID"

    createVersion obj@Object{objectFrame} = do
        version <- getEventUuid
        Storage $ do
            dataDir <- ask
            let docdir = dataDir </> objectDir obj
            let file = docdir </> UUID.encodeBase32 version
            liftIO $ do
                createDirectoryIfMissing True docdir
                BSL.writeFile file $ serializeStateFrame objectFrame

    readVersion docId@(DocId objectId) version = Storage $ do
        dataDir <- ask
        contents <- liftIO $
            BSL.readFile $
            dataDir </> docDir docId </> UUID.encodeBase32 version
        case parseStateFrame contents of
            Right objectFrame -> pure Object{objectId, objectFrame}
            Left ronError     -> case fallbackParse objectId contents of
                Right object        -> pure object
                Left _fallbackError -> throwError ronError

    deleteVersion docId version = Storage $ do
        dataDir <- ask
        liftIO $ do
            let file = dataDir </> docDir docId </> UUID.encodeBase32 version
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
    pure Handle{..}

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
docDir (DocId docId) = collectionName @a </> UUID.encodeBase32 docId

objectDir :: forall a . Collection a => Object a -> FilePath
objectDir Object{objectId} = docDir $ DocId @a objectId

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
    decodeMac (MAC b5 b4 b3 b2 b1 b0) =
        Binary.decode $ BSL.pack [0, 0, b5, b4, b3, b2, b1, b0]
