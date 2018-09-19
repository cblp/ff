{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Storage where

import           Control.Concurrent.STM (TVar)
import           Control.Error (fmapL)
import           Control.Exception (catch, throwIO)
import           Control.Monad (filterM, unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, asks,
                                       runReaderT)
import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (Clock, LamportClock,
                                    LamportTime (LamportTime), LocalTime,
                                    Pid (Pid), Process, getTime,
                                    runLamportClock)
import           Data.Aeson (FromJSON, ToJSON, ToJSONKey, eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty')
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (chr, ord)
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Traversable (for)
import           Numeric (showIntAtBase)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory, removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)

type CollectionName = FilePath

class (CvRDT doc, FromJSON doc, ToJSON doc) => Collection doc where
    collectionName :: CollectionName

newtype DocId doc = DocId FilePath
    deriving (Eq, Ord, Show, ToJSON, ToJSONKey)

rawDocId :: DocId doc -> FilePath
rawDocId (DocId name) = name

-- | Environment is the dataDir
newtype StorageT clock a = Storage (ReaderT FilePath clock a)
    deriving (Applicative, Clock, Functor, Monad, MonadIO, Process)

type Storage = StorageT LamportClock

data Handle = Handle
    { hDataDir  :: FilePath
    , hClock    :: TVar LocalTime
    }

type Version = FilePath

data Document a = Document
    { value    :: a
    , versions :: NonEmpty Version
    }

class Clock m => MonadStorage m where
    listCollections :: m [CollectionName]

    -- | Must return @[]@ for non-existent collection
    listDocuments :: Collection doc => m [DocId doc]

    -- | Must return @[]@ for non-existent document
    listVersions :: Collection doc => DocId doc -> m [Version]

    -- | Must create collection and document if not exist
    createVersion :: Collection doc => DocId doc -> Version -> doc -> m ()

    readVersion
        :: Collection doc => DocId doc -> Version -> m (Either String doc)

    deleteVersion :: Collection doc => DocId doc -> Version -> m ()

listDirectoryIfExists :: MonadIO m => FilePath -> StorageT m [FilePath]
listDirectoryIfExists relpath = Storage $ do
    dir <- asks (</> relpath)
    liftIO $ do
        exists <- doesDirectoryExist dir
        if exists then listDirectory dir else pure []

instance (Clock m, MonadIO m) => MonadStorage (StorageT m) where
    listCollections = Storage $ do
        dataDir <- ask
        liftIO $
            listDirectory dataDir
            >>= filterM (doesDirectoryExist . (dataDir </>))

    listDocuments :: forall doc. Collection doc => StorageT m [DocId doc]
    listDocuments = map DocId <$> listDirectoryIfExists (collectionName @doc)

    listVersions (DocId docId :: DocId doc) =
        listDirectoryIfExists $ collectionName @doc </> docId

    createVersion docId version doc = Storage $ do
        docDir <- askDocDir docId
        let file = docDir </> version
        liftIO $ do
            createDirectoryIfMissing True docDir
            BSL.writeFile file $ encodePretty' jsonConfig doc

    readVersion docId version = Storage $ do
        docDir <- askDocDir docId
        let file = docDir </> version
        contents <- liftIO $ BSL.readFile file
        pure $ eitherDecode contents

    deleteVersion docId version = Storage $ do
        docDir <- askDocDir docId
        let file = docDir </> version
        liftIO $
            removeFile file
            `catch` \e ->
                unless (isDoesNotExistError e) $ throwIO e

jsonConfig :: Json.Config
jsonConfig = Json.defConfig
    { Json.confIndent = Json.Spaces 2
    , Json.confCompare = compare
    , Json.confTrailingNewline = True
    }

runStorage :: Handle -> Storage a -> IO a
runStorage Handle{..} = runLamportClock hClock . runStorageT hDataDir

runStorageT :: FilePath -> StorageT m a -> m a
runStorageT hDataDir (Storage action) = runReaderT action hDataDir

load
    :: forall doc m
     . (Collection doc, MonadStorage m)
    => DocId doc
    -> m (Either String (Document doc))
load docId = loadRetry 3
  where
    loadRetry (n :: Int)
        | n > 0 = do
            versions0 <- listVersions docId
            case versions0 of
                []   -> pure $ Left "Invalid document"
                v:vs -> do
                    let versions = v :| vs
                    let wrapDoc value = Document{value, versions}
                    fmap (fmap wrapDoc . vsconcat) $ for versions $ \ver ->
                        fmapL (("version " ++ ver ++ ": ") ++)
                        <$> readVersion docId ver
        | otherwise = pure $ Left "Maximum retries exceeded"

askDocDir
    :: forall doc m
     . (Collection doc, MonadReader FilePath m)
    => DocId doc
    -> m FilePath
askDocDir (DocId docId) = asks (</> collectionName @doc </> docId)

update
    :: forall doc m
     . (Collection doc, MonadStorage m)
    => DocId doc
    -> doc
    -> m ()
update docId doc = do
    time <- getTime
    createVersion docId (lamportTimeToFileName time) doc

create :: (Collection doc, MonadStorage m) => doc -> m (DocId doc)
create doc = do
    docId <- DocId . lamportTimeToFileName <$> getTime
    update docId doc
    pure docId

showBase36K :: (Integral a, Show a) => a -> String -> String
showBase36K = showIntAtBase 36 intToDigit36

showBase36 :: (Integral a, Show a) => a -> String
showBase36 a = showBase36K a ""

intToDigit36 :: Int -> Char
intToDigit36 i
    | (i >= 0) && (i <= 9)   = chr (ord '0' + i)
    | (i >= 10) && (i <= 35) = chr (ord 'a' - 10 + i)
    | otherwise              = error ("not a digit " ++ show i)

lamportTimeToFileName :: LamportTime -> FilePath
lamportTimeToFileName (LamportTime time (Pid pid)) =
    showBase36K time $ '-' : showBase36K pid ""

modify
    :: (Collection doc, Eq doc, MonadStorage m)
    => DocId doc
    -> (doc -> m doc)
    -> m doc
modify docId f = do
    mdoc <- load docId
    case mdoc of
        Right Document{value = docOld, versions} -> do
            docNew <- f docOld
            when (docNew /= docOld || length versions /= 1) $ do
                for_ versions (deleteVersion docId)
                update docId docNew
            pure docNew
        Left e -> fail $ "Document " ++ show docId ++ ": " ++ e

-- | Validation-like version of 'sconcat'.
vsconcat :: (Semigroup e, Semigroup a) => NonEmpty (Either e a) -> Either e a
vsconcat = foldr1 vappend
  where
    vappend    (Left  e1)    (Left  e2) = Left  $ e1 <> e2
    vappend e1@(Left  _ )     _         =         e1
    vappend    (Right a1)    (Right a2) = Right $ a1 <> a2
    vappend     _         e2@(Left  _ ) =               e2
