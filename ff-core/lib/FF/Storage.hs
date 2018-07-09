{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Storage where

import           Control.Concurrent.STM (TVar)
import           Control.Error (ExceptT (..), runExceptT)
import           Control.Exception (catch, throwIO)
import           Control.Monad (unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (Clock, LamportClock,
                                    LamportTime (LamportTime), LocalTime,
                                    Pid (Pid), Process, getTime,
                                    runLamportClock)
import           Data.Aeson (FromJSON, ToJSON, ToJSONKey, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (chr, ord)
import           Data.Foldable (for_)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Semigroup (sconcat)
import           Data.Traversable (for)
import           Numeric (showIntAtBase)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory, removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)

class (CvRDT doc, FromJSON doc, ToJSON doc) => Collection doc where
    collectionName :: FilePath

newtype DocId doc = DocId FilePath
    deriving (Eq, Ord, ToJSON, ToJSONKey)

instance Show (DocId doc) where
    show (DocId path) = path

-- | Environment is the dataDir
newtype Storage a = Storage (ReaderT FilePath LamportClock a)
    deriving (Applicative, Clock, Functor, Monad, MonadIO, Process)

type Version = FilePath

data Document a = Document
    { value    :: a
    , versions :: [Version]
    }

-- | Environment is the dataDir
class Clock m => MonadStorage m where
    listDirectoryIfExists
        :: FilePath     -- ^ Path relative to data dir
        -> m [FilePath] -- ^ Paths relative to data dir
    createFile :: Collection doc => DocId doc -> LamportTime -> doc -> m ()
    readFileEither
        :: Collection doc => DocId doc -> Version -> m (Either String doc)
    removeFileIfExists :: Collection doc => DocId doc -> Version -> m ()

instance MonadStorage Storage where
    listDirectoryIfExists relpath = Storage $ do
        dir <- asks (</> relpath)
        liftIO $ do
            exists <- doesDirectoryExist dir
            if exists then listDirectory dir else pure []

    createFile docId time doc = Storage $ do
        docDir <- askDocDir docId
        let file = docDir </> lamportTimeToFileName time
        liftIO $ do
            createDirectoryIfMissing True docDir
            BSL.writeFile file $ encode doc

    readFileEither docId version = Storage $ do
        docDir <- askDocDir docId
        let file = docDir </> version
        contents <- liftIO $ BSL.readFile file
        pure $ eitherDecode contents

    removeFileIfExists docId version = Storage $ do
        docDir <- askDocDir docId
        let file = docDir </> version
        liftIO $
            removeFile file
            `catch` \e ->
                unless (isDoesNotExistError e) $ throwIO e

runStorage :: FilePath -> TVar LocalTime -> Storage a -> IO a
runStorage dataDir var (Storage action) =
    runLamportClock var $ runReaderT action dataDir

listDocuments
    :: forall doc m . (Collection doc, MonadStorage m) => m [DocId doc]
listDocuments = map DocId <$> listDirectoryIfExists (collectionName @doc)

load
    :: forall a m
     . (Collection a, MonadStorage m)
    => DocId a
    -> m (Maybe (Document a))
load docId = do
    (versions, values) <- loadAnyway
    pure $ (\v -> Document{value = v, versions}) . sconcat <$> nonEmpty values
  where
    loadAnyway = do
        versions <- listVersions docId
        eValues <- runExceptT $ for versions $ ExceptT . readFileEither docId
        case eValues of
            Right values -> pure (versions, values)
            Left _       -> loadAnyway

listVersions
    :: forall doc m
     . (Collection doc, MonadStorage m)
    => DocId doc
    -> m [Version]
listVersions (DocId docId) =
    listDirectoryIfExists $ collectionName @doc </> docId

askDocDir
    :: forall doc m
     . (Collection doc, MonadReader FilePath m)
    => DocId doc
    -> m FilePath
askDocDir (DocId docId) = asks (</> collectionName @doc </> docId)

save
    :: forall doc m
     . (Collection doc, MonadStorage m)
    => DocId doc
    -> doc
    -> m ()
save docId doc = do
    time <- getTime
    createFile docId time doc

create :: (Collection doc, MonadStorage m) => doc -> m (DocId doc)
create doc = do
    docId <- DocId . lamportTimeToFileName <$> getTime
    save docId doc
    pure docId

showBase36K :: (Integral a, Show a) => a -> String -> String
showBase36K = showIntAtBase 36 intToDigit36

showBase36 :: (Integral a, Show a) => a -> String
showBase36 a = showBase36K a ""

intToDigit36 :: Int -> Char
intToDigit36 i | (i >= 0) && (i <= 9)   = chr (ord '0' + i)
               | (i >= 10) && (i <= 35) = chr (ord 'a' - 10 + i)
               | otherwise              = error ("not a digit " ++ show i)

lamportTimeToFileName :: LamportTime -> FilePath
lamportTimeToFileName (LamportTime time (Pid pid)) =
    showBase36K time $ '-' : showBase36K pid ""

-- | For user-supplied function input Nothing means non-existent document.
modify
    :: (Collection doc, Eq doc, MonadStorage m)
    => DocId doc
    -> (Maybe doc -> m (a, doc))
    -> m a
modify docId f =
    load docId >>= \case
        Just Document{value = docOld, versions} -> do
            (a, docNew) <- f $ Just docOld
            when (docNew /= docOld) $ do
                for_ versions (removeFileIfExists docId)
                save docId docNew
            pure a
        Nothing -> do
            (a, docNew) <- f Nothing
            save docId docNew
            pure a
