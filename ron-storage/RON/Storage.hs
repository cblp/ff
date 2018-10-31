{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Storage
    ( Collection (..)
    , CollectionName
    , DocId (..)
    , Document (..)
    , MonadStorage (..)
    , Version
    , createVersion
    , decodeDocId
    , loadDocument
    , modify
    , rawDocId
    , readVersion
    ) where

import           Control.Monad (guard, when)
import           Control.Monad.Except (MonadError, catchError, liftEither,
                                       throwError)
import           Control.Monad.State.Strict (StateT, execStateT)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe (fromMaybe)
import           Data.Traversable (for)
import           System.FilePath ((</>))

import           RON.Data (ReplicatedAsObject, reduceObject')
import           RON.Event (Clock, getEventUuid)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (Object (Object), UUID, objectFrame, objectId)
import qualified RON.UUID as UUID

type Version = FilePath

newtype DocId a = DocId FilePath

instance Collection a => Show (DocId a) where
    show (DocId file) = collectionName @a </> file

rawDocId :: DocId doc -> FilePath
rawDocId (DocId file) = file

type CollectionName = FilePath

class ReplicatedAsObject a => Collection a where
    collectionName :: CollectionName
    fallbackParse :: UUID -> ByteString -> Either String (Object a)

-- | TODO rename list -> getList
class (Clock m, MonadError String m) => MonadStorage m where
    listCollections :: m [CollectionName]

    -- | Must return @[]@ for non-existent collection
    listDocuments :: Collection a => m [DocId a]

    -- | Must return @[]@ for non-existent document
    listVersions :: Collection a => DocId a -> m [Version]

    -- | Must create collection and document if not exist
    saveVersionContent
        :: Collection a => DocId a -> Version -> ByteString -> m ()

    loadVersionContent :: Collection a => DocId a -> Version -> m ByteString

    deleteVersion :: Collection a => DocId a -> Version -> m ()

    changeDocId :: Collection a => DocId a -> DocId a -> m ()

decodeDocId :: FilePath -> Maybe UUID
decodeDocId file = do
    uuid <- UUID.decodeBase32 file
    guard $ UUID.encodeBase32 uuid == file
    pure uuid

readVersion
    :: MonadStorage m => Collection a => DocId a -> Version -> m (Object a)
readVersion docid@(DocId dir) version = do
    objectId <-
        liftEither $
        maybe (Left $ "Bad Base32 UUID " ++ show dir) Right $ decodeDocId dir
    contents <- loadVersionContent docid version
    case parseStateFrame contents of
        Right objectFrame -> pure Object{objectId, objectFrame}
        Left ronError     -> case fallbackParse objectId contents of
            Right object       -> pure object
            Left fallbackError -> throwError $ case BSLC.head contents of
                '{' -> fallbackError
                _   -> ronError

-- | Result of DB reading
data Document a = Document
    { value    :: Object a
        -- ^ merged value
    , versions :: NonEmpty Version
    }
    deriving Show

loadDocument :: (Collection a, MonadStorage m) => DocId a -> m (Document a)
loadDocument docid = loadRetry (3 :: Int)
  where
    loadRetry n
        | n > 0 = do
            versions0 <- listVersions docid
            case versions0 of
                []   -> throwError $ "Empty document " ++ show docid
                v:vs -> do
                    let versions = v :| vs
                    let wrapDoc value = Document{value, versions}
                    e1 <-
                        for versions $ \ver -> do
                            let ctx =   "document "  ++ show docid
                                    ++  ", version " ++ ver
                                    ++  ": "
                            e1 <- try $ readVersion docid ver
                            pure $ fmapL (ctx ++) e1
                    liftEither $ wrapDoc <$> vsconcat e1
        | otherwise = throwError "Maximum retries exceeded"

-- | Validation-like version of 'sconcat'.
vsconcat :: NonEmpty (Either String (Object a)) -> Either String (Object a)
vsconcat = foldr1 vappend
  where
    vappend    (Left  e1)    (Left  e2) = Left $ e1 ++ "\n" ++ e2
    vappend e1@(Left  _ )     _         = e1
    vappend    (Right a1)    (Right a2) = reduceObject' a1 a2
    vappend     _         e2@(Left  _ ) = e2

try :: MonadError e m => m a -> m (Either e a)
try ma = (Right <$> ma) `catchError` (pure . Left)

fmapL :: (a -> b) -> Either a c -> Either b c
fmapL f = \case
    Left a  -> Left $ f a
    Right c -> Right c

-- TODO(2018-10-22, cblp) call `deleteVersion` from `createVersion`
modify
    :: (Collection a, MonadStorage m)
    => DocId a -> StateT (Object a) m () -> m (Object a)
modify docid f = do
    Document{value = docOld, versions} <- loadDocument docid
    docNew <- execStateT f docOld
    when (docNew /= docOld || length versions /= 1) $ do
        createVersion (Just docid) docNew
        for_ versions $ deleteVersion docid
    pure docNew

createVersion
    :: (Collection a, MonadStorage m) => Maybe (DocId a) -> Object a -> m ()
createVersion mDocid Object{objectId, objectFrame} = do
    version <- UUID.encodeBase32 <$> getEventUuid
    let docid = fromMaybe (DocId $ UUID.encodeBase32 objectId) mDocid
    saveVersionContent docid version (serializeStateFrame objectFrame)
