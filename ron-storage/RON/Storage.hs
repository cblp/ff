{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RON.Storage
    ( Collection (..)
    , CollectionName
    , DocId (..)
    , Document (..)
    , MonadStorage (..)
    , load
    , modify
    , rawDocId
    ) where

import           Control.Monad (when)
import           Control.Monad.Except (MonadError, catchError, liftEither,
                                       throwError)
import           Control.Monad.State.Strict (StateT, execStateT)
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Traversable (for)

import           RON.Data (ReplicatedAsObject, reduceObject')
import           RON.Event (Clock (..))
import           RON.Types (Object, UUID)
import qualified RON.UUID as UUID

type Version = UUID

newtype DocId a = DocId UUID
    deriving Show

rawDocId :: DocId doc -> UUID
rawDocId (DocId uuid) = uuid

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
    createVersion :: Collection a => Object a -> m ()

    readVersion :: Collection a => DocId a -> Version -> m (Object a)

    deleteVersion :: Collection a => DocId a -> Version -> m ()

-- | Result of DB reading
data Document a = Document
    { value    :: Object a
        -- ^ merged value
    , versions :: NonEmpty Version
    }

load :: forall a m . (Collection a, MonadStorage m) => DocId a -> m (Document a)
load docId = loadRetry 3
  where
    -- loadRetry :: Int -> m (Document a)
    loadRetry (n :: Int)
        | n > 0 = do
            versions0 <- listVersions docId
            case versions0 of
                []   -> throwError "Invalid document"
                v:vs -> do
                    let versions = v :| vs
                    let wrapDoc value = Document{value, versions}
                    e <- fmap (fmap wrapDoc . vsconcat) $ for versions $ \ver ->
                        fmapL
                            (( "version " ++ show ver ++ " ("
                            ++ UUID.encodeBase32 ver ++ ")" ++ ": ") ++)
                        <$> catchExcept (readVersion docId ver)
                    liftEither e
        | otherwise = throwError "Maximum retries exceeded"

-- | Validation-like version of 'sconcat'.
vsconcat :: NonEmpty (Either String (Object a)) -> Either String (Object a)
vsconcat = foldr1 vappend
  where
    vappend    (Left  e1)    (Left  e2) = Left $ e1 <> e2
    vappend e1@(Left  _ )     _         = e1
    vappend    (Right a1)    (Right a2) = reduceObject' a1 a2
    vappend     _         e2@(Left  _ ) = e2

catchExcept :: MonadError e m => m a -> m (Either e a)
catchExcept ma = catchError (Right <$> ma) (pure . Left)

fmapL :: (a -> b) -> Either a c -> Either b c
fmapL f = \case
    Left a  -> Left $ f a
    Right c -> Right c

-- TODO(2018-10-22, cblp) call `deleteVersion` from `createVersion`
modify
    :: (Collection a, MonadStorage m)
    => DocId a -> StateT (Object a) m () -> m (Object a)
modify docId f = do
    Document{value = docOld, versions} <- load docId
    docNew <- execStateT f docOld
    when (docNew /= docOld || length versions /= 1) $ do
        createVersion docNew
        for_ versions (deleteVersion docId)
    pure docNew
