{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Storage.Test (TestDB, runStorageSim) where

import           Control.Monad.Except (ExceptT, MonadError, liftEither,
                                       runExceptT, throwError)
import           Control.Monad.State.Strict (StateT, get, gets, modify,
                                             runStateT)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           RON.Event (Clock, Replica, applicationSpecific, getEventUuid)
import           RON.Event.Simulation (ReplicaSim, runNetworkSim, runReplicaSim)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (Object (Object), objectFrame, objectId)
import qualified RON.UUID as UUID

import           RON.Storage (Collection, CollectionName, DocId (DocId),
                              MonadStorage, Version, collectionName,
                              createVersion, deleteVersion, fallbackParse,
                              listCollections, listDocuments, listVersions,
                              readVersion)

type ByteStringL = BSL.ByteString

type TestDB = Map CollectionName (Map DocumentId (Map Version Document))

type Document = [ByteStringL]

type DocumentId = FilePath

-- * Storage simulation

newtype StorageSim a = StorageSim (StateT TestDB (ExceptT String ReplicaSim) a)
    deriving (Applicative, Clock, Functor, Monad, MonadError String, Replica)

runStorageSim :: TestDB -> StorageSim a -> Either String (a, TestDB)
runStorageSim db (StorageSim action) =
    runNetworkSim $ runReplicaSim (applicationSpecific 34) $
    runExceptT $ runStateT action db

-- TODO(2018-10-26, cblp) move common implementation between Storage and
-- StorageSim to common functions
instance MonadStorage StorageSim where
    listCollections = StorageSim $ gets Map.keys

    listDocuments :: forall a . Collection a => StorageSim [DocId a]
    listDocuments = StorageSim $ do
        db <- get
        pure $ map DocId $ Map.keys $ db !. collectionName @a

    listVersions (DocId doc :: DocId a) = StorageSim $ do
        db <- get
        pure $ Map.keys $ db !. collectionName @a !. doc

    createVersion (Object{objectId, objectFrame} :: Object a) = do
        version <- UUID.encodeBase32 <$> getEventUuid
        let document = BSLC.lines $ serializeStateFrame objectFrame
        let insertDocumentVersion =
                Just . Map.insertWith (<>) version document . fromMaybe mempty
        let alterDocument
                = Just
                . Map.alter insertDocumentVersion (UUID.encodeBase32 objectId)
                . fromMaybe mempty
        let alterCollection = Map.alter alterDocument (collectionName @a)
        StorageSim $ modify alterCollection

    readVersion (DocId dir :: DocId a) version = StorageSim $ do
        db <- get
        let contents = BSLC.unlines $ db !. collectionName @a !. dir ! version
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

    deleteVersion (DocId doc :: DocId a) version
        = StorageSim
        . modify
        . (`Map.adjust` collectionName @a)
        . (`Map.adjust` doc)
        $ Map.delete version

(!.) :: Ord a => Map a (Map b c) -> a -> Map b c
m !. a = fromMaybe Map.empty $ m !? a
