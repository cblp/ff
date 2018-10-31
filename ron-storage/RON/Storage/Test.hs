{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RON.Storage.Test (TestDB, runStorageSim) where

import           Control.Monad.Except (ExceptT, MonadError, runExceptT)
import           Control.Monad.State.Strict (StateT, get, gets, modify,
                                             runStateT)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Functor.Compose (Compose (Compose), getCompose)
import           Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           RON.Event (Clock, Replica, applicationSpecific)
import           RON.Event.Simulation (ReplicaSim, runNetworkSim, runReplicaSim)

import           RON.Storage (Collection, CollectionName, DocId (DocId),
                              MonadStorage, Version, changeDocId,
                              collectionName, deleteVersion, listCollections,
                              listDocuments, listVersions, loadVersionContent,
                              saveVersionContent)

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

    saveVersionContent (DocId docid :: DocId a) version content = do
        let document = BSLC.lines content
        let insertDocumentVersion =
                Just . Map.insertWith (<>) version document . fromMaybe mempty
        let alterDocument
                = Just
                . Map.alter insertDocumentVersion docid
                . fromMaybe mempty
        let alterCollection = Map.alter alterDocument (collectionName @a)
        StorageSim $ modify alterCollection

    loadVersionContent (DocId dir :: DocId a) version = StorageSim $ do
        db <- get
        pure $ BSLC.unlines $ db !. collectionName @a !. dir ! version

    deleteVersion (DocId doc :: DocId a) version
        = StorageSim
        . modify
        . (`Map.adjust` collectionName @a)
        . (`Map.adjust` doc)
        $ Map.delete version

    changeDocId (DocId old :: DocId a) (DocId new :: DocId a) = StorageSim $
        modify $ (`Map.adjust` collectionName @a) $ \collection ->
            maybe collection (uncurry $ Map.insert new) $
            mapTake old collection

(!.) :: Ord a => Map a (Map b c) -> a -> Map b c
m !. a = fromMaybe Map.empty $ m !? a

mapTake :: Ord k => k -> Map k a -> Maybe (a, Map k a)
mapTake k = getCompose . Map.alterF (Compose . f) k where
    f = \case
        Nothing -> Nothing
        Just a  -> Just (a, Nothing)
