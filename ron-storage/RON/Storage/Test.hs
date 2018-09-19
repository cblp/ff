{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RON.Storage.Test (TestDB, runStorageSim) where

import           Control.Lens (at, non, (.=), (<>=))
import           Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.State.Strict (StateT, get, gets, runStateT)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           RON.Event (Clock, Replica, applicationSpecific, getEventUuid)
import           RON.Event.Simulation (ReplicaSim, runNetworkSim, runReplicaSim)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (Object (..), UUID)

import           RON.Storage (Collection, CollectionName, DocId (..),
                              MonadStorage (..), collectionName, fallbackParse)

type ByteStringL = BSL.ByteString

type TestDB = Map CollectionName (Map DocumentId (Map Version Document))

type Document = [ByteStringL]

type DocumentId = UUID

type Version = UUID

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
        version <- getEventUuid
        StorageSim $
            at (collectionName @a) . non mempty . at objectId . non mempty
            . at version . non mempty
            <>= BSLC.lines (serializeStateFrame objectFrame)

    readVersion (DocId objectId :: DocId a) version = StorageSim $ do
        db <- get
        let contents =
                BSLC.unlines $ db !. collectionName @a !. objectId ! version
        case parseStateFrame contents of
            Right objectFrame -> pure Object{objectId, objectFrame}
            Left ronError     -> case fallbackParse objectId contents of
                Right object       -> pure object
                Left fallbackError -> throwError $ case BSLC.head contents of
                    '{' -> fallbackError
                    _   -> ronError

    deleteVersion (DocId doc :: DocId a) version = StorageSim $
        at (collectionName @a) . non mempty . at doc . non mempty
        . at version .= Nothing

(!.) :: Ord a => Map a (Map b c) -> a -> Map b c
m !. a = fromMaybe Map.empty $ m !? a
