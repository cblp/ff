{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Test (TestDB, runStorageSim) where

import           Control.Lens (Traversal', at, non, use, uses, (.=))
import           Control.Monad.State.Strict (StateT, gets, runStateT)
import           CRDT.LamportClock (Clock, Pid (..), Process)
import           CRDT.LamportClock.Simulation (ProcessSim, runLamportClockSim,
                                               runProcessSim)
import qualified Data.Aeson as JSON
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           FF.Storage (CollectionName, DocId (..), MonadStorage (..),
                             Version, collectionName)
import qualified FF.Storage as Storage

type TestDB = Map CollectionName Collection

type Collection = Map FilePath Document

type Document = Map Version [ByteString]

newtype StorageSim a = StorageSim (StateT TestDB ProcessSim a)
    deriving (Applicative, Clock, Functor, Monad, Process)

instance MonadStorage StorageSim where
    listCollections = StorageSim $ gets Map.keys

    listDocuments
        :: forall doc. Storage.Collection doc => StorageSim [DocId doc]
    listDocuments =
        StorageSim $ uses (at' (collectionName @doc)) $ map DocId . Map.keys

    listVersions (DocId docId :: DocId doc) =
        StorageSim $ uses (at' (collectionName @doc) . at' docId) Map.keys

    createVersion (DocId docId :: DocId doc) version doc = StorageSim $
        at' (collectionName @doc) . at' docId . at' version
        .= BSLC.lines (JSON.encode doc)

    readVersion (DocId docId :: DocId doc) version = StorageSim $ do
        mdoc <- use (at' (collectionName @doc) . at' docId . at version)
        pure $ case mdoc of
            Nothing  -> Left "Not found"
            Just doc -> JSON.eitherDecode $ BSLC.unlines doc

    deleteVersion (DocId docId :: DocId doc) version = StorageSim $
        at' (collectionName @doc) . at' docId . at version .= Nothing

runStorageSim :: TestDB -> StorageSim a -> Either String (a, TestDB)
runStorageSim db (StorageSim action) =
    runLamportClockSim . runProcessSim (Pid 314159) $ runStateT action db

at' :: (Ord k, Eq v, Monoid v) => k -> Traversal' (Map k v) v
at' i = at i . non mempty
