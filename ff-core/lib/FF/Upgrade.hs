{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Upgrade
  ( upgradeDatabase
    )
where

import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import FF.Types (Note)
import RON.Data (ObjectStateT, Rep, getObjectStateChunk, reducibleOpType)
import RON.Data.LWW (lwwType)
import RON.Data.ORSet (ORSetRep)
import RON.Error (Error (Error), MonadE, liftMaybe)
import RON.Event (getEventUuid)
import RON.Prelude
import RON.Storage.Backend
  ( MonadStorage,
    changeDocId,
    getCollections,
    getDocuments
    )
import RON.Storage.FS (Collection, DocId, decodeDocId, docIdFromUuid, modify)
import RON.Types
  ( Object (Object),
    StateFrame,
    UUID,
    WireStateChunk (WireStateChunk, stateBody, stateType)
    )

upgradeDatabase :: (MonadFail m, MonadStorage m) => m ()
upgradeDatabase = do
  collections <- getCollections
  for_ collections $ \case
    "note" -> upgradeNoteCollection
    collection -> fail $ "unsupported type " ++ show collection

upgradeNoteCollection :: MonadStorage m => m ()
upgradeNoteCollection = do
  docs <- getDocuments @_ @Note
  for_ docs $ \docid -> do
    docid' <- upgradeDocId docid
    modify docid' $ do
      Object note <- ask
      convertLwwToSet note

convertLwwToSet :: (MonadE m, MonadState StateFrame m) => UUID -> m ()
convertLwwToSet uuid = do
  frame <- get
  WireStateChunk {stateType, stateBody} <-
    liftMaybe "no such object in chunk" $ Map.lookup uuid frame
  unless (stateType == lwwType)
    $ throwError
    $ Error "bad type"
        [Error "expected lww" [], Error ("got " <> show stateType) []]
  -- (a, chunk') <- f chunk
  modify'
    $ Map.insert uuid
    $ WireStateChunk {stateType = reducibleOpType @ORSetRep, stateBody = chunk'}

upgradeDocId :: (Collection a, MonadStorage m) => DocId a -> m (DocId a)
upgradeDocId docid = do
  let mu = decodeDocId docid
  case mu of
    Just (True, _) -> pure docid
    Just (False, uuid) -> do
      let docid' = docIdFromUuid uuid
      changeDocId docid docid'
      pure docid'
    Nothing -> do
      docid' <- docIdFromUuid <$> getEventUuid
      changeDocId docid docid'
      pure docid'
