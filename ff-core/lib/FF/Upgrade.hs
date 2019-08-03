{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Upgrade
  ( upgradeDatabase
    )
where

import Data.Foldable (for_)
import FF.Types (Note)
import RON.Event (getEventUuid)
import RON.Storage.Backend
  ( MonadStorage,
    changeDocId,
    getCollections,
    getDocuments
    )
import RON.Storage.FS
  ( Collection,
    DocId,
    decodeDocId,
    docIdFromUuid,
    modify
    )

upgradeDatabase :: MonadStorage m => m ()
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
    modify docid' $ pure ()

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
