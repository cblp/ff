{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Upgrade (upgradeDatabase) where

import           Data.Foldable (for_)
import           RON.Event (getEventUuid)
import           RON.Storage (Collection, DocId (DocId), MonadStorage,
                              changeDocId, decodeDocId, listCollections,
                              listDocuments, modify)
import qualified RON.UUID as UUID

import           FF.Types (Note)

upgradeDatabase :: MonadStorage m => m ()
upgradeDatabase = do
    collections <- listCollections
    for_ collections $ \case
        "note"      -> upgradeCollection @Note
        collection  -> fail $ "unsupported type " ++ show collection

upgradeCollection :: forall a m . (Collection a, MonadStorage m) => m ()
upgradeCollection = do
    docs <- listDocuments @_ @a
    for_ docs $ \docid -> do
        docid' <- upgradeDocId docid
        modify docid' $ pure ()

upgradeDocId :: (Collection a, MonadStorage m) => DocId a -> m (DocId a)
upgradeDocId docid@(DocId file) = do
    let mu = decodeDocId file
    case mu of
        Just _  -> pure docid
        Nothing -> do
            docid' <- DocId . UUID.encodeBase32 <$> getEventUuid
            changeDocId docid docid'
            pure docid'
