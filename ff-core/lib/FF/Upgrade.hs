{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Upgrade (upgradeDatabase) where

import           Data.Foldable (for_)
import           RON.Storage (Collection, MonadStorage, listCollections,
                              listDocuments, modify)

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
    for_ docs (`modify` pure ())
