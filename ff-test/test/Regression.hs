{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Regression (mkRegressionTest) where

import           Data.Aeson (ToJSON)
import           Data.Aeson.TH (defaultOptions, deriveToJSON)
import           Data.Traversable (for)
import           Data.Yaml (encodeFile)
import           RON.Data (evalObjectState, getObject)
import           RON.Data.RGA (RGA)
import           RON.Storage (Collection, CollectionName, DocId, loadDocument)
import           RON.Storage.Backend (Document (Document, objectFrame),
                                      getCollections, getDocuments)
import           RON.Storage.FS (Handle, newHandle, runStorage)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden (goldenVsFileDiff)

import           FF.Types (Note, NoteStatus, Status, Track)

deriveToJSON defaultOptions ''RGA

deriveToJSON defaultOptions ''Note
deriveToJSON defaultOptions ''NoteStatus
deriveToJSON defaultOptions ''Status
deriveToJSON defaultOptions ''Track

mkRegressionTest :: IO (FilePath -> TestTree)
mkRegressionTest = do
    h <- newHandle "../.ff"
    collections <- runStorage h getCollections
    tests <- for collections $ \collection -> case collection of
        "note" -> testCollection @Note h collection
        _      -> fail $ "unsupported type " ++ show collection
    pure $ \tmp -> testGroup "regression" [test tmp | test <- tests]

testCollection
    :: forall a
    . (Collection a, ToJSON a)
    => Handle -> CollectionName -> IO (FilePath -> TestTree)
testCollection h collectionName = do
    docs <- runStorage h $ getDocuments @_ @a
    pure $ \tmp -> testGroup collectionName $ map (testDoc h tmp) docs

testDoc :: (Collection a, ToJSON a) => Handle -> FilePath -> DocId a -> TestTree
testDoc h tmp docid =
    goldenVsFileDiff (show docid) diff ("ff.dump" </> show docid) outFile action
  where
    outFile = tmp </> show docid
    action = do
        Document{objectFrame} <- runStorage h $ loadDocument docid
        val <- either (fail . show) pure $ evalObjectState objectFrame getObject
        createDirectoryIfMissing True $ takeDirectory outFile
        encodeFile outFile val

diff :: String -> String -> [String]
diff ref new = ["colordiff", "-u", ref, new]
