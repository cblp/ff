{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Regression
  ( mkRegressionTest
    )
where

import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Traversable (for)
import Data.Yaml (encodeFile)
import FF.Storage (Handle, newHandle, runFFStorage)
import FF.Types
  ( Entity (Entity, entityVal),
    Note,
    NoteId,
    NoteStatus,
    Status,
    Track,
    loadNote
    )
import RON.Data.RGA (RGA)
import RON.Storage (CollectionName)
import RON.Storage.Backend (getCollections, getDocuments)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

deriveToJSON defaultOptions ''RGA

deriveToJSON defaultOptions ''Note

deriveToJSON defaultOptions ''NoteStatus

deriveToJSON defaultOptions ''Status

deriveToJSON defaultOptions ''Track

mkRegressionTest :: IO (FilePath -> TestTree)
mkRegressionTest = do
  h <- newHandle "../.ff"
  collections <- runFFStorage h getCollections
  tests <-
    for collections $ \collection -> case collection of
      "note" -> testNoteCollection h collection
      _ -> fail $ "unsupported type " ++ show collection
  pure $ \tmp -> testGroup "regression" [test tmp | test <- tests]

testNoteCollection :: Handle -> CollectionName -> IO (FilePath -> TestTree)
testNoteCollection h collectionName = do
  docs <- runFFStorage h $ getDocuments @_ @Note
  pure $ \tmp -> testGroup collectionName $ map (testNote h tmp) docs

testNote :: Handle -> FilePath -> NoteId -> TestTree
testNote h tmp docid =
  goldenVsFileDiff (show docid) diff ("ff.dump" </> show docid) outFile action
  where
    outFile = tmp </> show docid
    action = do
      Entity {entityVal = val} <- runFFStorage h $ loadNote docid
      createDirectoryIfMissing True $ takeDirectory outFile
      encodeFile outFile val

diff :: String -> String -> [String]
diff ref new = ["colordiff", "-u", ref, new]
