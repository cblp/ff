{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FF
  ( cmdDeleteContact,
    cmdDeleteNote,
    cmdDone,
    cmdEdit,
    cmdNewContact,
    cmdNewNote,
    cmdPostpone,
    cmdSearch,
    cmdUnarchive,
    fromRga,
    fromRgaM,
    getContactSamples,
    getDataDir,
    getTaskSamples,
    getUtcToday,
    getWikiSamples,
    loadTasks,
    noDataDirectoryMessage,
    splitModes,
    takeSamples,
    updateTrackedNotes
    )
where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, evalState, state)
import Data.Bool (bool)
import Data.Foldable (asum, for_, toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (genericLength, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
  ( Day,
    addDays,
    fromGregorian,
    getCurrentTime,
    toModifiedJulianDay,
    utctDay
    )
import Data.Traversable (for)
import FF.Config (Config (Config), ConfigUI (ConfigUI), dataDir, shuffle)
import FF.Options (Edit (..), New (..), maybeClearToMaybe)
import FF.Types
  ( Contact (..),
    ContactId,
    ContactSample,
    Entity (..),
    Limit,
    ModeMap,
    Note (..),
    NoteId,
    NoteSample,
    NoteStatus (..),
    Sample (..),
    Status (..),
    Track (..),
    contact_name_zoom,
    contact_status_assign,
    emptySample,
    loadNote,
    note_end_assign,
    note_end_read,
    note_start_assign,
    note_start_read,
    note_status_assign,
    note_status_read,
    note_text_zoom,
    note_track_read,
    taskMode
    )
import RON.Data
  ( MonadObjectState,
    ObjectStateT,
    evalObjectState,
    getObject,
    newObjectFrame,
    runObjectState
    )
import RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import RON.Error (MonadE, throwErrorText)
import RON.Event (ReplicaClock)
import RON.Storage
  ( Collection,
    DocId,
    createDocument,
    docIdFromUuid,
    loadDocument
    )
import RON.Storage.Backend
  ( Document (Document, objectFrame),
    MonadStorage (getDocuments),
    createVersion
    )
import RON.Types (ObjectFrame (ObjectFrame, uuid))
import System.Directory
  ( doesDirectoryExist,
    findExecutable,
    getCurrentDirectory
    )
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), normalise, splitDirectories)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process.Typed (proc, runProcess)
import System.Random (StdGen, mkStdGen, randoms, split)

load :: (Collection a, MonadStorage m) => DocId a -> m (Entity a)
load docid = do
  Document {objectFrame} <- loadDocument docid
  entityVal <- evalObjectState objectFrame getObject
  pure $ Entity docid entityVal

loadAll :: (Collection a, MonadStorage m) => m [Entity a]
loadAll = getDocuments >>= traverse load

loadAllNotes :: MonadStorage m => m [Entity Note]
loadAllNotes = getDocuments >>= traverse loadNote

searchStatus :: Bool -> Status
searchStatus = bool Active Archived

loadContacts :: MonadStorage m => Bool -> m [Entity Contact]
loadContacts isArchived =
  filter ((== Just (searchStatus isArchived)) . contact_status . entityVal)
    <$> loadAll

getContactSamples :: MonadStorage m => Bool -> m ContactSample
getContactSamples = getContactSamplesWith $ const True

getContactSamplesWith
  :: MonadStorage m
  => (Text -> Bool) -- ^ predicate to filter contacts by text
  -> Bool -- ^ search within archived contacts
  -> m ContactSample
getContactSamplesWith predicate isArchived = do
  contacts <- loadContacts isArchived
  pure . (\ys -> Sample ys $ genericLength ys) $ filter predicate' contacts
  where
    predicate' = predicate . Text.pack . fromRgaM . contact_name . entityVal

fromRga :: RGA a -> [a]
fromRga (RGA xs) = xs

fromRgaM :: Maybe (RGA a) -> [a]
fromRgaM = maybe [] fromRga

loadTasks :: MonadStorage m => Bool -> m [Entity Note]
loadTasks isArchived = filter isArchived' <$> loadAllNotes
  where
    isArchived' =
      (Just (TaskStatus $ searchStatus isArchived) ==) . note_status . entityVal

loadWikis :: MonadStorage m => m [Entity Note]
loadWikis = filter ((Just Wiki ==) . note_status . entityVal) <$> loadAllNotes

getTaskSamples
  :: MonadStorage m
  => Bool -- ^ search within archived tasks
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> m (ModeMap NoteSample)
getTaskSamples = getTaskSamplesWith $ const True

getTaskSamplesWith
  :: MonadStorage m
  => (Text -> Bool) -- ^ predicate to filter notes by text
  -> Bool -- ^ search within archived tasks
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> m (ModeMap NoteSample)
getTaskSamplesWith predicate isArchived ConfigUI {shuffle} limit today = do
  tasks <- loadTasks isArchived
  pure
    . takeSamples limit
    . shuffleOrSort
    . splitModesBy entityVal today
    $ filter (predicate . Text.pack . fromRgaM . note_text . entityVal) tasks
  where
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
      | shuffle = shuffleTraverseItems gen
      | otherwise =
        -- in sorting by entityId no business-logic is involved,
        -- it's just for determinism
        fmap $ sortOn $ note_start . entityVal &&& entityId

getWikiSamples
  :: MonadStorage m
  => Bool -- archived search
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> m NoteSample
getWikiSamples = getWikiSamplesWith $ const True

getWikiSamplesWith
  :: MonadStorage m
  => (Text -> Bool) -- ^ predicate to filter tasks by text
  -> Bool -- ^ if archived search, return Nothing
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> m NoteSample
getWikiSamplesWith predicate archive ConfigUI {shuffle} limit today =
  if archive then
    pure emptySample
  else
    do
      wikis0 <- loadWikis
      let wikis1 = filter predicate' wikis0
      let wikis2 = case limit of
            Nothing -> wikis1
            Just l  -> take (fromIntegral l) wikis1
      pure . toSample $ shuffleOrSort wikis2
  where
    predicate' = predicate . Text.pack . fromRgaM . note_text . entityVal
    toSample ys = Sample ys $ genericLength ys
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
      | shuffle = shuffleItems gen
      | otherwise =
        -- in sorting by entityId no business-logic is involved,
        -- it's just for determinism
        sortOn entityId

shuffleItems :: StdGen -> [b] -> [b]
shuffleItems gen = (`evalState` gen) . shuf

shuf :: MonadState StdGen m => [b] -> m [b]
shuf xs = do
  g <- state split
  pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

shuffleTraverseItems :: Traversable t => StdGen -> t [b] -> t [b]
shuffleTraverseItems gen = (`evalState` gen) . traverse shuf

splitModesBy :: (note -> Note) -> Day -> [note] -> ModeMap [note]
splitModesBy f today = Map.unionsWith (++) . map singleton
  where
    singleton task = Map.singleton (taskMode today $ f task) [task]

splitModes :: Day -> [Note] -> ModeMap [Note]
splitModes = splitModesBy identity

takeSamples :: Maybe Limit -> ModeMap [a] -> ModeMap (Sample a)
takeSamples Nothing = fmap mkSample
  where
    mkSample ys = Sample ys $ genericLength ys
takeSamples (Just limit) = (`evalState` limit) . traverse takeSample
  where
    takeSample xs =
      state $ \n -> (Sample (take (fromIntegral n) xs) len, n `natSub` len)
      where
        len = genericLength xs
    natSub a b
      | a <= b    = 0
      | otherwise = a - b

updateTrackedNote
  :: MonadStorage m
  => HashMap Track NoteId -- ^ selection of all aready tracked notes
  -> Note -- ^ external note to insert
  -> m ()
updateTrackedNote oldNotes note = case note of
  Note {note_track = Just track} -> case HashMap.lookup track oldNotes of
    Nothing -> do
      obj <- newObjectFrame note
      createDocument obj
    Just noteid -> void $ modify noteid $ do
      note_status_assignIfDiffer note_status
      note_text_zoom $ RGA.edit text
  _ -> throwError "External note is expected to be supplied with tracking"
  where
    Note {note_status, note_text = (fromRgaM -> text)} = note

updateTrackedNotes :: MonadStorage m => [Note] -> m ()
updateTrackedNotes newNotes = do
  -- TODO(2018-10-22, cblp) index notes by track in the database and select
  -- specific note by its track
  notes <- getDocuments
  oldNotesM <-
    for notes $ \noteId -> do
      Document {objectFrame} <- loadDocument noteId
      mTrack <- evalObjectState objectFrame note_track_read
      pure $ (,noteId) <$> mTrack
  let oldNotes = HashMap.fromList $ catMaybes oldNotesM
  for_ newNotes $ updateTrackedNote oldNotes

cmdNewNote :: MonadStorage m => New -> Day -> m (Entity Note)
cmdNewNote New {text, start, end, isWiki} today = do
  let start' = fromMaybe today start
  whenJust end $ assertStartBeforeEnd start'
  (status, note_end, noteStart) <-
    case end of
      _ | not isWiki -> pure (TaskStatus Active, end, start')
      Nothing -> pure (Wiki, Nothing, today)
      Just _ -> throwError "A wiki must have no end date."
  let note = Note
        { note_end,
          note_start  = Just noteStart,
          note_status = Just status,
          note_text   = Just $ RGA $ Text.unpack text,
          note_track  = Nothing
          }
  obj@ObjectFrame {uuid} <- newObjectFrame note
  createDocument obj
  pure $ Entity (docIdFromUuid uuid) note

cmdNewContact :: MonadStorage m => Text -> m (Entity Contact)
cmdNewContact name = do
  let contact =
        Contact
          { contact_name   = Just $ RGA $ Text.unpack name,
            contact_status = Just Active
            }
  obj@ObjectFrame {uuid} <- newObjectFrame contact
  createDocument obj
  pure $ Entity (docIdFromUuid uuid) contact

cmdDeleteContact :: MonadStorage m => ContactId -> m (Entity Contact)
cmdDeleteContact cid = modifyAndView cid $ do
  contact_status_assign $ Just Deleted
  contact_name_zoom $ RGA.editText ""

cmdSearch
  :: MonadStorage m
  => Text -- ^ query
  -> Bool -- ^ search within archived tasks or contacts
  -> ConfigUI
  -> Maybe Limit
  -> Day -- ^ today
  -> m (ModeMap NoteSample, NoteSample, ContactSample)
cmdSearch substr archive ui limit today = do
  -- TODO(cblp, 2018-12-21) search tasks and wikis in one step
  tasks <- getTaskSamplesWith predicate archive ui limit today
  wikis <- getWikiSamplesWith predicate archive ui limit today
  contacts <- getContactSamplesWith predicate archive
  pure (tasks, wikis, contacts)
  where
    predicate = Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold

cmdDeleteNote :: MonadStorage m => NoteId -> m (Entity Note)
cmdDeleteNote nid = modifyAndView nid $ do
  assertNoteIsNative
  note_status_assign $ Just $ TaskStatus Deleted
  note_text_zoom $ RGA.editText ""
  note_start_assign $ Just $ fromGregorian 0 1 1
  note_end_assign Nothing

cmdDone :: MonadStorage m => NoteId -> m (Entity Note)
cmdDone nid = modifyAndView nid $ do
  assertNoteIsNative
  note_status_assign $ Just $ TaskStatus Archived

cmdUnarchive :: MonadStorage m => NoteId -> m (Entity Note)
cmdUnarchive nid =
  modifyAndView nid $ note_status_assign $ Just $ TaskStatus Active

cmdEdit :: (MonadIO m, MonadStorage m) => Edit -> m [Entity Note]
cmdEdit edit = case edit of
  Edit {ids = _ :| _ : _, text = Just _} ->
    throwError "Can't edit content of multiple notes"
  Edit {ids = nid :| [], text, start = Nothing, end = Nothing} ->
    fmap (: []) $ modifyAndView nid $ do
      assertNoteIsNative
      note_text_zoom $ do
        noteText' <-
          case text of
            Just noteText' -> pure noteText'
            Nothing -> do
              noteText <- RGA.getText
              liftIO $ runExternalEditor noteText
        RGA.editText noteText'
  Edit {ids, text, start, end} ->
    fmap toList . for ids $ \nid ->
      modifyAndView nid $ do
        -- check text editability
        whenJust text $ const assertNoteIsNative
        -- check start and end editability
        when (isJust start || isJust end) $ do
          status <- note_status_read
          when (status == Just Wiki)
            $ throwError "Wiki dates are immutable"
        -- check start and end relation
        do
          curStart <- note_start_read
          curEnd   <- note_end_read
          let newStartEnd =
                (,)
                  <$> (start <|> curStart)
                  <*> (end'  <|> curEnd)
              end' = end >>= maybeClearToMaybe
          whenJust newStartEnd
            $ uncurry assertStartBeforeEnd
        -- update
        whenJust end   $ note_end_assign   . maybeClearToMaybe
        whenJust start $ note_start_assign . Just
        whenJust text  $ note_text_zoom    . RGA.editText

cmdPostpone :: (MonadIO m, MonadStorage m) => NoteId -> m (Entity Note)
cmdPostpone nid = modifyAndView nid $ do
  today <- getUtcToday
  start <- note_start_read
  let start' = addDays 1 $ maybe today (max today) start
  note_start_assign $ Just start'
  mEnd <- note_end_read
  case mEnd of
    Just end | end < start' -> note_end_assign $ Just start'
    _ -> pure ()

-- | Load document, apply changes and put it back to storage
-- TODO(2019-07-26, cblp) put it back to RON.Storage
modify
  :: (Collection a, MonadStorage m)
  => DocId a
  -> ObjectStateT a m b
  -> m b
modify docid f = do
  oldDoc <- loadDocument docid
  (b, objectFrame') <- runObjectState (objectFrame oldDoc) f
  createVersion (Just (docid, oldDoc)) objectFrame'
  pure b

modifyAndView
  :: (Collection a, MonadStorage m)
  => DocId a
  -> ObjectStateT a m ()
  -> m (Entity a)
modifyAndView docid f = do
  entityVal <-
    modify docid $ do
      f
      getObject
  pure $ Entity docid entityVal

getUtcToday :: MonadIO io => io Day
getUtcToday = liftIO $ utctDay <$> getCurrentTime

runExternalEditor :: Text -> IO Text
runExternalEditor textOld = do
  editor <-
    asum
      $ assertExecutableFromEnv "EDITOR"
      : map assertExecutable ["editor", "micro", "nano"]
  withSystemTempFile "ff.edit" $ \file fileH -> do
    Text.hPutStr fileH textOld
    hClose fileH
    runProcess (proc editor [file]) >>= \case
      ExitSuccess -> Text.strip <$> Text.readFile file
      ExitFailure {} -> pure textOld
  where
    assertExecutable prog = do
      Just _ <- findExecutable prog
      pure prog
    assertExecutableFromEnv param = assertExecutable =<< getEnv param

assertStartBeforeEnd :: MonadE m => Day -> Day -> m ()
assertStartBeforeEnd start end =
  unless (start <= end) $ throwError "task cannot end before it is started"

note_status_assignIfDiffer
  :: (ReplicaClock m, MonadE m, MonadObjectState Note m)
  => Maybe NoteStatus
  -> m ()
note_status_assignIfDiffer newStatus = do
  curStatus <- note_status_read
  when (curStatus /= newStatus)
    $ note_status_assign newStatus

assertNoteIsNative :: (MonadE m, MonadObjectState Note m) => m ()
assertNoteIsNative = do
  -- TODO(2018-10-22, cblp) use `case of some/none` without full decoding of
  -- `some`
  tracking <- note_track_read
  whenJust tracking $ \Track {track_url} ->
    throwErrorText
      $ "A tracked note must be edited in its source"
      <> maybe "" (" :" <>) track_url

getDataDir :: Config -> IO (Maybe FilePath)
getDataDir Config {dataDir} = do
  cur <- getCurrentDirectory
  mDataDirFromVcs <- findVcs $ parents cur
  pure $ mDataDirFromVcs <|> dataDir
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise
    findVcs [] = pure Nothing
    findVcs (dir : dirs) = do
      isDirVcs <- doesDirectoryExist (dir </> ".git")
      if isDirVcs then
        pure . Just $ dir </> ".ff"
      else
        findVcs dirs

noDataDirectoryMessage :: String
noDataDirectoryMessage =
  "Data directory isn't set, run `ff config dataDir --help`"

identity :: a -> a
identity x = x

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Nothing -> pure ()
  Just x -> f x
