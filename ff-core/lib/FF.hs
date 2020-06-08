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
    viewTaskSamples,
    getUtcToday,
    viewWikiSamples,
    load,
    filterTasksByStatus,
    loadAll,
    loadAllTagTexts,
    loadAllNotes,
    loadTagsByRefs,
    noDataDirectoryMessage,
    splitModes,
    sponsors,
    takeSamples,
    updateTrackedNotes,
    viewNote,
    viewNoteSample,
  )
where

import           Control.Applicative (empty, (<|>))
import           Control.Monad (unless, void, when)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (MonadState, evalState, state)
import qualified Data.ByteString as BS
import           Data.Foldable (asum, for_, toList, traverse_)
import           Data.Functor (($>))
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List (genericLength, sortOn)
import           Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import           Data.Set (Set, disjoint, isSubsetOf, (\\))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import           Data.Time (Day, addDays, getCurrentTime, toModifiedJulianDay,
                            utctDay)
import           Data.Traversable (for)
import           RON.Data (MonadObjectState, ObjectStateT, evalObjectState,
                           newObjectFrame, readObject)
import           RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import           RON.Error (MonadE, throwErrorText)
import           RON.Event (ReplicaClock)
import           RON.Storage (Collection, DocId, createDocument, decodeDocId,
                              docIdFromUuid, loadDocument, modify)
import           RON.Storage.Backend (Document (Document, objectFrame),
                                      MonadStorage (getDocuments))
import           RON.Types (ObjectFrame (ObjectFrame, uuid),
                            ObjectRef (ObjectRef))
import qualified ShellWords
import           System.Directory (doesDirectoryExist, findExecutable,
                                   getCurrentDirectory)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.FilePath (normalise, splitDirectories, (</>))
import           System.IO (hClose, hPutStrLn, stderr)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)
import           System.Random (StdGen, mkStdGen, randoms, split)

import           FF.Config (Config (Config), ConfigUI (ConfigUI), dataDir,
                            externalEditor, loadConfig, shuffle)
import           FF.Options (Assign (Clear, Set), Edit (..), New (..),
                             Tags (..), assignToMaybe)
import           FF.Types (Contact (..), ContactId, ContactSample, Entity (..),
                           EntityDoc, EntityView, Limit, ModeMap, Note (..),
                           NoteId, NoteSample, NoteStatus (..), Sample (..),
                           Status (..), Tag (..), Track (..), View (..),
                           contact_name_clear, contact_status_clear, loadNote,
                           note_end_clear, note_end_read, note_end_set,
                           note_start_clear, note_start_read, note_start_set,
                           note_status_clear, note_status_read, note_status_set,
                           note_tags_add, note_tags_clear, note_tags_read,
                           note_tags_remove, note_text_clear, note_text_zoom,
                           note_track_read, taskMode, uuidToText)

load :: (Collection a, MonadStorage m) => DocId a -> m (EntityDoc a)
load docid = do
  Document{objectFrame} <- loadDocument docid
  entityVal <- evalObjectState objectFrame readObject
  pure Entity{entityId = docid, entityVal}

loadAll :: (Collection a, MonadStorage m) => m [EntityDoc a]
loadAll = getDocuments >>= traverse load

loadAllNotes :: MonadStorage m => m [EntityDoc Note]
loadAllNotes = getDocuments >>= traverse loadNote

loadContacts :: MonadStorage m => Status -> m [EntityDoc Contact]
loadContacts status =
  filter ((== Just status) . contact_status . entityVal) <$> loadAll

-- | Load all tags as texts
loadAllTagTexts :: MonadStorage m => m (Set Text)
loadAllTagTexts = Set.fromList . mapMaybe (tag_text . entityVal) <$> loadAll

-- | Load 'Tag' references only for text strings existing in the collection.
loadTagRefsByText ::
  (Foldable f, MonadStorage m) => f Text -> m (HashSet (ObjectRef Tag))
loadTagRefsByText queryTags = do
  allTags <- loadAll
  pure $
    HashSet.fromList
      [ docIdToRef entityId
      | Entity{entityId, entityVal = Tag{tag_text = Just tag}} <- allTags
      , tag `elem` queryTags
      ]

loadTagsByRefs ::
  MonadStorage m => HashSet (ObjectRef Tag) -> m (HashMap (ObjectRef Tag) Text)
loadTagsByRefs refs =
  fmap (HashMap.fromList . catMaybes) $
  for (toList refs) $ \ref ->
    fmap (ref,) . tag_text . entityVal <$> load (refToDocId ref)

-- | Create tag objects with given texts.
createTags :: MonadStorage m => Set Text -> m (HashSet (ObjectRef Tag))
createTags tags =
  fmap HashSet.fromList $
  for (toList tags) $ \tag -> do
    tagFrame@ObjectFrame{uuid} <- newObjectFrame Tag{tag_text = Just tag}
    createDocument tagFrame
    pure $ ObjectRef uuid

-- | Add new tags to Collection of tags.
--
-- It doesn't create tags that are in the collection already.
-- It returns references that should be added to note_tags.
-- References may content ones that note_tags has already.
getOrCreateTags ::
  (Foldable f, MonadStorage m) => f Text -> m (HashSet (ObjectRef Tag))
getOrCreateTags tags
  | null tags = pure HashSet.empty
  | otherwise = do
      allTags <- loadAllTagTexts
      existentTagRefs <- loadTagRefsByText tags
      let newTags = Set.fromList (toList tags) \\ allTags
      createdTagRefs <- createTags newTags
      pure $ existentTagRefs <> createdTagRefs

viewNote :: MonadStorage m => EntityDoc Note -> m (EntityView Note)
viewNote Entity{entityId, entityVal} =
  do
    tagsLoaded <- loadTagsByRefs tagRefs
    let
      tags =
        HashMap.fromList
          [ (uuidToText uuid, tag)
          | (ObjectRef uuid, tag) <- HashMap.toList tagsLoaded
          ]
    pure Entity{entityId, entityVal = NoteView{note = entityVal, tags}}
  where
    tagRefs = HashSet.fromList note_tags
    Note{note_tags} = entityVal

viewNoteSample :: MonadStorage m => Sample (EntityDoc Note) -> m NoteSample
viewNoteSample Sample{items, total} = do
  noteviews <- mapM viewNote items
  pure $ Sample noteviews total

getContactSamples :: MonadStorage m => Status -> m ContactSample
getContactSamples = getContactSamplesWith $ const True

getContactSamplesWith ::
  MonadStorage m =>
  -- | predicate to filter contacts by text
  (Text -> Bool) ->
  -- | filter by status
  Status ->
  m ContactSample
getContactSamplesWith predicate status =
  do
    contacts <- loadContacts status
    pure . (\ys -> Sample ys $ genericLength ys) $ filter predicate' contacts
  where
    predicate' = predicate . Text.pack . fromRgaM . contact_name . entityVal

fromRga :: RGA a -> [a]
fromRga (RGA xs) = xs

fromRgaM :: Maybe (RGA a) -> [a]
fromRgaM = maybe [] fromRga

filterTasksByStatus :: Status -> [EntityDoc Note] -> [EntityDoc Note]
filterTasksByStatus status =
  filter $ (Just (TaskStatus status) ==) . note_status . entityVal

filterWikis :: [EntityDoc Note] -> [EntityDoc Note]
filterWikis = filter ((Just Wiki ==) . note_status . entityVal)

viewTaskSamples ::
  MonadStorage m =>
  -- | filter by status
  Status ->
  ConfigUI ->
  Maybe Limit ->
  -- | today
  Day ->
  -- | requested tags
  Tags ->
  -- | without tags
  Set Text ->
  [EntityDoc Note] ->
  m (ModeMap NoteSample)
viewTaskSamples = viewTaskSamplesWith $ const True

viewTaskSamplesWith ::
  MonadStorage m =>
  -- | predicate to filter notes by text
  (Text -> Bool) ->
  -- | filter status
  Status ->
  ConfigUI ->
  Maybe Limit ->
  -- | today
  Day ->
  -- | requested tags
  Tags ->
  -- | without tags
  Set Text ->
  [EntityDoc Note] ->
  m (ModeMap NoteSample)
viewTaskSamplesWith
    textPredicate
    status
    ConfigUI{shuffle}
    limit
    today
    tagsRequested
    withoutTags
    notes
  =
    do
      -- filter unrefined tasks
      let notes' = filter notePredicate $ filterTasksByStatus status notes
      -- refine tasks
      tasks <- traverse viewNote notes'
      -- filter refined tasks
      let tasks' = filter noteViewPredicate tasks
      -- prepare result
      pure . takeSamples limit . shuffleOrSort $ splitModes today tasks'
    where

      gen = mkStdGen . fromIntegral $ toModifiedJulianDay today

      shuffleOrSort
        | shuffle = shuffleTraverseItems gen
        | otherwise =
            -- in sorting by entityId no business-logic is involved,
            -- it's just for determinism
            fmap $
            sortOn $
              \Entity{entityId, entityVal = NoteView{note = Note{note_start}}}
              ->
                (note_start, entityId)

      notePredicate Entity{entityVal = Note{note_text}} =
        textPredicate $ Text.pack $ fromRgaM note_text

      tagPredicate tags = case tagsRequested of
        Tags tagsRequested' ->
          tagsRequested' `isSubsetOf` tags && withoutTags `disjoint` tags
        NoTags -> null tags

      noteViewPredicate Entity{entityVal = NoteView{tags}} =
        tagPredicate $ Set.fromList $ toList tags

viewWikiSamples ::
  MonadStorage m =>
  ConfigUI ->
  Maybe Limit ->
  -- | today
  Day ->
  [EntityDoc Note] ->
  m NoteSample
viewWikiSamples = toWikiSamplesWith $ const True

toWikiSamplesWith ::
  MonadStorage m =>
  -- | predicate to filter tasks by text
  (Text -> Bool) ->
  ConfigUI ->
  Maybe Limit ->
  -- | today
  Day ->
  [EntityDoc Note] ->
  m NoteSample
toWikiSamplesWith predicate ConfigUI{shuffle} limit today notes =
  viewNoteSample $ toSample $ shuffleOrSort wikis2
  where
    wikis0 = filterWikis notes
    wikis1 = filter predicate' wikis0
    wikis2 = case limit of
      Nothing -> wikis1
      Just l -> take (fromIntegral l) wikis1

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
splitModesBy f today = Map.unionsWith (++) . map singleton where
  singleton task = Map.singleton (taskMode today $ f task) [task]

splitModes :: Day -> [EntityView Note] -> ModeMap [EntityView Note]
splitModes = splitModesBy (note . entityVal)

takeSamples :: Maybe Limit -> ModeMap [a] -> ModeMap (Sample a)
takeSamples Nothing =
  fmap mkSample
  where
    mkSample ys = Sample ys $ genericLength ys
takeSamples (Just limit) =
  (`evalState` limit) . traverse takeSample
  where
    takeSample xs =
      state $ \n -> (Sample (take (fromIntegral n) xs) len, n `natSub` len)
      where
        len = genericLength xs
    natSub a b
      | a <= b = 0
      | otherwise = a - b

updateTrackedNote ::
  MonadStorage m =>
  -- | selection of all aready tracked notes
  HashMap Track NoteId ->
  -- | external note (with tags) to insert
  View Note ->
  m ()
updateTrackedNote oldNotes NoteView{note, tags} =
  case note_track of
    Just track -> do
      newRefs <- getOrCreateTags tags
      case HashMap.lookup track oldNotes of
        Nothing -> do
          obj <- newObjectFrame note{note_tags = toList newRefs}
          createDocument obj
        Just noteid ->
          void $ modify noteid $ do
            note_status_setIfDiffer note_status
            note_text_zoom $ RGA.edit text
            -- Add new tags
            currentRefs <- HashSet.fromList <$> note_tags_read
            traverse_ note_tags_add $ newRefs \- currentRefs
            traverse_ note_tags_remove $ currentRefs \- newRefs
    Nothing ->
      throwError "External note is expected to be supplied with tracking"
  where
    Note{note_status, note_text = (fromRgaM -> text), note_track} = note

updateTrackedNotes :: MonadStorage m => [View Note] -> m ()
updateTrackedNotes newNotes = do
  -- TODO(2018-10-22, https://github.com/ff-notes/ron/issues/116, cblp) index
  -- notes by track in the database and select specific note by its track
  notes <- getDocuments
  oldNotesM <-
    for notes $ \noteId -> do
      Document{objectFrame} <- loadDocument noteId
      mTrack <- evalObjectState objectFrame note_track_read
      pure $ (,noteId) <$> mTrack
  let oldNotes = HashMap.fromList $ catMaybes oldNotesM
  for_ newNotes $ updateTrackedNote oldNotes

cmdNewNote :: MonadStorage m => New -> Day -> m (EntityDoc Note)
cmdNewNote New{text, start, end, isWiki, tags} today = do
  let start' = fromMaybe today start
  whenJust end $ assertStartBeforeEnd start'
  (status, note_end, noteStart) <-
    case end of
      _ | not isWiki -> pure (TaskStatus Active, end, start')
      Nothing -> pure (Wiki, Nothing, today)
      Just _ -> throwError "A wiki must have no end date."
  refs <- getOrCreateTags tags
  let note = Note
        { note_end
        , note_start  = Just noteStart
        , note_status = Just status
        , note_text   = Just $ RGA $ Text.unpack text
        , note_tags   = toList refs
        , note_track  = Nothing
        , note_links  = []
        }
  obj@ObjectFrame{uuid} <- newObjectFrame note
  createDocument obj
  pure $ Entity (docIdFromUuid uuid) note

cmdNewContact :: MonadStorage m => Text -> m (EntityDoc Contact)
cmdNewContact name = do
  let contact =
        Contact
          { contact_name = Just $ RGA $ Text.unpack name,
            contact_status = Just Active
          }
  obj@ObjectFrame{uuid} <- newObjectFrame contact
  createDocument obj
  pure $ Entity (docIdFromUuid uuid) contact

cmdDeleteContact :: MonadStorage m => ContactId -> m (EntityDoc Contact)
cmdDeleteContact cid =
  modifyAndView cid $ do
    contact_status_clear
    contact_name_clear

cmdSearch ::
  MonadStorage m =>
  -- | query
  Text ->
  -- | search within archived tasks or contacts
  Status ->
  ConfigUI ->
  Maybe Limit ->
  -- | today
  Day ->
  -- | requested tags
  Tags ->
  -- | without tags
  Set Text ->
  m (ModeMap NoteSample, NoteSample, ContactSample)
cmdSearch substr status ui limit today tags withoutTags =
  do
    notes <- loadAllNotes
    tasks <-
      viewTaskSamplesWith predicate status ui limit today tags withoutTags notes
    wikis    <- toWikiSamplesWith predicate ui limit today notes
    contacts <- getContactSamplesWith predicate status
    pure (tasks, wikis, contacts)
  where
    predicate = Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold

cmdDeleteNote :: MonadStorage m => NoteId -> m (EntityDoc Note)
cmdDeleteNote nid =
  modifyAndView nid $ do
    assertNoteIsNative
    note_status_clear
    note_text_clear
    note_start_clear
    note_end_clear
    note_tags_clear

cmdDone :: MonadStorage m => NoteId -> m (EntityDoc Note)
cmdDone nid = modifyAndView nid $ do
  assertNoteIsNative
  note_status_set $ TaskStatus Archived

cmdUnarchive :: MonadStorage m => NoteId -> m (EntityDoc Note)
cmdUnarchive nid =
  modifyAndView nid $ note_status_set $ TaskStatus Active

cmdEdit :: (MonadIO m, MonadStorage m) => Edit -> m [EntityDoc Note]
cmdEdit edit = case edit of
  Edit{ids = _ :| _ : _, text = Just _} ->
    throwError "Can't edit content of multiple notes"
  Edit{ ids = nid :| []
      , text
      , start = Nothing
      , end = Nothing
      , addTags
      , deleteTags
      }
    | null addTags, null deleteTags ->
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
  Edit{ids, text, start, end, addTags, deleteTags} -> do
    refsToAdd <- getOrCreateTags addTags
    refsToDelete <- loadTagRefsByText deleteTags
    fmap toList . for ids $ \nid ->
      modifyAndView nid $ do
        -- check text editability
        whenJust text $ const assertNoteIsNative
        -- check start and end editability
        when (isJust start || isJust end) $ do
          status <- note_status_read
          when (status == Just Wiki) $
            throwError "Wiki dates are immutable"
        -- check start and end relation
        do
          curStart <- note_start_read
          curEnd <- note_end_read
          let newStartEnd =
                (,)
                  <$> (start <|> curStart)
                  <*> (end' <|> curEnd)
              end' = end >>= assignToMaybe
          whenJust newStartEnd $
            uncurry assertStartBeforeEnd
        -- update
        whenJust end $ \case
          Clear -> note_end_clear
          Set e -> note_end_set e
        whenJust start note_start_set
        whenJust text $ note_text_zoom . RGA.editText
        -- add new tags
        unless (null addTags) $ do
          currentRefs <- HashSet.fromList <$> note_tags_read
          -- skip tags if note_tags has them already
          let newRefs = refsToAdd \- currentRefs
          mapM_ note_tags_add newRefs
        -- delete tags
        unless (null deleteTags) $
          mapM_ note_tags_remove refsToDelete

cmdPostpone :: (MonadIO m, MonadStorage m) => NoteId -> m (EntityDoc Note)
cmdPostpone nid =
  modifyAndView nid $ do
    today <- getUtcToday
    start <- note_start_read
    let start' = addDays 1 $ maybe today (max today) start
    note_start_set start'
    mEnd <- note_end_read
    case mEnd of
      Just end | end < start' -> note_end_set start'
      _                       -> pure ()

modifyAndView ::
  (Collection a, MonadStorage m) =>
  DocId a ->
  ObjectStateT a m () ->
  m (EntityDoc a)
modifyAndView docid f = do
  entityVal <-
    modify docid $ do
      f
      readObject
  pure $ Entity docid entityVal

getUtcToday :: MonadIO io => io Day
getUtcToday = liftIO $ utctDay <$> getCurrentTime

runExternalEditor :: Text -> IO Text
runExternalEditor textOld =
  do
    editor :| editorArgs <-
      asum $
          assertExecutableFromConfig
        : assertExecutableFromEnv "VISUAL"
        : assertExecutableFromEnv "EDITOR"
        : map assertExecutable ["editor", "micro", "nano", "vi", "vim"]
    withSystemTempFile "ff.txt" $ \file fileH -> do
      BS.hPutStr fileH $ Text.encodeUtf8 textOld
      hClose fileH
      hPutStrLn stderr "waiting for external editor to close"
      runProcess (proc editor $ editorArgs ++ [file]) >>= \case
        ExitSuccess ->
          Text.strip . Text.decodeUtf8With TextError.ignore <$> BS.readFile file
        ExitFailure{} -> pure textOld
  where
    assertExecutable prog = do
      Just _ <- findExecutable prog
      pure $ prog :| []

    assertExecutableFromConfig = do
      cfg <- loadConfig
      maybe empty assertExecutable $ externalEditor cfg

    assertExecutableFromEnv var = do
      editorCmd <- getEnv var
      let
        eEditor = do
          editor <- ShellWords.parse editorCmd
          maybe (Left "empty") Right $ nonEmpty editor
      case eEditor of
        Left err -> do
          hPutStrLn stderr $ "error in $EDITOR environment variable: " <> err
          empty
        Right editor@(prog :| _) -> assertExecutable prog $> editor

assertStartBeforeEnd :: MonadE m => Day -> Day -> m ()
assertStartBeforeEnd start end =
  unless (start <= end) $ throwError "task cannot end before it is started"

note_status_setIfDiffer ::
  (ReplicaClock m, MonadE m, MonadObjectState Note m) =>
  Maybe NoteStatus ->
  m ()
note_status_setIfDiffer newStatus = do
  curStatus <- note_status_read
  when (curStatus /= newStatus) $
    maybe note_status_clear note_status_set newStatus

assertNoteIsNative :: (MonadE m, MonadObjectState Note m) => m ()
assertNoteIsNative = do
  tracking <- note_track_read
  whenJust tracking $ \Track{track_url} ->
    throwErrorText $
          "A tracked note must be edited in its source"
      <>  maybe "" (" :" <>) track_url

getDataDir :: Config -> IO (Maybe FilePath)
getDataDir Config{dataDir} =
  do
    cur <- getCurrentDirectory
    mFFpath <- findFF $ parents cur
    pure $ mFFpath <|> dataDir
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise

    findFF [] = pure Nothing
    findFF (dir : dirs) = do
      let ffDir = dir </> ".ff"
      isFFdir <- doesDirectoryExist ffDir
      if isFFdir
        then pure $ Just ffDir
        else findFF dirs

noDataDirectoryMessage :: String
noDataDirectoryMessage =
  "Data directory isn't set, run `ff config dataDir --help`"

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Nothing -> pure ()
  Just x -> f x

sponsors :: [Text]
sponsors = ["Alexander Granin", "Nadezda"]

refToDocId :: ObjectRef a -> DocId a
refToDocId (ObjectRef uid) = docIdFromUuid uid

docIdToRef :: DocId a -> ObjectRef a
docIdToRef docId =
  case decodeDocId docId of
    Nothing       -> error "Decode UUID from DocId failed. DocId is "
    Just (_, uid) -> ObjectRef uid

(\-) :: (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
(\-) = HashSet.difference
