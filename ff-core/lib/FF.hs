{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module FF (
    cmdDeleteNote,
    cmdDeleteContact,
    cmdDone,
    cmdEdit,
    cmdNewNote,
    cmdNewContact,
    cmdPostpone,
    cmdSearch,
    cmdShow,
    cmdUnarchive,
    getContactSamples,
    getDataDir,
    getTaskSamples,
    getUtcToday,
    getWikiSamples,
    load,
    loadActiveTasks,
    loadAll,
    splitModes,
    takeSamples,
    updateTrackedNotes,
) where

import           Prelude hiding (id)

import           Control.Applicative ((<|>))
import           Control.Arrow ((&&&))
import           Control.Monad (unless, void, when)
import           Control.Monad.Except (liftEither, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (MonadState, StateT, evalState,
                                             evalStateT, gets, state)
import           Data.Bool (bool)
import           Data.Foldable (asum, for_, toList)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (genericLength, sortOn)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, fromGregorian, getCurrentTime,
                            toModifiedJulianDay, utctDay)
import           Data.Traversable (for)
import           RON.Data (getObject, newObject)
import qualified RON.Data.RGA as RGA
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock)
import           RON.Storage (Collection, DocId (..), Document (..),
                              MonadStorage, createDocument, docIdFromUuid,
                              getDocuments, loadDocument, modify)
import           RON.Storage.IO (Storage)
import           RON.Types (Object (Object, id))
import           System.Directory (doesDirectoryExist, findExecutable,
                                   getCurrentDirectory)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.FilePath (normalise, splitDirectories, (</>))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)
import           System.Random (StdGen, mkStdGen, randoms, split)

import           FF.Config (Config (Config), ConfigUI (ConfigUI), dataDir,
                            shuffle)
import           FF.Options (Edit (..), New (..), maybeClearToMaybe)
import           FF.Types (Contact (..), ContactId, ContactSample, Entity (..),
                           Limit, ModeMap, Note (..), NoteId, NoteSample,
                           NoteStatus (..), Sample (..), Status (..), Track,
                           contact_name_zoom, contact_status_assign,
                           note_end_assign, note_end_read, note_start_assign,
                           note_start_read, note_status_assign,
                           note_status_read, note_text_zoom, note_track_read,
                           taskMode)

load :: (Collection a, MonadStorage m) => DocId a -> m (Entity a)
load docid = do
    Document{value = obj} <- loadDocument docid
    entityVal <- liftEither $ getObject obj
    pure $ Entity docid entityVal

loadAll :: (Collection a, MonadStorage m) => m [Entity a]
loadAll = getDocuments >>= traverse load

loadActiveContacts :: MonadStorage m => m [Entity Contact]
loadActiveContacts =
    filter ((== Active) . contact_status . entityVal) <$> loadAll

loadArchivedContacts :: MonadStorage m => m [Entity Contact]
loadArchivedContacts =
    filter ((== Archived) . contact_status . entityVal) <$> loadAll

getContactSamples :: MonadStorage m => Bool -> m ContactSample
getContactSamples = getContactSamplesWith $ const True

getContactSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter contacts by text
    -> Bool            -- ^ search within archived contacts
    -> m ContactSample
getContactSamplesWith predicate archive = do
    contacts <- bool loadActiveContacts loadArchivedContacts archive
    pure . (\ys -> Sample ys $ genericLength ys) $
        filter (predicate . Text.pack . contact_name . entityVal) contacts

loadActiveTasks :: MonadStorage m => m [Entity Note]
loadActiveTasks =
    filter ((TaskStatus Active ==) . note_status . entityVal) <$> loadAll

loadArchivedTasks :: MonadStorage m => m [Entity Note]
loadArchivedTasks =
    filter ((TaskStatus Archived ==) . note_status . entityVal) <$> loadAll

loadWikis :: MonadStorage m => m [Entity Note]
loadWikis = filter ((Wiki ==) . note_status . entityVal) <$> loadAll

getTaskSamples
    :: MonadStorage m
    => Bool            -- ^ search within archived tasks
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (ModeMap NoteSample)
getTaskSamples = getTaskSamplesWith $ const True

getTaskSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter notes by text
    -> Bool            -- ^ search within archived tasks
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (ModeMap NoteSample)
getTaskSamplesWith predicate archive ConfigUI{shuffle} limit today = do
    tasks <- bool loadActiveTasks loadArchivedTasks archive
    pure .
        takeSamples limit .
        shuffleOrSort .
        splitModesBy entityVal today $
        filter (predicate . Text.pack . note_text . entityVal) tasks
  where
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
        | shuffle   = shuffleTraverseItems gen
        | otherwise =
            -- in sorting by entityId no business-logic is involved,
            -- it's just for determinism
            fmap $ sortOn $ note_start . entityVal &&& entityId

getWikiSamples
    :: MonadStorage m
    => Bool            -- archived search
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (Maybe NoteSample)
getWikiSamples = getWikiSamplesWith $ const True

getWikiSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter tasks by text
    -> Bool            -- ^ if archived search, return Nothing
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (Maybe NoteSample)
getWikiSamplesWith predicate archive ConfigUI{shuffle} limit today =
    if archive then pure Nothing
    else do
        wikis0 <- loadWikis
        let wikis1 = filter (predicate . Text.pack . note_text . entityVal) wikis0
        let wikis2 = case limit of
                Nothing -> wikis1
                Just l -> take (fromIntegral l) wikis1
        pure . Just . toSample $ shuffleOrSort wikis2
  where
    toSample ys = Sample ys $ genericLength ys
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
        | shuffle   = shuffleItems gen
        | otherwise =
            -- in sorting by entityId no business-logic is involved,
            -- it's just for determinism
            sortOn entityId

shuffleItems :: StdGen -> [b] -> [b]
shuffleItems gen = (`evalState` gen) . shuf
  where
    shuf xs = do
        g <- state split
        pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

shuffleTraverseItems :: Traversable t => StdGen -> t [b] -> t [b]
shuffleTraverseItems gen = (`evalState` gen) . traverse shuf
  where
    shuf xs = do
        g <- state split
        pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

splitModesBy :: (note -> Note) -> Day -> [note] -> ModeMap [note]
splitModesBy f today = Map.unionsWith (++) . map singleton where
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
    => HashMap Track NoteId
        -- ^ selection of all aready tracked notes
    -> Note  -- ^ external note to insert
    -> m ()
updateTrackedNote oldNotes note = case note of
    Note{note_track = Just track} -> case HashMap.lookup track oldNotes of
        Nothing -> do
            obj <- newObject note
            createDocument obj
        Just noteid -> void $ modify noteid $ do
            note_status_assignIfDiffer note_status
            note_text_zoom $ RGA.edit note_text
    _ -> throwError "External note is expected to be supplied with tracking"
  where
    Note{note_status, note_text} = note

updateTrackedNotes :: [Note] -> Storage ()
updateTrackedNotes newNotes = do
    -- TODO(2018-10-22, cblp) index notes by track in the database and select
    -- specific note by its track
    notes <- getDocuments
    oldNotes <-
        fmap (HashMap.fromList . catMaybes) .
        for notes $ \noteId -> do
            Document{value = obj} <- loadDocument noteId
            mTrack <- (`evalStateT` obj) note_track_read
            pure $ (, noteId) <$> mTrack
    for_ newNotes $ updateTrackedNote oldNotes

cmdNewNote :: MonadStorage m => New -> Day -> m (Entity Note)
cmdNewNote New{text, start, end, isWiki} today = do
    let start' = fromMaybe today start
    whenJust end $ assertStartBeforeEnd start'
    (note_status, note_end, note_start) <- case end of
        _ | not isWiki -> pure (TaskStatus Active, end, start')
        Nothing        -> pure (Wiki, Nothing, today)
        Just _         -> throwError "A wiki must have no end date."
    let note = Note
            { note_end
            , note_start
            , note_status
            , note_text = Text.unpack text
            , note_track = Nothing
            }
    obj@Object{id} <- newObject note
    createDocument obj
    pure $ Entity (docIdFromUuid id) note

cmdNewContact :: MonadStorage m => Text -> m (Entity Contact)
cmdNewContact name = do
    let contact =
            Contact{contact_name = Text.unpack name, contact_status = Active}
    obj@Object{id} <- newObject contact
    createDocument obj
    pure $ Entity (docIdFromUuid id) contact

cmdDeleteContact :: MonadStorage m => ContactId -> m (Entity Contact)
cmdDeleteContact cid = modifyAndView cid $ do
    contact_status_assign Deleted
    contact_name_zoom $ RGA.editText ""

cmdSearch
    :: Text         -- ^ query
    -> Bool         -- ^ search within archived tasks or contacts
    -> ConfigUI
    -> Maybe Limit
    -> Day          -- ^ today
    -> Storage (ModeMap NoteSample, Maybe NoteSample, ContactSample)
cmdSearch substr archive ui limit today = do
    -- TODO(cblp, 2018-12-21) search tasks and wikis in one step
    tasks    <- getTaskSamplesWith    predicate archive ui limit today
    wikis    <- getWikiSamplesWith    predicate archive ui limit today
    contacts <- getContactSamplesWith predicate archive
    pure (tasks, wikis, contacts)
  where
    predicate = Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold

cmdShow :: NoteId -> Storage (Entity Note)
cmdShow = load

cmdDeleteNote :: MonadStorage m => NoteId -> m (Entity Note)
cmdDeleteNote nid = modifyAndView nid $ do
    assertNoteIsNative
    note_status_assign $ TaskStatus Deleted
    note_text_zoom     $ RGA.editText ""
    note_start_assign  $ fromGregorian 0 1 1
    note_end_assign      Nothing

cmdDone :: MonadStorage m => NoteId -> m (Entity Note)
cmdDone nid = modifyAndView nid $ do
    assertNoteIsNative
    note_status_assign $ TaskStatus Archived

cmdUnarchive :: MonadStorage m => NoteId -> m (Entity Note)
cmdUnarchive nid = modifyAndView nid $ note_status_assign $ TaskStatus Active

cmdEdit :: Edit -> Storage [Entity Note]
cmdEdit edit = case edit of
    Edit{ids = _ :| _ : _, text = Just _} ->
        throwError "Can't edit content of multiple notes"
    Edit{ids = id :| [], text, start = Nothing, end = Nothing} ->
        fmap (:[]) $ modifyAndView id $ do
            assertNoteIsNative
            note_text_zoom $ do
                noteText' <- case text of
                    Just noteText' -> pure noteText'
                    Nothing        -> do
                        noteText <- liftEither =<< gets RGA.getText
                        liftIO $ runExternalEditor noteText
                RGA.editText noteText'
    Edit{ids, text, start, end} ->
        fmap toList . for ids $ \id ->
            modifyAndView id $ do
                -- check text editability
                whenJust text $ const assertNoteIsNative
                -- check start and end editability
                when (isJust start || isJust end) $ do
                    status <- note_status_read
                    when (status == Wiki) $
                        throwError "Wiki dates are immutable"
                -- check start and end relation
                do  curStart <- note_start_read
                    curEnd   <- note_end_read
                    let newStartEnd = (,)
                            <$> (start <|> Just curStart)
                            <*> (end'  <|> curEnd)
                        end' = end >>= maybeClearToMaybe
                    whenJust newStartEnd $
                        uncurry assertStartBeforeEnd
                -- update
                whenJust end  $ note_end_assign . maybeClearToMaybe
                whenJust start  note_start_assign
                whenJust text $ note_text_zoom . RGA.editText

cmdPostpone :: NoteId -> Storage (Entity Note)
cmdPostpone nid = modifyAndView nid $ do
    today <- getUtcToday
    start <- note_start_read
    let start' = addDays 1 $ max today start
    note_start_assign start'
    mEnd <- note_end_read
    case mEnd of
        Just end | end < start' -> note_end_assign $ Just start'
        _                       -> pure ()

modifyAndView
    :: (Collection a, MonadStorage m)
    => DocId a -> StateT (Object a) m () -> m (Entity a)
modifyAndView docid f = do
    obj <- modify docid f
    entityVal <- liftEither $ getObject obj
    pure $ Entity docid entityVal

getUtcToday :: MonadIO io => io Day
getUtcToday = liftIO $ utctDay <$> getCurrentTime

runExternalEditor :: Text -> IO Text
runExternalEditor textOld = do
    editor <- asum $ assertExecutableFromEnv "EDITOR" : map
        assertExecutable
        ["editor", "micro", "nano"]
    withSystemTempFile "ff.edit" $ \file fileH -> do
        Text.hPutStr fileH textOld
        hClose fileH
        runProcess (proc editor [file]) >>= \case
            ExitSuccess   -> Text.strip <$> Text.readFile file
            ExitFailure{} -> pure textOld
  where
    assertExecutable prog = do
        Just _ <- findExecutable prog
        pure prog
    assertExecutableFromEnv param = assertExecutable =<< getEnv param

assertStartBeforeEnd :: MonadE m => Day -> Day -> m ()
assertStartBeforeEnd start end =
    unless (start <= end) $ throwError "task cannot end before it is started"

note_status_assignIfDiffer
    :: (ReplicaClock m, MonadE m, MonadState (Object Note) m)
    => NoteStatus -> m ()
note_status_assignIfDiffer newStatus = do
    curStatus <- note_status_read
    when (curStatus /= newStatus) $
        note_status_assign newStatus
{-# ANN note_status_assignIfDiffer ("HLint: ignore Use camelCase" :: String) #-}

assertNoteIsNative :: (MonadE m, MonadState (Object Note) m) => m ()
assertNoteIsNative = do
    -- TODO(2018-10-22, cblp) use `case of some/none` without full decoding of
    -- `some`
    tracking <- note_track_read
    whenJust tracking $ \_ ->
        throwError "A tracked note must be modified in its source."

getDataDir :: Config -> IO FilePath
getDataDir cfg = do
    cur <- getCurrentDirectory
    mDataDirFromVcs <- findVcs $ parents cur
    case mDataDirFromVcs of
        Just dataDir -> pure dataDir
        Nothing      -> checkDataDir cfg
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise
    findVcs []         = pure Nothing
    findVcs (dir:dirs) = do
        isDirVcs <- doesDirectoryExist (dir </> ".git")
        if isDirVcs then
            pure . Just $ dir </> ".ff"
        else
            findVcs dirs

checkDataDir :: Monad m => Config -> m FilePath
checkDataDir Config{dataDir} = case dataDir of
    Just dir -> pure dir
    Nothing  -> fail "Data directory isn't set, run `ff config dataDir --help`"

identity :: a -> a
identity x = x

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
    Nothing -> pure ()
    Just x  -> f x
