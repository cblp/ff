{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module FF
    ( cmdDeleteNote
    , cmdDeleteContact
    , cmdDone
    , cmdEdit
    , cmdNewNote
    , cmdNewContact
    , cmdPostpone
    , cmdSearch
    , cmdUnarchive
    , getContactSamples
    , getNoteSamples
    , getUtcToday
    , getWikiSamples
    , loadActiveNotes
    , loadAll
    , splitModes
    , takeSamples
    , updateTrackedNotes
    ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Except (MonadError, liftEither, throwError)
import           Control.Monad.Extra (unless, when, whenJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (MonadState, StateT, evalState,
                                             evalStateT, execStateT, gets,
                                             state)
import           Data.Foldable (asum, for_)
import           Data.Functor (($>))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (genericLength, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, fromGregorian, getCurrentTime,
                            toModifiedJulianDay, utctDay)
import           Data.Traversable (for)
import           RON.Data (getObject, newObject)
import qualified RON.Data.RGA as RGA
import           RON.Event (Clock)
import           RON.Storage (Collection, DocId (..), Document (..),
                              MonadStorage, createVersion, listDocuments,
                              loadDocument, modify)
import           RON.Storage.IO (Storage)
import           RON.Types (Object, objectId)
import           System.Directory (findExecutable)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)
import           System.Random (StdGen, mkStdGen, randoms, split)

import           FF.Config (ConfigUI (..))
import           FF.Options (Edit (..), New (..))
import           FF.Types (Contact (..), ContactId, ContactSample, Entity,
                           pattern Entity, EntityF (..), Limit, ModeMap,
                           Note (..), NoteId, NoteSample, NoteStatus (..),
                           Sample (..), Status (..), Track, contact_name_zoom,
                           contact_status_assign, note_end_assign,
                           note_end_read, note_start_assign, note_start_read,
                           note_status_assign, note_status_read, note_text_zoom,
                           note_track_read, taskMode)

loadAll :: (Collection a, MonadStorage m) => m [Entity a]
loadAll = do
    docs <- listDocuments
    for docs $ \docId -> do
        Document{value = obj} <- loadDocument docId
        entityVal <- liftEither $ getObject obj
        pure $ Entity (objectId obj) entityVal

loadActiveContacts :: MonadStorage m => m [Entity Contact]
loadActiveContacts =
    filter ((== Active) . contact_status . entityVal) <$> loadAll

getContactSamples :: MonadStorage m => m ContactSample
getContactSamples = getContactSamplesWith $ const True

getContactSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter contacts by text
    -> m ContactSample
getContactSamplesWith predicate = do
    activeContacts <- loadActiveContacts
    pure . (\ys -> Sample ys $ genericLength ys) $
        filter (predicate . Text.pack . contact_name . entityVal) activeContacts

loadTrackedNotes :: MonadStorage m => m [(NoteId, Object Note)]
loadTrackedNotes = do
    docs <- listDocuments
    fmap catMaybes . for docs $ \docId -> do
        Document{value = obj} <- loadDocument docId
        mTrack <- (`evalStateT` obj) note_track_read
        pure $ mTrack $> (docId, obj)

loadActiveNotes :: MonadStorage m => m [Entity Note]
loadActiveNotes =
    filter ((TaskStatus Active ==) . note_status . entityVal) <$> loadAll

loadWikiNotes :: MonadStorage m => m [Entity Note]
loadWikiNotes = filter ((Wiki ==) . note_status . entityVal) <$> loadAll

getNoteSamples
    :: MonadStorage m
    => ConfigUI
    -> Maybe Limit
    -> Day  -- ^ today
    -> m (ModeMap NoteSample)
getNoteSamples = getNoteSamplesWith $ const True

getNoteSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter notes by text
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (ModeMap NoteSample)
getNoteSamplesWith predicate ConfigUI{shuffle} limit today = do
    activeNotes <- loadActiveNotes
    pure .
        takeSamples limit .
        shuffleOrSort .
        splitModesBy entityVal today $
        filter (predicate . Text.pack . note_text . entityVal) activeNotes
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
    => ConfigUI
    -> Maybe Limit
    -> Day  -- ^ today
    -> m NoteSample
getWikiSamples = getWikiSamplesWith $ const True

getWikiSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter notes by text
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m NoteSample
getWikiSamplesWith predicate ConfigUI{shuffle} limit today = do
    wikiNotes <- loadWikiNotes
    let filteredNotes =
            filter (predicate . Text.pack . note_text . entityVal) wikiNotes
    let wikis = case limit of
            Nothing -> filteredNotes
            Just l -> take (fromIntegral l) filteredNotes
    pure . toSample $ shuffleOrSort wikis
  where
    toSample ys = Sample ys $ genericLength ys
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today
    shuffleOrSort
        | shuffle   = shuffleItems gen
        | otherwise =
            -- in sorting by entityId no business-logic is involved,
            -- it's just for determinism
            sortOn $ note_start . entityVal &&& entityId

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
splitModesBy f today = Map.unionsWith (++) . map singleton
  where
    singleton note = Map.singleton (taskMode today $ f note) [note]

splitModes :: Day -> [Note] -> ModeMap [Note]
splitModes = splitModesBy id

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
    => HashMap Track (NoteId, Object Note)
        -- ^ selection of all aready tracked notes
    -> Note  -- ^ external note to insert
    -> m ()
updateTrackedNote oldNotes note = case note of
    Note{note_track = Just track} -> do
        (mNoteid, obj) <- case HashMap.lookup track oldNotes of
            Nothing -> do
                obj <- newObject note
                pure (Nothing, obj)
            Just (noteid, oldNote) -> do
                newNote <- (`execStateT` oldNote) $ do
                    note_status_assignIfDiffer note_status
                    note_text_zoom $ RGA.edit note_text
                pure (Just noteid, newNote)
        createVersion mNoteid obj
    _ -> throwError "External note is expected to be supplied with tracking"
  where
    Note{note_status, note_text} = note

updateTrackedNotes :: [Note] -> Storage ()
updateTrackedNotes nvNews = do
    -- TODO(2018-10-22, cblp) index notes by track in the database and select
    -- specific note by its track
    oldNotes <- loadTrackedNotes
    oldNotesWithTrack <- for oldNotes $ \note@(_, oldNote) -> do
        track <- (`evalStateT` oldNote) note_track_read
        pure (track, note)
    let oldNotesByTrack = HashMap.fromList
            [(track, note) | (Just track, note) <- oldNotesWithTrack]
    for_ nvNews $ updateTrackedNote oldNotesByTrack

cmdNewNote :: MonadStorage m => New -> Day -> m (Entity Note)
cmdNewNote New{newText, newStart, newEnd, newWiki} today = do
    let newStart' = fromMaybe today newStart
    whenJust newEnd $ assertStartBeforeEnd newStart'
    (note_status, note_end, note_start) <-
        if newWiki then case newEnd of
            Nothing -> pure (Wiki, Nothing, today)
            Just _  -> throwError "Wiki note has no end date."
        else pure (TaskStatus Active, newEnd, newStart')
    let note = Note
            { note_end
            , note_start
            , note_status
            , note_text = Text.unpack newText
            , note_track = Nothing
            }
    obj <- newObject note
    createVersion Nothing obj
    pure $ Entity (objectId obj) note

cmdNewContact :: MonadStorage m => Text -> m (Entity Contact)
cmdNewContact name = do
    let contact =
            Contact{contact_name = Text.unpack name, contact_status = Active}
    obj <- newObject contact
    createVersion Nothing obj
    pure $ Entity (objectId obj) contact

cmdDeleteContact :: MonadStorage m => ContactId -> m (Entity Contact)
cmdDeleteContact cid = modifyAndView cid $ do
    contact_status_assign Deleted
    contact_name_zoom $   RGA.editText ""

cmdSearch
    :: Text  -- ^ query
    -> Maybe Limit
    -> Day  -- ^ today
    -> Storage (ModeMap NoteSample, NoteSample, ContactSample)
cmdSearch substr limit today = do
    notes <- getNoteSamplesWith predicate ui limit today
    wiki <- getWikiSamplesWith predicate ui limit today
    contacts <- getContactSamplesWith predicate
    pure (notes, wiki, contacts)
  where
    predicate = Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold
    ui = ConfigUI {shuffle = False}

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

cmdEdit :: Edit -> Storage (Entity Note)
cmdEdit Edit{..} = case (editText, editStart, editEnd) of
    (Nothing, Nothing, Nothing) ->
        modifyAndView editId $ do
            assertNoteIsNative
            note_text_zoom $ do
                text <- liftEither =<< gets RGA.getText
                text' <- liftIO $ runExternalEditor text
                RGA.editText text'
    _ ->
        modifyAndView editId $ do
            whenJust editText $ const assertNoteIsNative
            checkStartEnd
            update
  where
    checkStartEnd = do
        start <- note_start_read
        mend  <- note_end_read
        let newStartEnd = case (editStart, editEnd, mend) of
                (Just eStart, Nothing        , Just end) -> Just (eStart, end)
                (Nothing    , Just (Just end), _       ) -> Just (start,  end)
                (Just eStart, Just (Just end), _       ) -> Just (eStart, end)
                _                                        -> Nothing
        whenJust newStartEnd $
            uncurry assertStartBeforeEnd

    update = do
        status <- note_status_read
        (start, end) <- case status of
            Wiki -> case (editStart, editEnd) of
                (Nothing, Nothing) -> pure (Nothing, Nothing)
                _ -> throwError "Wiki dates are immutable"
            _ -> pure (editStart, editEnd)
        whenJust end $ \d -> note_end_assign d
        whenJust start $ \d -> note_start_assign d
        whenJust editText $ \t -> note_text_zoom $ RGA.editText t

cmdPostpone :: NoteId -> Storage (Entity Note)
cmdPostpone nid = modifyAndView nid $ do
    today <- getUtcToday
    start <- note_start_read
    let start' = addDays 1 $ max today start
    note_start_assign start'
    mend <- note_end_read
    case mend of
        Just end | end < start' -> note_end_assign $ Just start'
        _                       -> pure ()

modifyAndView
    :: (Collection a, MonadStorage m)
    => DocId a -> StateT (Object a) m () -> m (Entity a)
modifyAndView docid f = do
    obj <- modify docid f
    entityVal <- liftEither $ getObject obj
    pure $ Entity (objectId obj) entityVal

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

assertStartBeforeEnd :: MonadError String m => Day -> Day -> m ()
assertStartBeforeEnd start end =
    unless (start <= end) $ throwError "task cannot end before it is started"

note_status_assignIfDiffer
    :: (Clock m, MonadError String m, MonadState (Object Note) m)
    => NoteStatus -> m ()
note_status_assignIfDiffer newStatus = do
    curStatus <- note_status_read
    when (curStatus /= newStatus) $
        note_status_assign newStatus
{-# ANN note_status_assignIfDiffer ("HLint: ignore Use camelCase" :: String) #-}

assertNoteIsNative :: (MonadError String m, MonadState (Object Note) m) => m ()
assertNoteIsNative = do
    -- TODO(2018-10-22, cblp) use `case of some/none` without full decoding of
    -- `some`
    tracking <- note_track_read
    whenJust tracking $ \_ ->
        throwError "Oh, no! It is tracked note. Not for modifying. Sorry :("
