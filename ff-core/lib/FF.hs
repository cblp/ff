{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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
    , loadAllNotes
    , newNote
    , splitModes
    , takeSamples
    , updateTrackedNotes
    ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Extra (unless, void, whenJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (evalState, state)
import qualified CRDT.Cv.Max as Max
import           CRDT.Cv.RGA (RgaString)
import qualified CRDT.Cv.RGA as RGA
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Foldable (asum)
import           Data.List (genericLength, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, fromGregorian, getCurrentTime,
                            toModifiedJulianDay, utctDay)
import           Data.Traversable (for)
import           System.Directory (findExecutable)
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)
import           System.Random (StdGen, mkStdGen, randoms, split)

import           FF.Config (ConfigUI (..))
import           FF.Options (Edit (..), New (..))
import           FF.Storage (DocId (..), Document (..),
                             MonadStorage, Storage, create, listDocuments, load,
                             modify)
import           FF.Types (Contact (..), ContactId, ContactSample,
                           ContactView (..), Limit, ModeMap, Note (..), NoteId,
                           NoteSample, NoteStatus (..), NoteView (..),
                           Sample (..), Status (..), Tracked, contactView,
                           noteView, rgaFromText, rgaToText,
                           singletonTaskModeMap)

loadAllContacts :: (MonadStorage m) => m [ContactView]
loadAllContacts = do
    docs      <- listDocuments
    mcontacts <- for docs load
    pure
        [ contactView contactId value
        | (contactId, Right Document{value}) <- zip docs mcontacts
        ]

loadActiveContacts :: MonadStorage m => m [ContactView]
loadActiveContacts =
    filter (\ContactView { contactViewStatus } -> contactViewStatus == Active) <$> loadAllContacts

filterContacts :: (Text -> Bool) -> [ContactView] -> [ContactView]
filterContacts predicate notes =
    [ n | n@ContactView{contactViewId = DocId x, contactViewName} <- notes
    , predicate contactViewName || predicate (Text.pack x)
    ]

getContactSamples :: MonadStorage m => m ContactSample
getContactSamples = getContactSamplesWith $ const True

getContactSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter contacts by text
    -> m ContactSample
getContactSamplesWith predicate = do
    activeContacts <- loadActiveContacts
    pure . (\ys -> Sample ys $ genericLength ys) $
        filterContacts predicate activeContacts

loadAllNotes :: MonadStorage m => m [NoteView]
loadAllNotes = do
    docs   <- listDocuments
    mnotes <- for docs load
    pure
        [ noteView noteId value
        | (noteId, Right Document{value}) <- zip docs mnotes
        ]

loadTrackedNotes :: MonadStorage m => m [(NoteId, Note)]
loadTrackedNotes = do
    docs   <- listDocuments
    mnotes <- for docs load
    pure
        [ (noteId, value)
        | (noteId, Right Document{value = value@Note{noteTracked = Just _}})
            <- zip docs mnotes
        ]

loadActiveNotes :: MonadStorage m => m [NoteView]
loadActiveNotes =
    filter (\NoteView { status } -> status == TaskStatus Active) <$> loadAllNotes

loadWikiNotes :: MonadStorage m => m [NoteView]
loadWikiNotes =
    filter (\NoteView { status } -> status == Wiki) <$> loadAllNotes

filterNotes :: (Text -> Bool) -> [NoteView] -> [NoteView]
filterNotes predicate notes =
    [ n | n@NoteView {nid = Just (DocId i), text} <- notes
    , predicate text || predicate (Text.pack i)
    ]

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
getNoteSamplesWith predicate ConfigUI { shuffle } limit today = do
    activeNotes <- loadActiveNotes
    -- in sorting by nid no business-logic is involved,
    -- it's just for determinism
    pure .
        takeSamples limit .
        (if shuffle then shuffleTraverseItems gen else fmap (sortOn $ start &&& nid)) .
        splitModes today $
        filterNotes predicate activeNotes
  where
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today

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
getWikiSamplesWith predicate ConfigUI { shuffle } limit today = do
    wikiNotes <- loadWikiNotes
    let filteredNotes = filterNotes predicate wikiNotes
    let wiki = case limit of
            Nothing -> filteredNotes
            Just l -> take (fromIntegral l) filteredNotes
    pure . toSample $
    -- in sorting by nid no business-logic is involved,
    -- it's just for determinism
        (if shuffle then shuffleItems gen else sortOn $ start &&& nid) wiki
  where
    toSample ys = Sample ys $ genericLength ys
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today

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

splitModes :: Day -> [NoteView] -> ModeMap [NoteView]
splitModes today = Map.unionsWith (<>) . fmap (singletonTaskModeMap today)

takeSamples :: Maybe Limit -> ModeMap [NoteView] -> ModeMap NoteSample
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
    :: [(NoteId, Note)] -- ^ selection of all aready tracked notes
    -> NoteView
    -> Storage ()
updateTrackedNote oldNotes NoteView{..} =
    case sameTrack of
        Nothing -> do
            note <- newNote' status text start end tracked
            void $ create note
        Just (n, _) ->
            void $ modify n $ \note@Note{..} -> do
                noteStatus' <- lwwAssignIfDiffer status noteStatus
                noteText'   <- rgaEditText text noteText
                pure note{noteStatus = noteStatus', noteText = noteText'}
  where
    isSameTrack oldNote = tracked == (Max.query <$> noteTracked oldNote)
    sameTrack = listToMaybe $ filter (isSameTrack . snd) oldNotes

updateTrackedNotes :: [NoteView] -> Storage ()
updateTrackedNotes nvNews = do
    oldNotes <- loadTrackedNotes
    mapM_ (updateTrackedNote oldNotes) nvNews

-- | Native 'Note' smart constructor
newNote :: Clock m => NoteStatus -> Text -> Day -> Maybe Day -> m Note
newNote status text start end = newNote' status text start end Nothing

-- | Generic 'Note' smart constructor
newNote' :: Clock m => NoteStatus -> Text -> Day -> Maybe Day -> Maybe Tracked -> m Note
newNote' status text start end tracked = do
    noteStatus <- LWW.initialize status
    noteText   <- rgaFromText text
    noteStart  <- LWW.initialize start
    noteEnd    <- LWW.initialize end
    let noteTracked = Max.initial <$> tracked
    pure Note{..}

cmdNewNote :: MonadStorage m => New -> Day -> m NoteView
cmdNewNote New { newText, newStart, newEnd, newWiki } today = do
    let newStart' = fromMaybe today newStart
    case newEnd of
        Just end -> assertStartBeforeEnd newStart' end
        _        -> pure ()
    (status, end, start) <-
        if newWiki then case newEnd of
            Nothing -> pure (Wiki, Nothing, today)
            Just _  -> fail "Wiki note has no end date."
        else pure (TaskStatus Active, newEnd, newStart')
    note <- newNote status newText start end
    nid  <- create note
    pure $ noteView nid note

-- | Generic 'Contact' smart constructor
newContact' :: Clock m => Status -> Text -> m Contact
newContact' st name = do
    contactStatus <- LWW.initialize st
    contactName   <- rgaFromText name
    pure Contact{..}

cmdNewContact :: MonadStorage m => Text -> m ContactView
cmdNewContact name = do
    contact <- newContact' Active name
    cid  <- create contact
    pure $ contactView cid contact

cmdDeleteContact :: ContactId -> Storage ContactView
cmdDeleteContact cid = modifyAndViewContact cid $ \contact@Contact {..} -> do
    contactStatus' <- LWW.assign Deleted contactStatus
    contactName'   <- rgaEditText Text.empty contactName
    pure contact
        { contactStatus = contactStatus'
        , contactName   = contactName'
        }

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

cmdDeleteNote :: NoteId -> Storage NoteView
cmdDeleteNote nid = modifyAndView nid $ \note@Note {..} -> do
    assertNoteIsNative note
    noteStatus' <- LWW.assign (TaskStatus Deleted) noteStatus
    noteText'   <- rgaEditText Text.empty noteText
    noteStart'  <- LWW.assign (fromGregorian 0 1 1) noteStart
    noteEnd'    <- LWW.assign Nothing noteEnd
    pure note { noteStatus = noteStatus'
              , noteText   = noteText'
              , noteStart  = noteStart'
              , noteEnd    = noteEnd'
              }

cmdDone :: NoteId -> Storage NoteView
cmdDone nid = modifyAndView nid $ \note@Note { noteStatus } -> do
    assertNoteIsNative note
    noteStatus' <- LWW.assign (TaskStatus Archived) noteStatus
    pure note { noteStatus = noteStatus' }

cmdUnarchive :: NoteId -> Storage NoteView
cmdUnarchive nid = modifyAndView nid $ \note@Note { noteStatus } -> do
    noteStatus' <- LWW.assign (TaskStatus Active) noteStatus
    pure note { noteStatus = noteStatus' }

cmdEdit :: Edit -> Storage NoteView
cmdEdit Edit{..} = case (editText, editStart, editEnd) of
    (Nothing, Nothing, Nothing) ->
        modifyAndView editId $ \note@Note{noteText} -> do
            assertNoteIsNative note
            text'     <- liftIO . runExternalEditor $ rgaToText noteText
            noteText' <- rgaEditText text' noteText
            pure note{noteText = noteText'}
    _ ->
        modifyAndView editId $ \note -> do
            whenJust editText $ \_ -> assertNoteIsNative note
            checkStartEnd note
            update note
  where
    checkStartEnd Note { noteStart = (LWW.query -> noteStart), noteEnd } =
        case newStartEnd of
            Just (start, end) -> assertStartBeforeEnd start end
            Nothing           -> pure ()
      where
        newStartEnd = case (editStart, editEnd, LWW.query noteEnd) of
            (Just start, Nothing        , Just end) -> Just (start, end)
            (Nothing   , Just (Just end), _       ) -> Just (noteStart, end)
            (Just start, Just (Just end), _       ) -> Just (start, end)
            _ -> Nothing
    update :: Note -> Storage Note
    update note@Note{noteStatus = (LWW.query -> noteStatus), noteEnd, noteStart, noteText} = do
        (start, end) <- case noteStatus of
            Wiki -> case (editStart, editEnd) of
                (Nothing, Nothing) -> pure (Nothing, Nothing)
                _                  -> fail "Wiki note has unchangable dates."
            _ -> pure (editStart, editEnd)
        noteEnd'   <- lwwAssignIfJust end noteEnd
        noteStart' <- lwwAssignIfJust start noteStart
        noteText'  <- maybe (pure noteText) (`rgaEditText` noteText) editText
        pure note
            { noteEnd   = noteEnd'
            , noteStart = noteStart'
            , noteText  = noteText'
            }

lwwAssignIfJust :: Clock m => Maybe a -> LWW a -> m (LWW a)
lwwAssignIfJust = maybe pure LWW.assign

cmdPostpone :: NoteId -> Storage NoteView
cmdPostpone nid = modifyAndView nid $ \note@Note { noteStart, noteEnd } -> do
    today <- getUtcToday
    let start' = addDays 1 $ max today $ LWW.query noteStart
    noteStart' <- LWW.assign start' noteStart
    noteEnd'   <- case LWW.query noteEnd of
        Just end | end < start' -> LWW.assign (Just start') noteEnd
        _                       -> pure noteEnd
    pure note { noteStart = noteStart', noteEnd = noteEnd' }

modifyAndView :: NoteId -> (Note -> Storage Note) -> Storage NoteView
modifyAndView nid f = noteView nid <$> modify nid f

modifyAndViewContact :: ContactId -> (Contact -> Storage Contact) -> Storage ContactView
modifyAndViewContact cid f = contactView cid <$> modify cid f

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

assertStartBeforeEnd :: Monad m => Day -> Day -> m ()
assertStartBeforeEnd start end =
    unless (start <= end) $ fail "task cannot end before it is started"

rgaEditText :: Clock m => Text -> RgaString -> m RgaString
rgaEditText = RGA.edit . Text.unpack

lwwAssignIfDiffer :: (Eq a, Clock m) => a -> LWW a -> m (LWW a)
lwwAssignIfDiffer new var = do
    let cur = LWW.query var
    if new == cur then
        pure var
    else
        LWW.assign new var

assertNoteIsNative :: Monad m => Note -> m ()
assertNoteIsNative = \case
    Note{noteTracked = Just _} ->
        fail "Oh, no! It is tracked note. Not for modifying. Sorry :("
    _ ->
        pure ()
