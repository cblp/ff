{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( cmdDelete
    , cmdDone
    , cmdEdit
    , cmdNew
    , cmdPostpone
    , cmdSearch
    , cmdUnarchive
    , getSamples
    , getUtcToday
    , loadAllNotes
    , loadActiveNotes
    , newNote
    , splitModes
    , takeSamples
    ) where

import           Control.Arrow ((&&&))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (evalState, state)
import           CRDT.Cv.RGA (RgaString)
import qualified CRDT.Cv.RGA as RGA
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Foldable (asum)
import           Data.List (genericLength, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
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
import           FF.Storage (Collection, DocId, Document (..), MonadStorage,
                             Storage, listDocuments, load, modify, saveNew)
import           FF.Types (Limit, ModeMap, Note (..), NoteId, NoteView (..),
                           Sample (..), Status (Active, Archived, Deleted),
                           noteView, singletonTaskModeMap)
                          --  Tracked (..), noteView, singletonTaskModeMap)

getSamples
    :: MonadStorage m
    => ConfigUI
    -> Maybe Limit
    -> Day  -- ^ today
    -> m (ModeMap Sample)
getSamples = getSamplesWith $ const True

cmdSearch
    :: Text
    -> Maybe Limit
    -> Day  -- ^ today
    -> Storage (ModeMap Sample)
cmdSearch substr = getSamplesWith
    (Text.isInfixOf (Text.toCaseFold substr) . Text.toCaseFold)
    ConfigUI {shuffle = False}

loadAllNotes :: MonadStorage m => m [NoteView]
loadAllNotes = do
    docs   <- listDocuments
    mnotes <- for docs load
    pure
        [ noteView noteId value
        | (noteId, Just Document{value}) <- zip docs mnotes
        ]

loadActiveNotes :: MonadStorage m => m [NoteView]
loadActiveNotes =
    filter (\NoteView { status } -> status == Active) <$> loadAllNotes

getSamplesWith
    :: MonadStorage m
    => (Text -> Bool)  -- ^ predicate to filter notes by text
    -> ConfigUI
    -> Maybe Limit
    -> Day             -- ^ today
    -> m (ModeMap Sample)
getSamplesWith predicate ConfigUI { shuffle } limit today = do
    activeNotes <- loadActiveNotes
    -- in sorting by nid no business-logic is involved,
    -- it's just for determinism
    pure .
        takeSamples limit .
        (if shuffle then shuffleItems gen else fmap (sortOn $ start &&& nid)) .
        splitModes today $
        filter (predicate . text) activeNotes
  where
    gen = mkStdGen . fromIntegral $ toModifiedJulianDay today

shuffleItems :: Traversable t => StdGen -> t [b] -> t [b]
shuffleItems gen = (`evalState` gen) . traverse shuf
  where
    shuf xs = do
        g <- state split
        pure . map snd . sortOn fst $ zip (randoms g :: [Int]) xs

splitModes :: Day -> [NoteView] -> ModeMap [NoteView]
splitModes today = Map.unionsWith (<>) . fmap (singletonTaskModeMap today)

takeSamples :: Maybe Limit -> ModeMap [NoteView] -> ModeMap Sample
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

-- newTrack
--     :: Clock m
--     => NoteView
--     -> m Note
-- newTrack NoteView { status, text, start, end, extId, source} = do
--     noteStatus <- LWW.initialize status
--     noteText   <- rgaFromText text
--     noteStart  <- LWW.initialize start
--     noteEnd    <- LWW.initialize end
--     noteTrack  <- LWW.initialize $ pure $ Tracked
--         (fromMaybe "" extId)
--         (fromMaybe "" source)
--     pure Note {..}

-- cmdTrack :: MonadStorage m => NoteView -> m NoteView
-- cmdTrack noteList = do
--     note <- newTrack noteList
--     nid  <- saveNew note
--     pure $ noteView nid note

newNote
    :: Clock m
    => Status
    -> Text
    -> Day
    -> Maybe Day
    -> m Note
newNote status text start end = do
    noteStatus <- LWW.initialize status
    noteText   <- rgaFromText text
    noteStart  <- LWW.initialize start
    noteEnd    <- LWW.initialize end
    noteTrack  <- LWW.initialize Nothing
    pure Note {..}

cmdNew :: MonadStorage m => New -> Day -> m NoteView
cmdNew New { newText, newStart, newEnd } today = do
    let newStart' = fromMaybe today newStart
    case newEnd of
        Just end -> assertStartBeforeEnd newStart' end
        _        -> pure ()
    note <- newNote Active newText newStart' newEnd
    nid  <- saveNew note
    pure $ noteView nid note

cmdDelete :: NoteId -> Storage NoteView
cmdDelete nid = modifyAndView nid $ \note@Note {..} -> do
    noteStatus' <- LWW.assign Deleted noteStatus
    noteText'   <- rgaEditText Text.empty noteText
    noteStart'  <- LWW.assign (fromGregorian 0 1 1) noteStart
    noteEnd'    <- LWW.assign Nothing noteEnd
    -- tracked'    <- LWW.assign Nothing tracked
    pure note { noteStatus = noteStatus'
              , noteText   = noteText'
              , noteStart  = noteStart'
              , noteEnd    = noteEnd'
              -- , tracked    = tracked'
              }

cmdDone :: NoteId -> Storage NoteView
cmdDone nid = modifyAndView nid $ \note@Note { noteStatus } -> do
    noteStatus' <- LWW.assign Archived noteStatus
    pure note { noteStatus = noteStatus' }

cmdUnarchive :: NoteId -> Storage NoteView
cmdUnarchive nid = modifyAndView nid $ \note@Note { noteStatus } -> do
    noteStatus' <- LWW.assign Active noteStatus
    pure note { noteStatus = noteStatus' }

cmdEdit :: Edit -> Storage NoteView
cmdEdit (Edit nid Nothing Nothing Nothing) =
    modifyAndView nid $ \note@Note { noteText } -> do
        text'     <- liftIO . runExternalEditor $ rgaToText noteText
        noteText' <- rgaEditText text' noteText
        pure note { noteText = noteText' }
cmdEdit Edit { editId = nid, editEnd, editStart, editText } =
    modifyAndView nid $ \note -> do
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
    update note@Note { noteEnd, noteStart, noteText } = do
        noteEnd'   <- lwwAssignIfJust editEnd noteEnd
        noteStart' <- lwwAssignIfJust editStart noteStart
        noteText'  <- case editText of
            Nothing -> pure noteText
            Just et -> rgaEditText et noteText
        pure note { noteEnd   = noteEnd'
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

-- | Check the document exists. Return actual version.
modifyOrFail
    :: (Collection doc, Eq doc)
    => DocId doc
    -> (doc -> Storage doc)
    -> Storage doc
modifyOrFail docId f = modify docId $ \case
    Nothing -> fail $ concat
        ["Can't load document ", show docId, ". Where did you get this id?"]
    Just docOld -> do
        docNew <- f docOld
        pure (docNew, docNew)

modifyAndView :: NoteId -> (Note -> Storage Note) -> Storage NoteView
modifyAndView nid f = noteView nid <$> modifyOrFail nid f

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

rgaFromText :: Clock m => Text -> m RgaString
rgaFromText = RGA.fromString . Text.unpack

rgaToText :: RgaString -> Text
rgaToText = Text.pack . RGA.toString
