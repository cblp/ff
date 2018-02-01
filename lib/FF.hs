{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FF
    ( Note
    , getSamples
    , cmdDelete
    , cmdDone
    , cmdEdit
    , cmdNew
    , cmdPostpone
    , cmdSearch
    ) where

import           Control.Arrow ((&&&))
import           Control.Error ((?:))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (evalState, state)
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (Day, addDays, getCurrentTime, utctDay)
import           Data.Traversable (for)
import           System.Exit (ExitCode (..))
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process.Typed (proc, runProcess)

import           FF.Options (Edit (..), New (..))
import           FF.Storage (Collection, DocId, Storage, list, load, modify,
                             saveNew)
import           FF.Types (ModeMap (..), Note (..), NoteId, NoteView (..),
                           Sample (..), Status (Active, Archived, Deleted),
                           noteView, singletonTaskModeMap)

getSamples :: Int -> Storage (ModeMap Sample)
getSamples limit = do
    today <- getUtcToday
    docs <- list
    mnotes <- for docs load
    let activeNotes =
            [ noteView doc note
            | (doc, Just note) <- zip docs mnotes
            , LWW.query (noteStatus note) == Active
            ]
    pure $ takeSamples limit $ splitModes today activeNotes

splitModes :: Day -> [NoteView] -> ModeMap [NoteView]
splitModes = foldMap . singletonTaskModeMap

takeSamples :: Int -> ModeMap [NoteView] -> ModeMap Sample
takeSamples limit ModeMap{..} = (`evalState` limit) $
    ModeMap
    <$> sample end   overdue
    <*> sample end   endToday
    <*> sample end   endSoon
    <*> sample start actual
    <*> sample start starting
  where
    sample key xs = state $ \n ->
        (Sample (take n xs') (fromIntegral len), n - len)
      where
        -- in sorting by nid no business-logic is involved,
        -- it's just for determinism
        xs' = sortOn (key &&& nid) xs
        len = length xs'

cmdNew :: New -> Storage NoteView
cmdNew New{newText, newStart, newEnd} = do
    newStart' <- fromMaybeA getUtcToday newStart
    case newEnd of
        Just end -> assertStartBeforeEnd newStart' end
        _        -> pure ()
    note <- do
        noteStatus  <- LWW.initial Active
        noteText    <- LWW.initial newText
        noteStart   <- LWW.initial newStart'
        noteEnd     <- LWW.initial newEnd
        pure Note{..}
    nid <- saveNew note
    pure $ noteView nid note

cmdDelete :: NoteId -> Storage NoteView
cmdDelete nid =
    modifyAndView nid $ \note@Note{noteStatus} -> do
        noteStatus' <- lwwModify (const Deleted) noteStatus
        pure note{noteStatus = noteStatus'}

cmdDone :: NoteId -> Storage NoteView
cmdDone nid =
    modifyAndView nid $ \note@Note{noteStatus} -> do
        noteStatus' <- lwwModify (const Archived) noteStatus
        pure note{noteStatus = noteStatus'}

cmdEdit :: Edit -> Storage NoteView
cmdEdit (Edit nid Nothing Nothing Nothing) =
    modifyAndView nid $ \note@Note{noteText} -> do
        text' <- liftIO $ runExternalEditor $ LWW.query noteText
        noteText' <- lwwModify (const text') noteText
        pure note{noteText = noteText'}
cmdEdit Edit{editId = nid, editEnd, editStart, editText} =
    modifyAndView nid $ \note -> do
        checkStartEnd note
        update note
  where
    checkStartEnd Note{noteStart = (LWW.query -> noteStart), noteEnd} =
        case newStartEnd of
            Just (start, end) -> assertStartBeforeEnd start end
            Nothing           -> pure ()
      where
        newStartEnd = case (editStart, editEnd, LWW.query noteEnd) of
            (Just start, Nothing        , Just end) -> Just (start    , end)
            (Nothing   , Just (Just end), _       ) -> Just (noteStart, end)
            (Just start, Just (Just end), _       ) -> Just (start    , end)
            _                                       -> Nothing
    update note@Note{noteEnd, noteStart, noteText} = do
        noteEnd'   <- lwwModify (editEnd   ?:) noteEnd
        noteStart' <- lwwModify (editStart ?:) noteStart
        noteText'  <- lwwModify (editText  ?:) noteText
        pure note
            {noteEnd = noteEnd', noteStart = noteStart', noteText = noteText'}

cmdPostpone :: NoteId -> Storage NoteView
cmdPostpone nid =
    modifyAndView nid $ \note@Note{noteStart, noteEnd} -> do
        today <- getUtcToday
        let start' = addDays 1 $ max today $ LWW.query noteStart
        noteStart' <- lwwModify (const start')      noteStart
        noteEnd'   <- lwwModify (fmap (max start')) noteEnd
        pure note{noteStart = noteStart', noteEnd = noteEnd'}

cmdSearch :: Text -> Int -> Storage Agenda
cmdSearch text limit = do
    today <- getUtcToday
    let isOverdue NoteView{end} = end < Just today
    let isToday NoteView{end} = end == Just today
    docs <- list
    mnotes <- for docs load
    let activeNotes =
            [ noteView doc note
            | (doc, Just note@Note{noteStatus = (LWW.query -> Active)}) <- zip docs mnotes
            , Text.isInfixOf (Text.toLower text) (Text.toLower . LWW.query $ noteText note)
            ]
    let (notesWithEnd, startingNotes) = partition (isJust . end) activeNotes
    let (overdueNotes, endingNotes) = span isOverdue $ sortOn onEnd notesWithEnd
    let (endingTodayNotes, endingSoonNotes) = span isToday endingNotes
    pure Agenda
        { overdue     = sample limit overdueNotes
        , endingToday = sample (limit - length overdueNotes) endingTodayNotes
        , endingSoon  =
            sample
                (limit - length overdueNotes - length endingTodayNotes)
                endingSoonNotes
        , starting =
            sample
                (limit
                    - length overdueNotes
                    - length endingTodayNotes
                    - length endingSoonNotes)
                (sortOn onStart startingNotes)
        }
  where
    onEnd NoteView{end, nid} =
        ( end -- closest first
        , nid -- no business-logic involved, just for determinism
        )
    onStart NoteView{start, nid} =
        ( start -- oldest first
        , nid   -- no business-logic involved, just for determinism
        )
    sample n xs = Sample (take n xs) (genericLength xs)

fromMaybeA :: Applicative m => m a -> Maybe a -> m a
fromMaybeA m = maybe m pure

-- | Check the document exists. Return actual version.
modifyOrFail ::
    (Collection doc, Eq doc) => DocId doc -> (doc -> Storage doc) -> Storage doc
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

lwwModify :: (Eq a, Clock m) => (a -> a) -> LWW a -> m (LWW a)
lwwModify f lww = let
    x = LWW.query lww
    y = f x
    in
    if x /= y then LWW.assign y lww else pure lww

runExternalEditor :: Text -> IO Text
runExternalEditor textOld =
    withSystemTempFile "ff.edit" $ \file fileH -> do
        Text.hPutStr fileH textOld
        hClose fileH
        runProcess (proc editor [file]) >>= \case
            ExitSuccess   -> Text.strip <$> Text.readFile file
            ExitFailure{} -> pure textOld

editor :: FilePath
editor = "nano"

assertStartBeforeEnd :: Monad m => Day -> Day -> m ()
assertStartBeforeEnd start end =
    unless (start <= end) $ fail "task cannot end before it is started"
