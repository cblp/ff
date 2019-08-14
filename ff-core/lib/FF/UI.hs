{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module FF.UI (
    prettyContact,
    prettyContactSample,
    prettyNote,
    prettyNoteList,
    prettyTaskSections,
    prettyTasksWikisContacts,
    prettyWikiSample,
    sampleLabel,
    withHeader,
) where

import           Data.Char (isSpace)
import           Data.List (genericLength, intersperse)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Prettyprint.Doc (Doc, annotate, fillSep, hang,
                                            indent, pretty, sep, space, viaShow,
                                            vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle,
                                                            Color (..), bold,
                                                            color)
import           Data.Time (Day)
import           RON.Storage.Backend (DocId (DocId))

import           FF (fromRgaM)
import           FF.Types (Contact (..), ContactSample, Entity (..), ModeMap,
                           Note (..), NoteSample, NoteStatus (Wiki),
                           Sample (..), TaskMode (..), Track (..), omitted)

withHeader :: Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeader header value =
    hang indentation $ vsep [yellow $ pretty header, value]

indentation :: Int
indentation = 2

prettyDocId :: DocId a -> Doc ann
prettyDocId (DocId name) = pretty name

prettyTasksWikisContacts
    :: Bool                 -- ^ is output brief
    -> ModeMap NoteSample   -- ^ tasks
    -> NoteSample           -- ^ wikis
    -> ContactSample        -- ^ contacts
    -> Bool                 -- ^ does search involve tasks
    -> Bool                 -- ^ does search involve wikis
    -> Bool                 -- ^ does search involve contacts
    -> Doc AnsiStyle
prettyTasksWikisContacts
        isBrief tasks wiki contacts involveTasks involveWikis involveContacts =
    case (involveTasks, involveWikis, involveContacts) of
        (True,  False, False) -> ts
        (False, True,  False) -> ws
        (False, False, True ) -> cs
        (True,  True,  False) -> vsep [ts, ws]
        (False, True,  True ) -> vsep [ws, cs]
        (True,  False, True ) -> vsep [ts, cs]
        (_,     _,     _    ) -> vsep [ts, ws, cs]
  where
    ts = prettyTaskSections  isBrief tasks
    ws = prettyWikiSample    isBrief wiki
    cs = prettyContactSample isBrief contacts

prettyContactSample :: Bool -> ContactSample -> Doc AnsiStyle
prettyContactSample isBrief samples = stack isBrief $
    prettyContactSample' samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples
    prettyContactSample' = \case
        Sample{total = 0} -> red "No contacts to show"
        Sample{items} ->
            withHeader "Contacts:" . stack isBrief $
            map ((bullet <>) . indent 1 . prettyContact isBrief) items

prettyWikiSample :: Bool -> NoteSample -> Doc AnsiStyle
prettyWikiSample isBrief samples = stack isBrief $
    prettyWikiSample' samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples
    prettyWikiSample' = \case
        Sample{total = 0} -> red "No wikis to show"
        Sample{items} ->
            withHeader "Wiki:" .
            stack isBrief $
            map ((bullet <>) . indent 1 . prettyNote isBrief) items

prettyNoteList :: Bool -> [Entity Note] -> Doc AnsiStyle
prettyNoteList isBrief =
    stack isBrief . map ((bullet <>) . indent 1 . prettyNote isBrief)

-- | For both tasks and wikis
prettyNote
    :: Bool  -- ^ is brief
    -> Entity Note
    -> Doc AnsiStyle
prettyNote isBrief (Entity entityId note) = case isBrief of
    True -> fillSep [title text, meta] where
        meta = green "|" <+> cyan "id" <+> prettyDocId entityId
    False -> sparsedStack [wrapLines $ Text.pack text, sep meta] where
        meta
            = mconcat
                [   [ green "|" <+> cyan "id" <+> prettyDocId entityId
                    | entityId /= DocId ""
                    ]
                ,   [ green "|" <+> cyan "start" <+> viaShow @Day start
                    | note_status /= Just Wiki
                    ]
                ,   [ green "|" <+> cyan "end" <+> viaShow @Day end
                    | note_status /= Just Wiki
                    , Just end <- [note_end]
                    ]
                ]
            ++  [ green "|" <+> cyan "tracking" <+> pretty track_url
                | Just Track{..} <- [note_track]
                ]
  where
    Note{note_end, note_start, note_status, note_text, note_track} = note
    start = fromJust note_start
    text  = fromRgaM note_text

title :: String -> Doc ann
title
    = mconcat
    . map (fillSep . map pretty . Text.split isSpace)
    . take 1
    . Text.lines
    . Text.pack

prettyTaskSections :: Bool -> ModeMap (Sample (Entity Note)) -> Doc AnsiStyle
prettyTaskSections isBrief samples = stack isBrief
    $   [ prettyTaskSample isBrief mode sample
        | (mode, sample) <- Map.assocs samples
        ]
    ++  [magenta (pretty numOmitted) <> yellow " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettyTaskSample :: Bool -> TaskMode -> Sample (Entity Note) -> Doc AnsiStyle
prettyTaskSample isBrief mode = \case
    Sample{total = 0} -> red "No notes to show"
    Sample{total, items} ->
        withHeader (sampleLabel mode) . stack isBrief $
            map ((bullet <>) . indent 1 . prettyNote isBrief) items
            ++  [ hang indentation $
                    fillSep [toSeeAllLabel, blue $ cmdToSeeAll mode]
                | count /= total
                ]
      where
        toSeeAllLabel
            = yellow "To see all"
           <+> magenta (pretty $ Text.pack $ show total)
           <+> yellow "task(s), run:"
        count         = genericLength items
  where
    cmdToSeeAll = \case
        Overdue _  -> "ff search --overdue"
        EndToday   -> "ff search --today"
        EndSoon _  -> "ff search --soon"
        Actual     -> "ff search --actual"
        Starting _ -> "ff search --starting"

sampleLabel :: TaskMode -> Text
sampleLabel = \case
    Overdue n -> case n of
        1 -> "1 day overdue:"
        _ -> Text.pack (show n) <> " days overdue:"
    EndToday -> "Due today:"
    EndSoon n -> case n of
        1 -> "Due tomorrow:"
        _ -> "Due in " <> Text.pack (show n) <> " days:"
    Actual -> "Actual:"
    Starting n -> case n of
        1 -> "Starting tomorrow:"
        _ -> "Starting in " <> Text.pack (show n) <> " days:"

prettyContact :: Bool -> Entity Contact -> Doc AnsiStyle
prettyContact _isBrief (Entity entityId Contact{..}) = sep [pretty name, meta]
  where
    name = fromRgaM contact_name
    meta = green "|" <+> cyan "id" <+> prettyDocId entityId

wrapLines :: Text -> Doc ann
wrapLines =
    vsep . map (fillSep . map pretty . Text.split isSpace) . Text.splitOn "\n"

sparsedStack :: [Doc ann] -> Doc ann
sparsedStack = vsep . intersperse space

stack
    :: Bool  -- ^ is brief
    -> [Doc ann]
    -> Doc ann
stack = \case
    True  -> vsep
    False -> sparsedStack

bullet :: Doc AnsiStyle
bullet = green "*"

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate $ color Red <> bold

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate $ color Green <> bold

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate $ color Blue <> bold

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate $ color Cyan <> bold

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate $ color Magenta <> bold

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate $ color Yellow <> bold
