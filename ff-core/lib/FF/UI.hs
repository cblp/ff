{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module FF.UI (
    contactViewFull,
    noteViewFull,
    prettyContactSamplesOmitted,
    prettyNotesWikiContacts,
    prettySamplesBySections,
    prettyWikiSamplesOmitted,
    sampleFmap,
    sampleLabel,
    withHeader,
    ) where

import           Data.Char (isSpace)
import           Data.Foldable (toList)
import           Data.List (genericLength, intersperse)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as TextL
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), annotate, fillSep,
                                            hang, indent, sep, space, viaShow,
                                            vsep, (<+>))
import           Data.Time (Day)
import           RON.Text.Serialize (serializeUuid)
import           RON.Types (UUID)

import           FF.Types (Contact (..), ContactSample, Entity, pattern Entity,
                           EntityF (..), ModeMap, Note (..), NoteSample,
                           Sample (..), TaskMode (..), Track (..), omitted)

withHeader :: Text -> Doc Text -> Doc Text
withHeader header value = hang indentation $ vsep [pretty header, value]

indentation :: Int
indentation = 2

prettyUuid :: UUID -> Doc Text
prettyUuid = pretty . TextL.decodeUtf8 . serializeUuid

prettyNotesWikiContacts
    :: Bool  -- ^ brief output
    -> ModeMap NoteSample
    -> NoteSample
    -> ContactSample
    -> Bool  -- ^ search among tasks
    -> Bool  -- ^ search among wiki notes
    -> Bool  -- ^ search among contacts
    -> Doc Text
prettyNotesWikiContacts brief notes wiki contacts amongN amongW amongC =
    case (amongN, amongW, amongC) of
        (True,  False, False) -> ns
        (False, True,  False) -> ws
        (False, False, True ) -> cs
        (True,  True,  False) -> vsep [ns, ws]
        (False, True,  True ) -> vsep [ws, cs]
        (True,  False, True ) -> vsep [ns, cs]
        (_,     _,     _    ) -> vsep [ns, ws, cs]
  where
    ns = prettySamplesBySections brief notes
    ws = prettyWikiSamplesOmitted brief wiki
    cs = prettyContactSamplesOmitted brief contacts

prettyContactSamplesOmitted :: Bool -> ContactSample -> Doc Text
prettyContactSamplesOmitted _ samples = vsep $
    prettyContactSample True samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples

prettyContactSample :: Bool -> ContactSample -> Doc Text
prettyContactSample _ = \case
    Sample{sample_total = 0} -> mempty
    Sample{sample_items} ->
        withHeader "Contacts:" . vsep $
        map ((star <>) . indent 1 . contactViewFull) sample_items

prettyWikiSamplesOmitted :: Bool -> NoteSample -> Doc Text
prettyWikiSamplesOmitted brief samples = stack' brief $
    prettyWikiSample brief samples :
    [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples

prettyWikiSample :: Bool -> NoteSample -> Doc Text
prettyWikiSample brief = \case
    Sample{sample_total = 0} -> mempty
    Sample{sample_items} ->
        withHeader "Wiki notes:" .
        stack' brief $
        map ((star <>) . indent 1 . noteView) sample_items
  where
    noteView = if brief then noteViewBrief else noteViewFull

prettySamplesBySections
    :: Foldable f  => Bool -> ModeMap (Sample (EntityF f Note)) -> Doc Text
prettySamplesBySections brief samples = stack' brief
    $   [prettySample brief mode sample | (mode, sample) <- Map.assocs samples]
    ++  [pretty numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettySample :: Foldable f => Bool -> TaskMode -> Sample (EntityF f Note) -> Doc Text
prettySample brief mode = \case
    Sample{sample_total = 0} -> mempty
    Sample{sample_total, sample_items} ->
        withHeader (sampleLabel mode) . stack' brief $
            map ((star <>) . indent 1 . noteView) sample_items
            ++  [ annotate toSeeAllLabel (cmdToSeeAll mode)
                | count /= sample_total
                ]
      where
        toSeeAllLabel = "To see all " <> Text.pack (show sample_total) <> " task(s), run:"
        count         = genericLength sample_items
        noteView      = if brief then noteViewBrief else noteViewFull
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

noteViewBrief :: Foldable f => EntityF f Note -> Doc Text
noteViewBrief (EntityF fEntityId Note{..}) = fillSep [title, meta]
  where
    meta = foldMap (\i -> "| id" <+> prettyUuid i) fEntityId
    title
        = fillSep
        . map (fillSep . map pretty . Text.split isSpace)
        . take 1
        . Text.lines
        $ Text.pack note_text

noteViewFull :: Foldable f => EntityF f Note -> Doc Text
noteViewFull (EntityF fEntityId Note{..}) =
    sparsedStack [wrapLines $ Text.pack note_text, sep meta]
  where
    meta
        = concat
            [ ["| id"    <+> prettyUuid eid | eid <- toList fEntityId]
            , ["| start" <+> viaShow @Day note_start]
            , ["| end"   <+> viaShow @Day e | Just e <- [note_end]]
            ]
        ++  [ "| tracking" <+> pretty track_url
            | Just Track{..} <- [note_track]
            ]

contactViewFull :: Entity Contact -> Doc Text
contactViewFull (Entity entityId Contact{..}) =
    sep [pretty contact_name, meta]
  where
    meta = "| id" <+> prettyUuid entityId

wrapLines :: Text -> Doc Text
wrapLines =
    vsep . map (fillSep . map pretty . Text.split isSpace) . Text.splitOn "\n"

sparsedStack :: [Doc Text] -> Doc Text
sparsedStack = vsep . intersperse space

stack' :: Bool -> [Doc Text] -> Doc Text
stack' brief
    | brief     = vsep
    | otherwise = sparsedStack

sampleFmap :: (a -> b) -> Sample a -> Sample b
sampleFmap f sample@Sample{sample_items} =
    sample{sample_items = map f sample_items}

star :: Doc ann
star = "*"
