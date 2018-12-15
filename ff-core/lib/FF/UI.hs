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
    prettyNotes,
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
import           Data.Time (Day)
import           RON.Text.Serialize (serializeUuid)
import           RON.Types (UUID)
import           Text.PrettyPrint.Mainland (Doc, hang, indent, sep,
                                            space, spread, stack, star,
                                            strictText, string, (<+/>), (<+>),
                                            (</>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Types (Contact (..), ContactSample, Entity, pattern Entity,
                           EntityF (..), ModeMap, Note (..), NoteSample,
                           Sample (..), TaskMode (..), Track (..), omitted)

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 2

pshow :: Show a => a -> Doc
pshow = string . show

prettyUuid :: UUID -> Doc
prettyUuid = Pretty.lazyText . TextL.decodeUtf8 . serializeUuid

prettyNotesWikiContacts
    :: Bool  -- ^ brief output
    -> ModeMap NoteSample
    -> NoteSample
    -> ContactSample
    -> Bool  -- ^ search among tasks
    -> Bool  -- ^ search among wiki notes
    -> Bool  -- ^ search among contacts
    -> Doc
prettyNotesWikiContacts brief notes wiki contacts amongN amongW amongC =
    case (amongN, amongW, amongC) of
        (True,  False, False) -> ns
        (False, True,  False) -> ws
        (False, False, True ) -> cs
        (True,  True,  False) -> ns </> ws
        (False, True,  True ) -> ws </> cs
        (True,  False, True ) -> ns </> cs
        (_,     _,     _    ) -> ns </> ws </> cs
  where
    ns = prettySamplesBySections brief notes
    ws = prettyWikiSamplesOmitted brief wiki
    cs = prettyContactSamplesOmitted brief contacts

prettyContactSamplesOmitted :: Bool -> ContactSample -> Doc
prettyContactSamplesOmitted brief samples = stack' brief $
    prettyContactSample brief samples :
    [Pretty.text $ show numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples

prettyContactSample :: Bool -> ContactSample -> Doc
prettyContactSample brief = \case
    Sample{sample_total = 0} -> mempty
    Sample{sample_items} ->
        withHeader "Contacts:" . stack' brief $
        map ((star <>) . indent 1 . contactViewFull) sample_items

prettyWikiSamplesOmitted :: Bool -> NoteSample -> Doc
prettyWikiSamplesOmitted brief samples = stack' brief $
    prettyWikiSample brief samples :
    [Pretty.text $ show numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples

prettyNotes :: Foldable f => Bool -> [EntityF f Note] -> Doc
prettyNotes brief = stack' brief . map ((star <>) . indent 1 . noteView brief)

prettyWikiSample :: Bool -> NoteSample -> Doc
prettyWikiSample brief = \case
    Sample{sample_total = 0} -> mempty
    Sample{sample_items} ->
        withHeader "Wiki notes:" .
        stack' brief $
        map ((star <>) . indent 1 . noteView brief) sample_items

noteView :: Foldable f => Bool -> EntityF f Note -> Doc
noteView brief = if brief then noteViewBrief else noteViewFull

prettySamplesBySections
    :: Foldable f => Bool -> ModeMap (Sample (EntityF f Note)) -> Doc
prettySamplesBySections brief samples = stack' brief
    $   [prettySample brief mode sample | (mode, sample) <- Map.assocs samples]
    ++  [Pretty.text $ show numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettySample :: Foldable f => Bool -> TaskMode -> Sample (EntityF f Note) -> Doc
prettySample brief mode = \case
    Sample{sample_total = 0} -> mempty
    Sample{sample_total, sample_items} ->
        withHeader (sampleLabel mode) . stack' brief $
            map ((star <>) . indent 1 . noteView brief) sample_items
            ++  [ toSeeAllLabel .= Pretty.text (cmdToSeeAll mode)
                | count /= sample_total
                ]
      where
        toSeeAllLabel = "To see all " <> show sample_total <> " task(s), run:"
        count         = genericLength sample_items
  where
    cmdToSeeAll = \case
        Overdue _  -> "ff search --overdue"
        EndToday   -> "ff search --today"
        EndSoon _  -> "ff search --soon"
        Actual     -> "ff search --actual"
        Starting _ -> "ff search --starting"

sampleLabel :: TaskMode -> String
sampleLabel = \case
    Overdue n -> case n of
        1 -> "1 day overdue:"
        _ -> show n <> " days overdue:"
    EndToday -> "Due today:"
    EndSoon n -> case n of
        1 -> "Due tomorrow:"
        _ -> "Due in " <> show n <> " days:"
    Actual -> "Actual:"
    Starting n -> case n of
        1 -> "Starting tomorrow:"
        _ -> "Starting in " <> show n <> " days:"

noteViewBrief :: Foldable f => EntityF f Note -> Doc
noteViewBrief (EntityF fEntityId Note{..}) = title <+/> meta
  where
    meta = foldMap (\i -> "| id" <+> prettyUuid i) fEntityId
    title
        = stack
        . map (sep . map strictText . Text.split isSpace)
        . take 1
        . Text.lines
        $ Text.pack note_text

noteViewFull :: Foldable f => EntityF f Note -> Doc
noteViewFull (EntityF fEntityId Note{..}) =
    sparsedStack [wrapLines $ Text.pack note_text, sep meta]
  where
    meta
        = concat
            [ ["| id"    <+> prettyUuid eid | eid <- toList fEntityId]
            , ["| start" <+> pshow @Day note_start]
            , ["| end"   <+> pshow @Day e | Just e <- [note_end]]
            ]
        ++  [ "| tracking" <+> strictText track_url
            | Just Track{..} <- [note_track]
            ]

contactViewFull :: Entity Contact -> Doc
contactViewFull (Entity entityId Contact{..}) =
    spread [string contact_name, meta]
  where
    meta = "| id" <+> prettyUuid entityId

wrapLines :: Text -> Doc
wrapLines =
    stack . map (sep . map strictText . Text.split isSpace) . Text.splitOn "\n"

sparsedStack :: [Doc] -> Doc
sparsedStack = stack . intersperse space

stack' :: Bool -> [Doc] -> Doc
stack' brief
    | brief     = stack
    | otherwise = sparsedStack

sampleFmap :: (a -> b) -> Sample a -> Sample b
sampleFmap f sample@Sample{sample_items} =
    sample{sample_items = map f sample_items}
