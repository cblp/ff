{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module FF.UI where

import           Data.Char (isSpace)
import           Data.List (genericLength, intersperse)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day)
import           Text.PrettyPrint.Mainland (Doc, hang, indent, sep, space,
                                            spread, stack, star, strictText,
                                            string, (<+/>), (<+>), (</>))
import qualified Text.PrettyPrint.Mainland as Pretty
import           Text.PrettyPrint.Mainland.Class (Pretty, ppr)

import           FF.Storage (rawDocId)
import           FF.Types (ContactSample, ContactView (..), ModeMap, NoteSample,
                           NoteView (..), Sample (..), TaskMode (..),
                           Tracked (..), omitted)

type Template a = a -> String

(.=) :: Pretty a => String -> a -> Doc
label .= value = hang indentation $ Pretty.text label <+/> ppr value

withHeader :: Pretty a => String -> a -> Doc
withHeader header value = hang indentation $ Pretty.text header </> ppr value

indentation :: Int
indentation = 2

pshow :: Show a => a -> Doc
pshow = string . show

prettyContactSamplesOmitted :: ContactSample -> Doc
prettyContactSamplesOmitted samples = sparsedStack $
    prettyContactSample samples :
    [Pretty.text $ show numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = omitted samples

prettyContactSample :: ContactSample -> Doc
prettyContactSample = \case
    Sample{total = 0} -> mempty
    Sample{docs} ->
        withHeader "Contacts:" . sparsedStack $
        map ((star <>) . indent 1 . contactViewFull) docs

prettySamplesBySections :: Bool -> ModeMap NoteSample -> Doc
prettySamplesBySections brief samples = stack' brief $
    [prettySample brief mode sample | (mode, sample) <- Map.assocs samples] ++
    [Pretty.text $ show numOmitted <> " task(s) omitted" | numOmitted > 0]
  where
    numOmitted = sum $ fmap omitted samples

prettySample :: Bool -> TaskMode -> NoteSample -> Doc
prettySample brief mode = \case
    Sample{total = 0} -> mempty
    Sample{total, docs} ->
        withHeader (sampleLabel mode) . stack' brief $
            map ((star <>) . indent 1 . noteView) docs
            ++  [ toSeeAllLabel .= Pretty.text (cmdToSeeAll mode)
                | count /= total
                ]
      where
        toSeeAllLabel = "To see all " <> show total <> " task(s), run:"
        count         = genericLength docs
        noteView = if brief then noteViewBrief else noteViewFull
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

noteViewBrief :: NoteView -> Doc
noteViewBrief NoteView{..} = title <+/> meta
  where
    meta = foldMap (\i -> "| id" <+> string (rawDocId i)) nid
    title
        = stack
        . map (sep . map strictText . Text.split isSpace)
        . take 1
        $ Text.lines text

noteViewFull :: NoteView -> Doc
noteViewFull NoteView{..} = sparsedStack [wrapLines text, sep meta]
  where
    meta
        = concat
            [ ["| id"    <+> string (rawDocId i) | Just i <- [nid]]
            , ["| start" <+> pshow @Day start]
            , ["| end"   <+> pshow @Day e | Just e <- [end]]
            ]
        ++  [ "| tracking" <+> strictText trackedUrl
            | Just Tracked{..} <- [tracked]
            ]

contactViewFull :: ContactView -> Doc
contactViewFull ContactView{..} = spread [strictText contactViewName, meta]
  where
    meta = "| id" <+> string (rawDocId contactViewId)

wrapLines :: Text -> Doc
wrapLines =
    stack . map (sep . map strictText . Text.split isSpace) . Text.splitOn "\n"

sparsedStack :: [Doc] -> Doc
sparsedStack = stack . intersperse space

stack' :: Bool -> [Doc] -> Doc
stack' brief
    | brief     = stack
    | otherwise = sparsedStack
