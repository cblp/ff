{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Options (
    Cmd (..),
    CmdAction (..),
    Config (..),
    Contact (..),
    DataDir (..),
    Edit (..),
    MaybeClear (..),
    New (..),
    Options (..),
    Search (..),
    Shuffle (..),
    Track (..),
    maybeClearToMaybe,
    parseOptions,
) where

import           Control.Applicative (optional, some, (<|>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Time (Day)
import           Options.Applicative (Completer, argument, auto, command,
                                      completer, customExecParser, defaultPrefs,
                                      flag', fullDesc, help, helper, info,
                                      listIOCompleter, long, metavar, option,
                                      prefDisambiguate, prefMultiSuffix,
                                      prefShowHelpOnError, progDesc, short, str,
                                      strArgument, strOption, subparser, switch,
                                      (<**>))
import           RON.Storage.IO (Collection, DocId (DocId), getDocuments,
                                 runStorage)
import qualified RON.Storage.IO as Storage

import           FF.Types (ContactId, Limit, Note, NoteId)
import qualified FF.Types

data Cmd
    = CmdConfig (Maybe Config)
    | CmdAction CmdAction
    | CmdVersion

data CmdAction
    = CmdAgenda     (Maybe Limit)
    | CmdContact    (Maybe Contact)
    | CmdDelete     [NoteId]
    | CmdDone       [NoteId]
    | CmdEdit       Edit
    | CmdNew        New
    | CmdPostpone   [NoteId]
    | CmdSearch     Search
    | CmdShow       [NoteId]
    | CmdTrack      Track
    | CmdUnarchive  [NoteId]
    | CmdUpgrade
    | CmdWiki       (Maybe Limit)

data Options = Options
    { brief     :: Bool
    , customDir :: Maybe FilePath
    , cmd       :: Cmd
    }

data Track = Track
    { dryRun  :: Bool
    , address :: Maybe Text
    , limit   :: Maybe Limit
    }

data Contact = Add Text | Delete ContactId

data Config = ConfigDataDir (Maybe DataDir) | ConfigUI (Maybe Shuffle)

data DataDir = DataDirJust FilePath | DataDirYandexDisk

data Shuffle = Shuffle | Sort

data MaybeClear a = Clear | Set a
    deriving (Show)

maybeClearToMaybe :: MaybeClear a -> Maybe a
maybeClearToMaybe = \case
    Clear -> Nothing
    Set x -> Just x

data Edit = Edit
    { ids   :: NonEmpty NoteId
    , text  :: Maybe Text
    , start :: Maybe Day
    , end   :: Maybe (MaybeClear Day)
    }
    deriving (Show)

data New = New
    { text   :: Text
    , start  :: Maybe Day
    , end    :: Maybe Day
    , isWiki :: Bool
    }

data Search = Search
    { text       :: Text
    , inTasks    :: Bool
    , inWikis    :: Bool
    , inContacts :: Bool
    , inArchived :: Bool
    , limit      :: Maybe Limit
    }

parseOptions :: Storage.Handle -> IO Options
parseOptions h =
    customExecParser prefs $ i parser "A note taker and task tracker"
  where
    prefs = defaultPrefs
        { prefDisambiguate    = True
        , prefMultiSuffix     = "..."
        , prefShowHelpOnError = True
        }
    parser =
        Options
        <$> briefOption
        <*> customDirOption
        <*> (version <|> subparser commands <|> (CmdAction <$> cmdAgenda))
    commands = mconcat
        [ action  "add"       iCmdAdd
        , action  "agenda"    iCmdAgenda
        , command "config"    iCmdConfig
        , action  "contact"   iCmdContact
        , action  "delete"    iCmdDelete
        , action  "done"      iCmdDone
        , action  "edit"      iCmdEdit
        , action  "new"       iCmdNew
        , action  "postpone"  iCmdPostpone
        , action  "search"    iCmdSearch
        , action  "show"      iCmdShow
        , action  "track"     iCmdTrack
        , action  "unarchive" iCmdUnarchive
        , action  "upgrade"   iCmdUpgrade
        , action  "wiki"      iCmdWiki
        ]
      where
        action s = command s . fmap CmdAction

    iCmdAdd       = i cmdNew        "add new task or note"
    iCmdAgenda    = i cmdAgenda     "show what you can do right now\
                                    \ [default action]"
    iCmdConfig    = i cmdConfig     "show/edit configuration"
    iCmdContact   = i cmdContact    "show contacts"
    iCmdDelete    = i cmdDelete     "delete a task"
    iCmdDone      = i cmdDone       "mark a task done (archive)"
    iCmdEdit      = i cmdEdit       "edit a task or a note"
    iCmdNew       = i cmdNew        "synonym for `add`"
    iCmdPostpone  = i cmdPostpone   "make a task start later"
    iCmdSearch    = i cmdSearch     "search for notes with the given text"
    iCmdShow      = i cmdShow       "show note by id"
    iCmdTrack     = i cmdTrack      "track issues from external sources"
    iCmdUnarchive = i cmdUnarchive  "restore the note from archive"
    iCmdUpgrade   = i cmdUpgrade    "check and upgrade the database to the most\
                                    \ recent format"
    iCmdWiki      = i cmdWiki       "show all wiki notes"

    cmdAgenda    = CmdAgenda    <$> optional limitOption
    cmdContact   = CmdContact   <$> optional contact
    cmdDelete    = CmdDelete    <$> some noteid
    cmdDone      = CmdDone      <$> some noteid
    cmdEdit      = CmdEdit      <$> edit
    cmdNew       = CmdNew       <$> new
    cmdPostpone  = CmdPostpone  <$> some noteid
    cmdSearch    = CmdSearch    <$> search
    cmdShow      = CmdShow      <$> some noteid
    cmdTrack     = CmdTrack     <$> track
    cmdUnarchive = CmdUnarchive <$> some noteid
    cmdUpgrade   = pure CmdUpgrade
    cmdWiki      = CmdWiki      <$> optional limitOption

    wiki = switch $ long "wiki" <> short 'w' <> help "Handle wiki note"
    briefOption = switch $
        long "brief" <> short 'b' <> help "List only note titles and ids"
    track = Track
        <$> dryRunOption
        <*> optional repo
        <*> optional limitOption
    dryRunOption = switch
        $  long "dry-run"
        <> short 'd'
        <> help "List only issues, don't set up tracking"
    repo = strOption
        $  long "repo"
        <> short 'r'
        <> metavar "USER/REPO"
        <> help "User or organization/repository"
    contact = subparser $ command "add" iAdd <> command "delete" iDelete
      where
        iAdd = i pAdd "Add contact"
        iDelete = i pDelete "Delete contact"
        pAdd =
            Add <$> strArgument (metavar "CONTACT_NAME" <> help "contact name")
        pDelete = Delete <$> argument readDocId
            (metavar "CONTACT_ID" <> help "contact id"
            <> completer completeContactIds)
    new = New
        <$> noteTextArgument
        <*> optional startDateOption
        <*> optional endDateOption
        <*> wiki
    edit = Edit
        <$> (NonEmpty.fromList <$> some noteid)
        <*> optional noteTextOption
        <*> optional startDateOption
        <*> optional maybeClearEnd
    search = Search
        <$> strArgument (metavar "TEXT")
        <*> searchT
        <*> searchW
        <*> searchC
        <*> searchA
        <*> optional limitOption
    searchT = switch $ long "tasks" <> short 't' <> help "Search among tasks"
    searchW = switch $ long "wiki" <> short 'w' <> help "Search among wiki"
    searchC =
        switch $ long "contacts" <> short 'c' <> help "Search among contacts"
    searchA = switch $ long "archived" <> short 'a' <> help "Search among deleted"
    noteid = argument readDocId $
        metavar "ID" <> help "note id" <> completer completeNoteIds
    noteTextArgument = strArgument $ metavar "TEXT" <> help "note text"
    endDateOption = dateOption $ long "end" <> short 'e' <> help "end date"
    limitOption =
        option auto $ long "limit" <> short 'l' <> help "Number of issues"
    startDateOption =
        dateOption $ long "start" <> short 's' <> help "start date"
    noteTextOption = strOption $
        long "text" <> short 't' <> help "note text" <> metavar "TEXT"
    maybeClearEnd
        =   Set <$> endDateOption
        <|> flag' Clear (long "end-clear" <> help "clear end date")

    dateOption m = option auto $ metavar "DATE" <> m

    customDirOption = optional $ strOption
        $  long "data-dir"
        <> short 'C'
        <> metavar "DIRECTORY"
        <> help "Path to the data dir"

    cmdConfig = fmap CmdConfig . optional $
        subparser $ command "dataDir" iDataDir <> command "ui" iUi
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = ConfigDataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust = DataDirJust <$> strArgument (metavar "DIRECTORY" <> help "path")
            pYandexDisk = flag'
                DataDirYandexDisk
                (long "yandex-disk" <> short 'y' <> help "detect Yandex.Disk")
        iUi = i ui "UI tweaks"
        ui = fmap ConfigUI . optional $
            flag'
                Shuffle
                (long "shuffle" <> help "shuffle notes in section") <|>
            flag' Sort (long "sort" <> help "sort notes in section")

    version = flag'
        CmdVersion
        (long "version" <> short 'V' <> help "Current ff-note version")

    i prsr desc = info (prsr <**> helper) $ fullDesc <> progDesc desc

    completeNoteIds = docIdCompleter @Note

    completeContactIds = docIdCompleter @FF.Types.Contact

    docIdCompleter :: forall a . Collection a => Completer
    docIdCompleter =
        listIOCompleter $ map unDocId <$> runStorage h (getDocuments @_ @a)

    unDocId (DocId name) = name

    readDocId = DocId <$> str
