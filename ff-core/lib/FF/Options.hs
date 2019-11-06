{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FF.Options
  ( Agenda (..),
    Assign (..),
    Cmd (..),
    CmdAction (..),
    Config (..),
    Contact (..),
    DataDir (..),
    Edit (..),
    New (..),
    Options (..),
    Search (..),
    Shuffle (..),
    Tags (..),
    Track (..),
    assignToMaybe,
    parseOptions,
    showHelp,
  )
where

import Control.Applicative ((<|>), many, optional)
import Data.List.NonEmpty (NonEmpty, some1)
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (Day)
import FF.Types (ContactId, Limit, Note, NoteId, Status (Active, Archived))
import qualified FF.Types
import Options.Applicative
  ( (<**>),
    Completer,
    InfoMod,
    ParseError (ShowHelpText),
    Parser,
    ParserInfo,
    ParserPrefs,
    argument,
    auto,
    command,
    completer,
    customExecParser,
    defaultPrefs,
    flag,
    flag',
    footer,
    fullDesc,
    help,
    helper,
    info,
    listIOCompleter,
    long,
    metavar,
    option,
    parserFailure,
    prefDisambiguate,
    prefMultiSuffix,
    prefShowHelpOnError,
    progDesc,
    renderFailure,
    short,
    str,
    strArgument,
    strOption,
    subparser,
    switch,
  )
import RON.Storage.Backend (DocId (DocId), getDocuments)
import RON.Storage.FS (Collection, runStorage)
import qualified RON.Storage.FS as StorageFS

data Cmd
  = CmdConfig (Maybe Config)
  | CmdAction CmdAction
  | CmdVersion

data CmdAction
  = CmdAgenda Agenda
  | CmdContact (Maybe Contact)
  | CmdDelete (NonEmpty NoteId)
  | CmdDone (NonEmpty NoteId)
  | CmdEdit Edit
  | CmdNew New
  | CmdPostpone (NonEmpty NoteId)
  | CmdSearch Search
  | CmdShow (NonEmpty NoteId)
  | CmdSponsors
  | CmdTags
  | CmdTrack Track
  | CmdUnarchive (NonEmpty NoteId)
  | CmdUpgrade
  | CmdWiki (Maybe Limit)

data Options
  = Options
      { brief :: Bool,
        customDir :: Maybe FilePath,
        cmd :: Cmd
      }

data Track
  = Track
      { dryRun :: Bool,
        address :: Maybe Text,
        limit :: Maybe Limit
      }

data Contact = Add Text | Delete ContactId

data Config = ConfigDataDir (Maybe DataDir) | ConfigUI (Maybe Shuffle)

data DataDir = DataDirJust FilePath | DataDirYandexDisk

data Shuffle = Shuffle | Sort

data Assign a = Clear | Set a
  deriving (Show)

assignToMaybe :: Assign a -> Maybe a
assignToMaybe = \case
  Clear -> Nothing
  Set x -> Just x

data Agenda
  = Agenda
      { limit :: Maybe Limit,
        tags :: Tags,
        withoutTags :: Set Text
      }

data Tags = Tags (Set Text) | NoTags

data Edit
  = Edit
      { ids :: NonEmpty NoteId,
        text :: Maybe Text,
        start :: Maybe Day,
        end :: Maybe (Assign Day),
        addTags :: Set Text,
        deleteTags :: Set Text
      }
  deriving (Show)

data New
  = New
      { text :: Text,
        start :: Maybe Day,
        end :: Maybe Day,
        isWiki :: Bool,
        tags :: Set Text
      }

data Search
  = Search
      { text :: Text,
        inTasks :: Bool,
        inWikis :: Bool,
        inContacts :: Bool,
        status :: Status,
        limit :: Maybe Limit,
        tags :: Tags,
        withoutTags :: Set Text
      }

parseOptions :: Maybe StorageFS.Handle -> IO Options
parseOptions = customExecParser prefs . parserInfo

prefs :: ParserPrefs
prefs =
  defaultPrefs
    { prefDisambiguate = True,
      -- TODO prefHelpLongEquals = True,
      prefMultiSuffix = "...",
      prefShowHelpOnError = True
    }

parser :: Maybe StorageFS.Handle -> Parser Options
parser h =
  Options
    <$> briefOption
    <*> customDirOption
    <*> (version <|> subparser commands <|> (CmdAction <$> cmdAgenda))
  where
    commands =
      mconcat
        [ action "add" iCmdAdd,
          action "agenda" iCmdAgenda,
          command "config" iCmdConfig,
          action "contact" iCmdContact,
          action "delete" iCmdDelete,
          action "done" iCmdDone,
          action "edit" iCmdEdit,
          action "new" iCmdNew,
          action "postpone" iCmdPostpone,
          action "search" iCmdSearch,
          action "show" iCmdShow,
          action "tags" iCmdTags,
          action "sponsors" iCmdSponsors,
          action "track" iCmdTrack,
          action "unarchive" iCmdUnarchive,
          action "upgrade" iCmdUpgrade,
          action "wiki" iCmdWiki
        ]
      where
        action s = command s . fmap CmdAction
    iCmdAdd = i cmdNew "add new task or note"
    iCmdAgenda = i cmdAgenda "show what you can do right now [default action]"
    iCmdConfig = i cmdConfig "show/edit configuration"
    iCmdContact = i cmdContact "show contacts"
    iCmdDelete = i cmdDelete "delete a task"
    iCmdDone = i cmdDone "mark a task done (archive)"
    iCmdEdit =
      i_
        cmdEdit
        "edit a task or a note, using command from environment variable EDITOR\
        \ or program `editor`"
        $ footer
          "Examples for EDITOR: 'code --wait', 'emacs', 'micro', 'nano', 'vim'"
    iCmdNew = i cmdNew "synonym for `add`"
    iCmdPostpone = i cmdPostpone "make a task start later"
    iCmdSearch = i cmdSearch "search for notes with the given text"
    iCmdShow = i cmdShow "show note by id"
    iCmdTags = i cmdTags "show tags of all notes"
    iCmdSponsors = i cmdSponsors "show project sponsors"
    iCmdTrack = i cmdTrack "track issues from external sources"
    iCmdUnarchive = i cmdUnarchive "restore the note from archive"
    iCmdUpgrade =
      i cmdUpgrade "check and upgrade the database to the most recent format"
    iCmdWiki = i cmdWiki "show all wiki notes"
    cmdAgenda = CmdAgenda <$> agenda
    cmdContact = CmdContact <$> optional contact
    cmdDelete = CmdDelete <$> some1 noteid
    cmdDone = CmdDone <$> some1 noteid
    cmdEdit = CmdEdit <$> edit
    cmdNew = CmdNew <$> new
    cmdPostpone = CmdPostpone <$> some1 noteid
    cmdSearch = CmdSearch <$> search
    cmdShow = CmdShow <$> some1 noteid
    cmdSponsors = pure CmdSponsors
    cmdTags = pure CmdTags
    cmdTrack = CmdTrack <$> track
    cmdUnarchive = CmdUnarchive <$> some1 noteid
    cmdUpgrade = pure CmdUpgrade
    cmdWiki = CmdWiki <$> optional limitOption
    wiki = switch $ long "wiki" <> short 'w' <> help "Handle wiki note"
    briefOption =
      switch $ long "brief" <> short 'b' <> help "List only note titles and ids"
    agenda = Agenda <$> optional limitOption <*> filterTags <*> withoutTagsOption
    filterTags = filterByNoTags <|> Tags <$> filterByTags
    track = Track <$> dryRunOption <*> optional repo <*> optional limitOption
    dryRunOption =
      switch $
        long "dry-run"
          <> short 'd'
          <> help "List only issues, don't set up tracking"
    repo =
      strOption $
        long "repo"
          <> short 'r'
          <> metavar "USER/REPO"
          <> help "User or organization/repository"
    contact = subparser $ command "add" iAdd <> command "delete" iDelete
      where
        iAdd = i pAdd "Add contact"
        iDelete = i pDelete "Delete contact"
        pAdd =
          Add <$> strArgument (metavar "CONTACT_NAME" <> help "contact name")
        pDelete =
          Delete
            <$> argument
              readDocId
              ( metavar "CONTACT_ID"
                  <> help "contact id"
                  <> completer completeContactIds
              )
    new =
      New
        <$> noteTextArgument
        <*> optional startDateOption
        <*> optional endDateOption
        <*> wiki
        <*> addTagsOption
    edit =
      Edit
        <$> some1 noteid
        <*> optional noteTextOption
        <*> optional startDateOption
        <*> optional assignEnd
        <*> addTagsOption
        <*> deleteTagsOption
    search =
      Search
        <$> strArgument (metavar "TEXT")
        <*> searchT
        <*> searchW
        <*> searchC
        <*> searchA
        <*> optional limitOption
        <*> filterTags
        <*> withoutTagsOption
    searchT = switch $ long "tasks" <> short 't' <> help "Search among tasks"
    searchW = switch $ long "wiki" <> short 'w' <> help "Search among wiki"
    searchC =
      switch $ long "contacts" <> short 'c' <> help "Search among contacts"
    searchA =
      flag Active Archived $
        long "archived" <> short 'a' <> help "Search among archived"
    noteid =
      argument readDocId $
        metavar "ID" <> help "note id" <> completer completeNoteIds
    noteTextArgument = strArgument $ metavar "TEXT" <> help "Note's text"
    filterByTags =
      fmap Set.fromList $ many $ strOption $
        long "tag" <> metavar "TAG" <> help "Filter by tag"
    filterByNoTags =
      flag' NoTags $
        long "no-tag"
          <> short 'n'
          <> help "Filter items that has no tags"
    addTagsOption =
      fmap Set.fromList $ many $ strOption $
        long "tag" <> metavar "TAG" <> help "Add tag"
    deleteTagsOption =
      fmap Set.fromList $ many $ strOption $
        long "delete-tag" <> short 'd' <> metavar "TAG" <> help "Delete tag"
    withoutTagsOption =
      fmap Set.fromList $ many $ strOption $
        long "without-tag" <> metavar "TAG" <> help "Filter items without tag"
    endDateOption = dateOption $ long "end" <> short 'e' <> help "end date"
    limitOption =
      option auto $ long "limit" <> short 'l' <> help "Number of issues"
    startDateOption =
      dateOption $ long "start" <> short 's' <> help "start date"
    noteTextOption =
      strOption $ long "text" <> short 't' <> help "note text" <> metavar "TEXT"
    assignEnd =
      Set <$> endDateOption
        <|> flag' Clear (long "end-clear" <> help "clear end date")
    dateOption m = option auto $ metavar "DATE" <> m
    customDirOption =
      optional $ strOption $
        long "data-dir"
          <> short 'C'
          <> metavar "DIRECTORY"
          <> help "Path to the data dir"
    cmdConfig =
      fmap CmdConfig . optional . subparser $
        command "dataDir" iDataDir <> command "ui" iUi
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = ConfigDataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust =
              DataDirJust <$> strArgument (metavar "DIRECTORY" <> help "path")
            pYandexDisk =
              flag'
                DataDirYandexDisk
                (long "yandex-disk" <> short 'y' <> help "detect Yandex.Disk")
        iUi = i ui "UI tweaks"
        ui =
          fmap ConfigUI . optional $
            flag'
              Shuffle
              (long "shuffle" <> help "shuffle notes in section")
              <|> flag' Sort (long "sort" <> help "sort notes in section")
    version =
      flag'
        CmdVersion
        (long "version" <> short 'V' <> help "Current ff-note version")
    completeNoteIds = docIdCompleter @Note
    completeContactIds = docIdCompleter @FF.Types.Contact
    docIdCompleter :: forall a. Collection a => Completer
    docIdCompleter = listIOCompleter $
      map unDocId <$> case h of
        Nothing -> pure []
        Just h' -> runStorage h' (getDocuments @_ @a)
    unDocId (DocId name) = name
    readDocId = DocId <$> str

i :: Parser a -> String -> ParserInfo a
i prsr desc = i_ prsr desc mempty

i_ :: Parser a -> String -> InfoMod a -> ParserInfo a
i_ prsr desc m = info (prsr <**> helper) $ fullDesc <> progDesc desc <> m

parserInfo :: Maybe StorageFS.Handle -> ParserInfo Options
parserInfo h = i (parser h) "A note taker and task tracker"

showHelp :: String
showHelp =
  fst $
    renderFailure
      (parserFailure prefs (parserInfo Nothing) ShowHelpText mempty)
      ""
