{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module FF.Options
    ( Cmd (..)
    , CmdAction (..)
    , Config (..)
    , DataDir (..)
    , Edit (..)
    , New (..)
    , Options (..)
    , Search (..)
    , Shuffle (..)
    , Track (..)
    , parseOptions
    ) where

import           Control.Applicative (optional, (<|>))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Data.Time (Day)
import           Options.Applicative (auto, command, execParser, flag',
                                      fullDesc, help, helper, info, completer, listIOCompleter, long,
                                      metavar, option, progDesc, short,
                                      strArgument, strOption, subparser, switch,
                                      (<**>))

import           FF.Storage (DocId (DocId), Handle, Storage, listDocuments, rawDocId, runStorage)
import           FF.Types (Limit, NoteId)

data Cmd
    = CmdConfig (Maybe Config)
    | CmdAction CmdAction
    | CmdVersion

data CmdAction
    = CmdAgenda     (Maybe Limit)
    | CmdDelete     NoteId
    | CmdDone       NoteId
    | CmdEdit       Edit
    | CmdNew        New
    | CmdPostpone   NoteId
    | CmdSearch     Search
    | CmdServe
    | CmdTrack      Track
    | CmdUnarchive  NoteId
    | CmdUpgrade

data Options = Options
    { optionBrief :: Bool
    , optionCmd   :: Cmd
    }

data Track = Track
    { trackDryrun  :: Bool
    , trackAddress :: Maybe Text
    , trackLimit   :: Maybe Limit
    }

data Config = ConfigDataDir (Maybe DataDir) | ConfigUI (Maybe Shuffle)

data DataDir = DataDirJust FilePath | DataDirYandexDisk

data Shuffle = Shuffle | Sort

data Edit = Edit
    { editId    :: NoteId
    , editText  :: Maybe Text
    , editStart :: Maybe Day
    , editEnd   :: Maybe (Maybe Day)
    -- ^ Nothing      -- no option             => no change
    --   Just Nothing -- option with tombstone => clear field
    --   Just value   -- option with value     => set field to value
    }
    deriving (Show)

data New = New
    { newText   :: Text
    , newStart  :: Maybe Day
    , newEnd    :: Maybe Day
    , newWiki   :: Bool
    }

data Search = Search
    { searchText  :: Text
    , searchLimit :: Maybe Limit
    }

parseOptions :: Handle -> IO Options
parseOptions h = execParser $ i parser "A note taker and task tracker"
  where
    parser   = Options <$> brief <*>
        (version <|> subparser commands <|> (CmdAction <$> cmdAgenda))
    commands = mconcat
        [ action  "add"       iCmdAdd
        , action  "agenda"    iCmdAgenda
        , command "config"    iCmdConfig
        , action  "delete"    iCmdDelete
        , action  "done"      iCmdDone
        , action  "edit"      iCmdEdit
        , action  "new"       iCmdNew
        , action  "postpone"  iCmdPostpone
        , action  "search"    iCmdSearch
        , action  "serve"     iCmdServe
        , action  "track"     iCmdTrack
        , action  "unarchive" iCmdUnarchive
        , action  "upgrade"   iCmdUpgrade
        ]
      where
        action s = command s . fmap CmdAction

    iCmdAdd       = i cmdNew        "add a new task or note"
    iCmdAgenda    = i cmdAgenda     "show what you can do right now\
                                    \ [default action]"
    iCmdConfig    = i cmdConfig     "show/edit configuration"
    iCmdDelete    = i cmdDelete     "delete a task"
    iCmdDone      = i cmdDone       "mark a task done (archive)"
    iCmdEdit      = i cmdEdit       "edit a task or a note"
    iCmdNew       = i cmdNew        "synonym for `add`"
    iCmdPostpone  = i cmdPostpone   "make a task start later"
    iCmdSearch    = i cmdSearch     "search for notes with the given text"
    iCmdServe     = i cmdServe      "serve web UI"
    iCmdTrack     = i cmdTrack      "track issues from external sources"
    iCmdUnarchive = i cmdUnarchive  "restore the note from archive"
    iCmdUpgrade   = i cmdUpgrade    "check and upgrade the database to the most\
                                    \ recent format"

    cmdAgenda    = CmdAgenda    <$> optional limit
    cmdDelete    = CmdDelete    <$> noteid
    cmdDone      = CmdDone      <$> noteid
    cmdEdit      = CmdEdit      <$> edit
    cmdNew       = CmdNew       <$> new
    cmdPostpone  = CmdPostpone  <$> noteid
    cmdSearch    = CmdSearch    <$> search
    cmdServe     = pure CmdServe
    cmdTrack     = CmdTrack     <$> track
    cmdUnarchive = CmdUnarchive <$> noteid
    cmdUpgrade   = pure CmdUpgrade

    wiki = switch
        (long "wiki" <> short 'w' <>
        help "Handle wiki note")
    brief = switch
        (long "brief" <> short 'b' <>
        help "List only note titles and ids")
    track = Track
        <$> dryRun
        <*> optional repo
        <*> optional limit
    dryRun = switch
        (long "dry-run" <> short 'd' <>
        help "List only issues, don't set up tracking")
    repo = strOption $
        long "repo" <> short 'r' <> metavar "USER/REPO" <>
        help "User or organization/repository"
    new = New <$> text <*> optional start <*> optional end <*> wiki
    edit = Edit
        <$> noteid
        <*> optional textOption
        <*> optional start
        <*> optional maybeEnd
    search = Search <$> strArgument (metavar "TEXT") <*> optional limit
    noteid = DocId <$> strArgument 
        (metavar "ID" <> help "note id" <> completer (ids h))
    text = strArgument $ metavar "TEXT" <> help "note text"
    end = dateOption $ long "end" <> short 'e' <> help "end date"
    limit = option auto $ long "limit" <> short 'l' <> help "Number of issues"
    start = dateOption $ long "start" <> short 's' <> help "start date"
    textOption = strOption $
        long "text" <> short 't' <> help "note text" <> metavar "TEXT"
    maybeEnd =
        Just <$> end <|>
        flag' Nothing (long "end-clear" <> help "clear end date")

    dateOption m = option auto $ metavar "DATE" <> m

    cmdConfig = fmap CmdConfig . optional $
        subparser $ command "dataDir" iDataDir <> command "ui" iUi
      where
        iDataDir = i pDataDir "the database directory"
        pDataDir = ConfigDataDir <$> optional (pJust <|> pYandexDisk)
          where
            pJust = DataDirJust <$> strArgument (metavar "DIR" <> help "path")
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

    ids handle = listIOCompleter $ 
        map rawDocId <$> runStorage handle (listDocuments :: Storage [NoteId])