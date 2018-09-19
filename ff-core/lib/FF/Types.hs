{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module FF.Types where

import           CRDT.Cv.Max (Max)
import qualified CRDT.Cv.Max as Max
import           CRDT.Cv.RGA (RgaString)
import qualified CRDT.Cv.RGA as RGA
import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Aeson (camelTo2)
import           Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import           Data.Map.Strict (Map)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semigroup.Generic (gmappend)
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           FF.CrdtAesonInstances ()
import           FF.Storage (DocId)

data Status = Active | Archived | Deleted
    deriving (Bounded, Enum, Eq, Show)

data NoteStatus = TaskStatus Status | Wiki
    deriving (Eq, Show)

data Tracked = Tracked
    { trackedProvider   :: Text
    , trackedSource     :: Text
    , trackedExternalId :: Text
    , trackedUrl        :: Text
    }
    deriving (Eq, Show, Ord)

deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Tracked

data Contact = Contact
    { contactStatus :: LWW Status
    , contactName   :: RgaString
    }
    deriving (Eq, Show, Generic)

data Note = Note
    { noteStatus  :: LWW NoteStatus
    , noteText    :: RgaString
    , noteStart   :: LWW Day
    , noteEnd     :: LWW (Maybe Day)
    , noteTracked :: Maybe (Max Tracked)
    }
    deriving (Eq, Generic, Show)

type NoteId = DocId Note

type ContactId = DocId Contact

instance Semigroup Note where
    (<>) = gmappend

instance Semilattice Note

instance Semigroup Contact where
    (<>) = gmappend

instance Semilattice Contact

data NoteView = NoteView
    { nid     :: Maybe NoteId
    , status  :: NoteStatus
    , text    :: Text
    , start   :: Day
    , end     :: Maybe Day
    , tracked :: Maybe Tracked
    }
    deriving (Eq, Show)

data ContactView = ContactView
    { contactViewId     :: ContactId
    , contactViewStatus :: Status
    , contactViewName   :: Text
    }
    deriving (Eq, Show)

-- | Sub-status of an 'Active' task from the perspective of the user.
data TaskMode
    = Overdue Natural   -- ^ end in past, with days
    | EndToday          -- ^ end today
    | EndSoon Natural   -- ^ started, end in future, with days
    | Actual            -- ^ started, no end
    | Starting Natural  -- ^ starting in future, with days
    deriving (Eq, Show)

taskModeOrder :: TaskMode -> Int
taskModeOrder = \case
    Overdue _  -> 0
    EndToday   -> 1
    EndSoon _  -> 2
    Actual     -> 3
    Starting _ -> 4

instance Ord TaskMode where
    Overdue  n <= Overdue  m = n >= m
    EndSoon  n <= EndSoon  m = n <= m
    Starting n <= Starting m = n <= m
    m1         <= m2         = taskModeOrder m1 <= taskModeOrder m2

type ModeMap = Map TaskMode

noteView :: NoteId -> Note -> NoteView
noteView nid Note {..} = NoteView
    { nid     = Just nid
    , status  = LWW.query noteStatus
    , text    = rgaToText noteText
    , start   = LWW.query noteStart
    , end     = LWW.query noteEnd
    , tracked = Max.query <$> noteTracked
    }

contactView :: ContactId -> Contact -> ContactView
contactView contactId Contact {..} = ContactView
    { contactViewId     = contactId
    , contactViewStatus = LWW.query contactStatus
    , contactViewName   = rgaToText contactName
    }

type Limit = Natural

rgaFromText :: Clock m => Text -> m RgaString
rgaFromText = RGA.fromString . Text.unpack

rgaToText :: RgaString -> Text
rgaToText = Text.pack . RGA.toString
