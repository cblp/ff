{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module FF.Types where

import           Control.Monad ((>=>))
import qualified CRDT.Cv.RGA as CRDT
import qualified CRDT.LamportClock as CRDT
import qualified CRDT.LWW as CRDT
import           Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject,
                             (.:), (.:?))
import qualified Data.Aeson as JSON
import           Data.Aeson.TH (defaultOptions, deriveFromJSON)
import           Data.Aeson.Types (parseEither)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Hashable (Hashable)
import           Data.List (genericLength)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, maybeToList)
import           Data.Text (Text)
import           Data.Time (Day, diffDays)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           RON.Data (Replicated, ReplicatedAsPayload, encoding,
                           fromPayload, payloadEncoding, stateFromChunk,
                           stateToChunk, toPayload)
import           RON.Data.Internal (mkStateChunk)
import           RON.Data.LWW (lwwType)
import           RON.Data.RGA (RgaRaw, rgaType)
import           RON.Data.Time (day)
import           RON.Event (Event (Event), applicationSpecific, encodeEvent,
                            localEpochTimeFromUnix)
import           RON.Schema (Declaration (DStructLww), StructLww (StructLww),
                             atomString, def, field, oaHaskellType, opaqueAtoms,
                             option, rgaString, saHaskellDeriving,
                             saHaskellFieldPrefix, structLww)
import           RON.Schema.TH (mkReplicated)
import           RON.Storage (Collection, DocId, collectionName, fallbackParse)
import           RON.Types (Atom (AUuid), Object (Object), Op (Op), UUID,
                            objectFrame, objectId)
import qualified RON.UUID as UUID

import           FF.CrdtAesonInstances ()

data Status = Active | Archived | Deleted
    deriving (Bounded, Enum, Eq, Show)

active, archived, deleted :: UUID
active   = fromJust $ UUID.mkName "Active"
archived = fromJust $ UUID.mkName "Archived"
deleted  = fromJust $ UUID.mkName "Deleted"

instance Replicated Status where encoding = payloadEncoding

instance ReplicatedAsPayload Status where
    toPayload = toPayload . \case
        Active   -> active
        Archived -> archived
        Deleted  -> deleted

    fromPayload = \case
        [AUuid u]
            | u == active   -> pure Active
            | u == archived -> pure Archived
            | u == deleted  -> pure Deleted
        _ -> fail "Expected single UUID"

data NoteStatus = TaskStatus Status | Wiki
    deriving (Eq, Show)

wiki :: UUID
wiki = fromJust $ UUID.mkName "Wiki"

instance Replicated NoteStatus where encoding = payloadEncoding

instance ReplicatedAsPayload NoteStatus where
    toPayload = \case
        TaskStatus status -> toPayload status
        Wiki              -> toPayload wiki
    fromPayload = \case
        [AUuid u] | u == wiki -> pure Wiki
        p                     -> TaskStatus <$> fromPayload p

{-
EDN version from future:

    (ron_schema_edn [2 0]

        (import Time Day)

        (opaque Status)  ; TODO enum
        (opaque NoteStatus)  ; TODO transparent enum?

        (struct_lww Track
            (provider   String)
            (source     String)
            (externalId String)
            (url        String)
            #Haskell {field_prefix "track_"})

        (struct_lww Contact
            (status Status)
            (name   RgaString)
            #Haskell {field_prefix "contact_"})

        (struct_lww Note
            (status NoteStatus)
            (text   RgaString)
            (start  Day)
            (end    (Option Day))
            (track  (Option Track))
            #Haskell {field_prefix "note_"})
    )
-}

$(let
    status = opaqueAtoms def{oaHaskellType = Just "Status"}
    noteStatus = opaqueAtoms def{oaHaskellType = Just "NoteStatus"}
    track = StructLww "Track"
        [ ("provider",   field atomString)
        , ("source",     field atomString)
        , ("externalId", field atomString)
        , ("url",        field atomString)
        ]
        def { saHaskellDeriving    = ["Eq", "Generic", "Hashable", "Show"]
            , saHaskellFieldPrefix = "track_"
            }
    contact = StructLww "Contact"
        [("status", field status), ("name", field rgaString)]
        def { saHaskellDeriving    = ["Eq", "Show"]
            , saHaskellFieldPrefix = "contact_"
            }
    note = StructLww "Note"
        [ ("status",  field noteStatus)
        , ("text",    field rgaString)
        , ("start",   field day)
        , ("end",     field $ option day)
        , ("track",   field $ option $ structLww track)
        ]
        def{saHaskellDeriving = ["Eq", "Show"], saHaskellFieldPrefix = "note_"}
    in mkReplicated [DStructLww track, DStructLww contact, DStructLww note])

type NoteId = DocId Note

type ContactId = DocId Contact

instance Collection Note where
    collectionName = "note"
    fallbackParse = parseNoteV1

instance Collection Contact where
    collectionName = "contact"
    fallbackParse = parseContactV1

data Sample a = Sample
    { sample_items :: [a]
    , sample_total :: Natural
    }
    deriving (Eq, Show)

type ContactSample = EntitySample Contact

type NoteSample = EntitySample Note

emptySample :: Sample a
emptySample = Sample{sample_items = [], sample_total = 0}

-- | Number of notes omitted from the sample.
omitted :: Sample a -> Natural
omitted Sample{..} = sample_total - genericLength sample_items

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

taskMode :: Day -> Note -> TaskMode
taskMode today Note{note_start, note_end} = case note_end of
    Nothing
        | note_start <= today -> Actual
        | otherwise           -> starting note_start today
    Just e -> case compare e today of
        LT -> overdue today e
        EQ -> EndToday
        GT | note_start <= today -> endSoon e today
           | otherwise           -> starting note_start today
  where
    overdue  = helper Overdue
    endSoon  = helper EndSoon
    starting = helper Starting
    helper m x y = m . fromIntegral $ diffDays x y

type ModeMap = Map TaskMode

type Limit = Natural

data EntityF f a = EntityF{entityId :: f UUID, entityVal :: a}
deriving instance (Eq a, Eq (f UUID)) => Eq (EntityF f a)
deriving instance (Show a, Show (f UUID)) => Show (EntityF f a)

-- TODO(2018-10-26, cblp) remove redundant morphism, on `track --dry-run` show
-- temporary UUIDs.
type Entity = EntityF Identity

pattern Entity :: UUID -> a -> Entity a
pattern Entity uuid a = EntityF (Identity uuid) a
{-# COMPLETE Entity #-}

type EntitySample a = Sample (Entity a)

-- * Legacy, v1

parseContactV1 :: UUID -> ByteString -> Either String (Object Contact)
parseContactV1 = undefined

parseNoteV1 :: UUID -> ByteString -> Either String (Object Note)
parseNoteV1 objectId = eitherDecode >=> parseEither p where

    p = withObject "Note" $ \obj -> do
        CRDT.LWW (end    :: Maybe Day) endTime    <- obj .:  "end"
        CRDT.LWW (start  :: Day)       startTime  <- obj .:  "start"
        CRDT.LWW (status :: Status)    statusTime <- obj .:  "status"
        (mTracked :: Maybe JSON.Object)           <- obj .:? "tracked"
        text :: CRDT.RgaString <- obj .: "text"
        let endTime'    = timeFromV1 endTime
            startTime'  = timeFromV1 startTime
            statusTime' = timeFromV1 statusTime
        let trackPayload = toPayload $ mTracked $> trackId
        mTrackObject <- case mTracked of
            Nothing -> pure Nothing
            Just tracked -> do
                externalId :: Text <- tracked .: "external_id"
                provider   :: Text <- tracked .: "provider"
                source     :: Text <- tracked .: "source"
                url        :: Text <- tracked .: "url"
                pure $ Just
                    ( (lwwType, trackId)
                    , mkStateChunk
                        [ Op trackId externalIdName $ toPayload externalId
                        , Op trackId providerName   $ toPayload provider
                        , Op trackId sourceName     $ toPayload source
                        , Op trackId urlName        $ toPayload url
                        ]
                    )
        let objectFrame = Map.fromList
                $   [   ( (lwwType, objectId)
                        , mkStateChunk
                            [ Op endTime'    endName    $ toPayload end
                            , Op startTime'  startName  $ toPayload start
                            , Op statusTime' statusName $ toPayload status
                            , Op objectId    textName   $ toPayload textId
                            , Op objectId    trackName  trackPayload
                            ]
                        )
                    ,   ((rgaType, textId), stateToChunk $ rgaFromV1 text)
                    ]
                ++  maybeToList mTrackObject
        pure Object{objectId, objectFrame}

    textId  = UUID.succValue objectId
    trackId = UUID.succValue textId

    endName        = fromJust $ UUID.mkName "end"
    startName      = fromJust $ UUID.mkName "start"
    statusName     = fromJust $ UUID.mkName "status"
    textName       = fromJust $ UUID.mkName "text"
    trackName      = fromJust $ UUID.mkName "track"
    externalIdName = fromJust $ UUID.mkName "externalId"
    providerName   = fromJust $ UUID.mkName "provider"
    sourceName     = fromJust $ UUID.mkName "source"
    urlName        = fromJust $ UUID.mkName "url"

timeFromV1 :: CRDT.LamportTime -> UUID
timeFromV1 (CRDT.LamportTime unixTime (CRDT.Pid pid)) =
    encodeEvent $
    Event (localEpochTimeFromUnix unixTime) (applicationSpecific pid)

rgaFromV1 :: CRDT.RgaString -> RgaRaw
rgaFromV1 (CRDT.RGA oldRga) = stateFromChunk
    [ Op event ref $ toPayload a
    | (vid, a) <- oldRga
    , let event = timeFromV1 vid
          ref   = case a of
              '\0' -> UUID.succValue event
              _    -> UUID.zero
    ]

-- used in parseNoteV1
deriveFromJSON defaultOptions ''Status

instance FromJSON NoteStatus where
    parseJSON v = case v of
        "Wiki" -> pure Wiki
        _      -> TaskStatus <$> parseJSON v
