{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time (Day, UTCTime (..), fromGregorian)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           GitHub (Issue (..), IssueState (..), Milestone (..), URL (..))
import           GitHub.Data.Definitions (SimpleUser (..))
import           GitHub.Data.Id (Id (..))
import           GitHub.Data.Name (Name (..))
import           Hedgehog (Gen, MonadTest, Property, forAll, property, (===))
import           Hedgehog.Internal.Property (failWith)
import           RON.Data (ReplicatedAsObject, getObject, newObject)
import           RON.Event (Event (..), LocalTime (TEpoch), applicationSpecific,
                            encodeEvent)
import           RON.Internal.Word (ls60)
import           RON.Storage.Test (TestDB, runStorageSim)
import           RON.Text (parseObject, serializeObject)
import           RON.Types (UUID)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (defaultMainGenerator)

import           FF (cmdNewNote, getNoteSamples)
import           FF.Config (ConfigUI (..))
import qualified FF.Github as Github
import           FF.Options (New (..))
import           FF.Types (pattern Entity, Limit, Note (..),
                           NoteStatus (TaskStatus), Sample (..),
                           Status (Active), TaskMode (Overdue), Track (..),
                           entityVal)
import           FF.Upgrade (upgradeDatabase)

import qualified Gen

main :: IO ()
main = $(defaultMainGenerator)

prop_not_exist :: Property
prop_not_exist = property $ do
    (agenda, fs') <-
        either fail pure $
        runStorageSim fs $
        getNoteSamples ui agendaLimit today
    agenda === Map.empty
    fs' === fs
  where
    fs = Map.empty

prop_smoke :: Property
prop_smoke = property $ do
    (agenda, fs') <-
        either fail pure $
        runStorageSim fs123 $
        getNoteSamples ui agendaLimit today
    agenda ===
        Map.singleton
            (Overdue 365478)
            Sample
                { sample_items =
                    [Entity
                        (event 77 77)
                        Note
                            { note_status = TaskStatus Active
                            , note_text   = "helloworld"
                            , note_start  = fromGregorian 22 11 24
                            , note_end    = Just $ fromGregorian 17 06 19
                            , note_track  = Nothing
                            }]
                , sample_total = 1
                }
    fs' === fs123

fs123 :: TestDB
fs123 =
    Map.singleton "note" $ Map.singleton "B00000000002D-200000000002D" $
    Map.fromList
        [ "event 2 93" -: BSLC.lines [i|
            *lww #B/000000001D+000000001D !
                @20+21  :end    >some =17 =06 =19
                @25+26  :start  =22 =11 =24
                @29+30  :status >Active
                @07+07  :text   >2+I9
                        :track  >none
            *rga #2+I9 :0 !
                @6+7            'h'
                @7+7            'e'
                @8+7            'l'
                @9+7            'l'
                @A+7            'o'
            |]
        , "event 3 99" -: BSLC.lines [i|
            *lww #000000001Q$000000001Q !
                @15+16  :end    >some =12 =01 =14
                @07+08  :start  =09 =10 =11
                @27+28  :status >Active
                @04+05  :text   >2+I9
                        :track  >none
            *rga #2+I9 :0 !
                @4+5            'w'
                @5+5            'o'
                @6+5            'r'
                @7+5            'l'
                @8+5            'd'
            |]
        ]

agendaLimit :: Maybe Limit
agendaLimit = Just 10

today :: Day
today = fromGregorian 1018 02 10

prop_new :: Property
prop_new = property $ do
    (note, fs') <-
        evalEitherS $ runStorageSim mempty $
        cmdNewNote New{newText, newStart, newEnd, newWiki = False} today
    let Note{note_text, note_start, note_end} = entityVal note
    note_text  === Text.unpack newText
    note_start === fromMaybe today newStart
    note_end   === newEnd
    fs'        === fs
  where
    newText  = "Мир"
    newStart = Just $ fromGregorian 2154 5 6
    newEnd   = Just $ fromGregorian 3150 1 2
    fs =
        Map.singleton "note" $ Map.singleton "B000000000005-2000000000012" $
        Map.singleton "B000000000006-2000000000012" $
        map encodeUtf8
            [ "*lww #B/0000000005+000000000Y @B/0000000005+000000000Y :0 !"
            ,   "\t@B/0000000005+000000000Y :end >some =3150 =1 =2 ,"
            ,   "\t@B/0000000005+000000000Y :start =2154 =5 =6 ,"
            ,   "\t@B/0000000005+000000000Y :status >Active ,"
            ,   "\t@B/0000000005+000000000Y :text >B/0000000004+000000000Y ,"
            ,   "\t@B/0000000005+000000000Y :track >none ,"
            , "*rga #B/0000000004+000000000Y @B/0000000003+000000000Y :0 !"
            ,   "\t@B/0000000001+000000000Y 'М' ,"
            ,   "\t@B/0000000002+000000000Y 'и' ,"
            ,   "\t@B/0000000003+000000000Y 'р' ,"
            , "."
            ]

evalEitherS :: (MonadTest m, HasCallStack) => Either String a -> m a
evalEitherS = \case
    Left  x -> withFrozenCallStack $ failWith Nothing x
    Right a -> pure a

jsonRoundtrip :: (Eq a, FromJSON a, Show a, ToJSON a) => Gen a -> Property
jsonRoundtrip genA = property $ do
    a <- forAll genA
    a' <- evalEitherS $ parseEither parseJSON $ toJSON a
    a === a'

ronRoundtrip :: (Eq a, ReplicatedAsObject a, Show a) => Gen a -> Property
ronRoundtrip genA = property $ do
    a <- forAll genA
    (obj, _) <- evalEitherS $ runStorageSim mempty $ newObject a
    let (u, bs) = serializeObject obj
    obj' <- evalEitherS $ parseObject u bs
    obj === obj'
    a' <- evalEitherS $ getObject obj'
    a === a'

test_JSON_Tests :: [TestTree]
test_JSON_Tests =
    [ testProperty "Config"  $ jsonRoundtrip Gen.config
    , testProperty "Contact" $ ronRoundtrip  Gen.contact
    , testProperty "Note"    $ ronRoundtrip  Gen.note
    ]

ui :: ConfigUI
ui = ConfigUI {shuffle = False}

prop_repo :: Property
prop_repo = property $
    Github.sampleMap "ff-notes/ff" limit todayForIssues issues === ideal
  where
    ideal = Map.singleton
        (Overdue 10)
        Sample
            { sample_items = pure Note
                { note_status = TaskStatus Active
                , note_text   = "import issues (GitHub -> ff)"
                , note_start  = fromGregorian 2018 06 21
                , note_end    = Just $ fromGregorian 2018 06 15
                , note_track  = Just Track
                    { track_provider = "github"
                    , track_source = "ff-notes/ff"
                    , track_externalId = "60"
                    , track_url = "https://github.com/ff-notes/ff/issues/60"
                    }
                }
            , sample_total = 1
            }

todayForIssues :: Day
todayForIssues = fromGregorian 2018 06 25

limit :: Maybe Limit
limit = Just 1

issues :: [Issue]
issues = pure Issue
    { issueClosedAt = Nothing
    , issueUpdatedAt =
        UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60 + 41)
    , issueEventsUrl = api "issues/60/events"
    , issueHtmlUrl = Just $ URL "https://github.com/ff-notes/ff/issues/60"
    , issueClosedBy = Nothing
    , issueLabels = mempty
    , issueNumber = 60
    , issueAssignees = mempty
    , issueUser = cblp
    , issueTitle = "import issues (GitHub -> ff)"
    , issuePullRequest = Nothing
    , issueUrl = api "issues/60"
    , issueCreatedAt = UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60)
    , issueBody = Just ""
    , issueState = StateOpen
    , issueId = Id 334520780
    , issueComments = 0
    , issueMilestone = Just Milestone
        { milestoneCreator = cblp
        , milestoneDueOn = Just $ UTCTime (fromGregorian 2018 06 15) (7 * 3600)
        , milestoneOpenIssues = 5
        , milestoneNumber = Id 1
        , milestoneClosedIssues = 0
        , milestoneDescription = Just ""
        , milestoneTitle = "GitHub sync"
        , milestoneUrl = api "milestones/1"
        , milestoneCreatedAt =
            UTCTime (fromGregorian 2018 06 16) (9 * 3600 + 15 * 60 + 35)
        , milestoneState = "open"
        }
    }
  where
    api x = URL $ "https://api.github.com/repos/ff-notes/ff/" <> x
    cblp = SimpleUser
        { simpleUserId = Id 63495
        , simpleUserLogin = N "cblp"
        , simpleUserAvatarUrl =
            URL "https://avatars0.githubusercontent.com/u/63495?v=4"
        , simpleUserUrl = URL "https://api.github.com/users/cblp"
        }

prop_json2ron :: Property
prop_json2ron = property $ do

    -- read JSON, merge, write RON
    do  ((), db') <- either fail pure $ runStorageSim fs123json upgradeDatabase
        db' === fs123merged

    -- idempotency
    do  ((), db') <-
            either fail pure $ runStorageSim fs123merged upgradeDatabase
        db' === fs123merged

fs123json :: TestDB
fs123json =
    Map.singleton "note" $ Map.singleton "000000000008K-000000000001J" $
    Map.fromList
        [ "event 2 72" -: BSLC.lines [i|{
            "end"   : ["17-06-19", 20, 21],
            "start" : ["22-11-24", 25, 26],
            "status": ["Active",   29, 30],
            "text"  : ["hello",     6,  7]
            } |]
        , "event 2 78" -: BSLC.lines [i|{
            "end"   : ["12-01-14", 15, 16],
            "start" : ["9-10-11",   7,  8],
            "status": ["Active",   27, 28],
            "text"  : ["world",     4,  5]
            } |]
        ]

fs123merged :: TestDB
fs123merged =
    Map.singleton "note" $ Map.singleton "000000000008K-000000000001J" $
    Map.singleton "B000000000001-2000000000012"
        [ "*lww #000000004K$000000000o @B/6n7T8JWK0T+000000000U :0 !"
        ,   "\t@B/6n7T8JWK0K+000000000L :end >some =17 =6 =19 ,"
        ,   "\t@B/6n7T8JWK0P+000000000Q :start =22 =11 =24 ,"
        ,   "\t@B/6n7T8JWK0T+000000000U :status >Active ,"
        ,   "\t@000000004K$000000000o :text >000000004L$000000000o ,"
        ,   "\t@000000004K$000000000o :track >none ,"
        , "*rga #000000004L$000000000o @B/6n7T8JWK0A+0000000007 :0 !"
        ,   "\t@B/6n7T8JWK06+0000000007 :0 'h' ,"
        ,   "\t@B/6n7T8JWK07+0000000007 :0 'e' ,"
        ,   "\t@B/6n7T8JWK08+0000000007 :0 'l' ,"
        ,   "\t@B/6n7T8JWK09+0000000007 :0 'l' ,"
        ,   "\t@B/6n7T8JWK0A+0000000007 :0 'o' ,"
        ,   "\t@B/6n7T8JWK04+0000000005 :0 'w' ,"
        ,   "\t@B/6n7T8JWK05+0000000005 :0 'o' ,"
        ,   "\t@B/6n7T8JWK06+0000000005 :0 'r' ,"
        ,   "\t@B/6n7T8JWK07+0000000005 :0 'l' ,"
        ,   "\t@B/6n7T8JWK08+0000000005 :0 'd' ,"
        , "."
        ]

(-:) :: a -> b -> (a, b)
a -: b = (a, b)
infixr 0 -:

event :: Word64 -> Word64 -> UUID
event x y = encodeEvent $ Event (TEpoch $ ls60 x) $ applicationSpecific y
