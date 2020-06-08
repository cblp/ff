{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database
  ( databaseTests,
  )
where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime (..), fromGregorian)
import FF (cmdNewNote, viewTaskSamples, loadAllNotes)
import FF.Config (defaultConfigUI)
import qualified FF.Github as Github
import FF.Options (New (..), Tags(Tags))
import FF.Types
  ( Limit,
    Note (..),
    NoteStatus (TaskStatus),
    Sample (Sample, items, total),
    Status (Active),
    TaskMode (Overdue),
    Track (..),
    View (NoteView, note, tags),
    entityVal,
    pattern Entity,
  )
import FF.Upgrade (upgradeDatabase)
import qualified Gen
import GitHub
  ( Issue (..),
    IssueNumber (IssueNumber),
    IssueState (..),
    IssueLabel(..),
    Milestone (..),
    URL (..),
  )
import GitHub.Data.Definitions (SimpleUser (..))
import GitHub.Data.Id (Id (..))
import GitHub.Data.Name (Name (..))
import Hedgehog ((===), Gen, Property, evalEither, forAll, property)
import RON.Data
  ( ReplicatedAsObject (readObject),
    evalObjectState,
    newObjectFrame,
  )
import RON.Data.RGA (RGA (RGA))
import RON.Storage.Backend (DocId (DocId))
import RON.Storage.Test (TestDB, runStorageSim)
import RON.Text (parseObject, serializeObject)
import RON.Types (ObjectRef (ObjectRef))
import qualified RON.UUID as UUID
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

databaseTests :: TestTree
databaseTests = $(testGroupGenerator)

prop_not_exist :: Property
prop_not_exist = property $ do
  (agenda, fs') <-
    evalEither
      $ runStorageSim fs $ do
        notes <- loadAllNotes
        viewTaskSamples Active defaultConfigUI agendaLimit today (Tags mempty) mempty notes
  Map.empty === agenda
  fs === fs'
  where
    fs = Map.empty

prop_smoke :: Property
prop_smoke = property $ do
  (agenda', fs') <-
    evalEither
      $ runStorageSim fs123 $ do
        notes <- loadAllNotes
        viewTaskSamples Active defaultConfigUI agendaLimit today (Tags mempty) mempty notes
  agenda === agenda'
  fs123 === fs'
  where
    agenda =
      Map.singleton
        (Overdue 365478)
        Sample
          { items =
              [ Entity
                  (DocId "B00000000002D-200000000002D")
                  NoteView
                    { note = Note
                        { note_status = Just $ TaskStatus Active,
                          note_text = Just $ RGA "helloworld",
                          note_start = Just $ fromGregorian 22 11 24,
                          note_end = Just $ fromGregorian 17 06 19,
                          note_tags = [],
                          note_track = Nothing,
                          note_links = []
                        },
                      tags = mempty
                    }
              ],
            total = 1
          }

fs123 :: TestDB
fs123 =
  Map.singleton "note" $ Map.singleton "B00000000002D-200000000002D"
    $ Map.fromList
        [ ( "event 2 93",
            BSLC.lines
              [i|
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
          ),
          ( "event 3 99",
            BSLC.lines
              [i|
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
          )
        ]

agendaLimit :: Maybe Limit
agendaLimit = Just 10

today :: Day
today = fromGregorian 1018 02 10

prop_new :: Property
prop_new =
  let text = "Мир"
      start = Just $ fromGregorian 2154 5 6
      end = Just $ fromGregorian 3150 1 2
      tags = Set.fromList ["Список", "тэг"]
      fs =
        Map.unions
          [ Map.singleton "note" $ Map.singleton "B000000007N4M-2000000000012"
              $ Map.singleton "B00000000P8JM-2000000000012"
              $ map encodeUtf8
              $ mconcat
                  [ [ "*set\t#B/0000000ynM+000000000Y\t!",
                      "\t@`{1DnM\t>end 3150 1 2",
                      "\t@}TnM\t>start 2154 5 6",
                      "\t@}inM\t>status >Active",
                      "\t@}ynM\t>tags >B/0000000Drz+000000000Y",
                      "\t@{2DnM\t>tags >B/0000000_QM+000000000Y",
                      "\t@}TnM\t>text >B/0000002inM+000000000Y"
                    ],
                    [ "*rga\t#{2inM\t@0\t!",
                      "\t@`}y_h\t'М'",
                      "\t@)i\t'и'",
                      "\t@)j\t'р'",
                      "."
                    ]
                  ],
            Map.singleton "tag"
              $ Map.unions
                  [ Map.singleton "B000000001NDU-2000000000012"
                      $ Map.singleton "B0000000039SM-2000000000012"
                      $ map encodeUtf8
                          [ "*set\t#B/0000000Drz+000000000Y\t!",
                            "\t@`}IOM\t>text 'Список'",
                            "."
                          ],
                    Map.singleton "B000000004HKM-2000000000012"
                      $ Map.singleton "B000000006N4M-2000000000012"
                      $ map encodeUtf8
                          [ "*set\t#B/0000000_QM+000000000Y\t!",
                            "\t@`}mnM\t>text 'тэг'",
                            "."
                          ]
                  ]
          ]
   in property $ do
        (note, fs') <-
          evalEither $ runStorageSim mempty
            $ cmdNewNote New {text, start, end, isWiki = False, tags} today
        let tags' =
              mapMaybe UUID.decodeBase32
                ["B000000001NDU-2000000000012", "B000000004HKM-2000000000012"]
        let Note {note_text, note_start, note_end, note_tags} = entityVal note
        Just (RGA $ Text.unpack text) === note_text
        start === note_start
        end === note_end
        map ObjectRef tags' === note_tags
        fs === fs'

jsonRoundtrip :: (Eq a, FromJSON a, Show a, ToJSON a) => Gen a -> Property
jsonRoundtrip genA = property $ do
  a <- forAll genA
  a' <- evalEither $ parseEither parseJSON $ toJSON a
  a === a'

ronRoundtrip :: (Eq a, ReplicatedAsObject a, Show a) => Gen a -> Property
ronRoundtrip genA = property $ do
  a <- forAll genA
  (obj, _) <- evalEither $ runStorageSim mempty $ newObjectFrame a
  let (u, bs) = serializeObject obj
  obj' <- evalEither $ parseObject u bs
  obj === obj'
  a' <- evalEither $ evalObjectState obj' readObject
  a === a'

prop_Config_JSON :: Property
prop_Config_JSON = jsonRoundtrip Gen.config

test_RON_Tests :: [TestTree]
test_RON_Tests =
  [ testProperty "Contact" $ ronRoundtrip Gen.contact,
    testProperty "Note" $ ronRoundtrip Gen.note
  ]

prop_repo :: Property
prop_repo =
  property
    (ideal === Github.sampleMap "ff-notes/ff" limit todayForIssues issues)
  where
    ideal =
      Map.singleton
        (Overdue 10)
        Sample
          { items =
              [ Entity
                  (DocId "")
                  NoteView
                    { note = Note
                        { note_status = Just $ TaskStatus Active,
                          note_text = Just $ RGA "import issues (GitHub -> ff)",
                          note_start = Just $ fromGregorian 2018 06 21,
                          note_end = Just $ fromGregorian 2018 06 15,
                          note_tags = [],
                          note_track = Just Track
                            { track_provider = Just "github",
                              track_source = Just "ff-notes/ff",
                              track_externalId = Just "60",
                              track_url =
                                Just "https://github.com/ff-notes/ff/issues/60"
                            },
                          note_links = []
                        },
                      tags = ["level_Research", "type_Enhancement"]
                    }
              ],
            total = 1
          }

todayForIssues :: Day
todayForIssues = fromGregorian 2018 06 25

limit :: Maybe Limit
limit = Just 1

issues :: [Issue]
issues =
  pure Issue
    { issueClosedAt = Nothing,
      issueUpdatedAt =
        UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60 + 41),
      issueEventsUrl = api "issues/60/events",
      issueHtmlUrl = Just $ URL "https://github.com/ff-notes/ff/issues/60",
      issueClosedBy = Nothing,
      issueLabels = labels,
      issueNumber = IssueNumber 60,
      issueAssignees = mempty,
      issueUser = cblp,
      issueTitle = "import issues (GitHub -> ff)",
      issuePullRequest = Nothing,
      issueUrl = api "issues/60",
      issueCreatedAt =
        UTCTime (fromGregorian 2018 06 21) (14 * 3600 + 30 * 60),
      issueBody = Just "",
      issueState = StateOpen,
      issueId = Id 334520780,
      issueComments = 0,
      issueMilestone = Just Milestone
        { milestoneCreator = cblp,
          milestoneDueOn =
            Just $ UTCTime (fromGregorian 2018 06 15) (7 * 3600),
          milestoneOpenIssues = 5,
          milestoneNumber = Id 1,
          milestoneClosedIssues = 0,
          milestoneDescription = Just "",
          milestoneTitle = "GitHub sync",
          milestoneUrl = api "milestones/1",
          milestoneCreatedAt =
            UTCTime (fromGregorian 2018 06 16) (9 * 3600 + 15 * 60 + 35),
          milestoneState = "open"
        }
    }
  where
    api x = URL $ "https://api.github.com/repos/ff-notes/ff/" <> x
    cblp = SimpleUser
      { simpleUserId = Id 63495,
        simpleUserLogin = N "cblp",
        simpleUserAvatarUrl =
          URL "https://avatars0.githubusercontent.com/u/63495?v=4",
        simpleUserUrl = URL "https://api.github.com/users/cblp"
      }
    labels = Vector.fromList
      [ IssueLabel
          { labelColor = "7057ff"
          , labelUrl = URL "https://api.github.com/repos/ff-notes/ron/labels/level_Research"
          , labelName = "level_Research"
          }
      , IssueLabel
          { labelColor = "b60205"
          , labelUrl = URL "https://api.github.com/repos/ff-notes/ron/labels/type_Enhancement"
          , labelName = "type_Enhancement"
          }
      ]

prop_json2ron :: Property
prop_json2ron = property $ do
  -- read JSON, merge, write RON
  do
    ((), db') <- evalEither $ runStorageSim fs123jsonAndLww upgradeDatabase
    fs123merged === db'
  -- idempotency
  do
    ((), db') <- evalEither $ runStorageSim fs123merged upgradeDatabase
    fs123merged === db'

fs123jsonAndLww :: TestDB
fs123jsonAndLww =
  Map.singleton "note"
    $ Map.fromList
        [ ( "000000000008K-000000000001J",
            Map.fromList
              [ ( "event 2 72",
                  BSLC.lines
                    [i|{
                      "end"   : ["17-06-19", 20, 21],
                      "start" : ["22-11-24", 25, 26],
                      "status": ["Active",   29, 30],
                      "text"  : ["hello",     6,  7]
                      }|]
                ),
                ( "event 2 78",
                  BSLC.lines
                    [i|{
                      "end"   : ["12-01-14", 15, 16],
                      "start" : ["9-10-11",   7,  8],
                      "status": ["Active",   27, 28],
                      "text"  : ["world",     4,  5]
                      }|]
                )
              ]
          ),
          ( "000000000008M-000000000001J",
            Map.singleton "event 3 24"
              $ mconcat
                  [ [ "*lww #000000004M$000000000o !",
                      "\t@B/6n7T8JWK0K+000000000L :end 17 6 19",
                      "\t@B/6n7T8JWK0P+000000000Q :start 22 11 24",
                      "\t@B/6n7T8JWK0T+000000000U :status >Active",
                      "\t@` :text >)P",
                      "\t:track >)Q"
                    ],
                    [ "*lww #)Q !",
                      "\t:externalId '54'",
                      "\t:provider 'github'",
                      "\t:source 'ff-notes/ff'",
                      "\t:url 'https://github.com/ff-notes/ff/pull/54'",
                      "*rga #)P @0 :0 !"
                    ],
                    [ "\t@B/6n7T8JWK06+0000000007 'h'",
                      "\t@)7 'e'",
                      "\t@)8 'l'",
                      "\t@)9 'l'",
                      "\t@)A 'o'",
                      "\t@B/6n7T8JWK04+0000000005 'w'",
                      "\t@)5 'o'",
                      "\t@)6 'r'",
                      "\t@)7 'l'",
                      "\t@)8 'd'",
                      "."
                    ]
                  ]
          )
        ]

fs123merged :: TestDB
fs123merged =
  Map.singleton "note"
    $ Map.fromList
        [ ( "000000000008K-000000000001J",
            Map.singleton "B00000000674M-2000000000012"
              $ mconcat
                  [ [ "*set\t#000000004K$000000000o\t!",
                      "\t@B/0000000Drz+000000000Y\t>end 17 6 19",
                      "\t@}IOM\t>start 22 11 24",
                      "\t@}QUM\t>status >Active",
                      "\t@}_QM\t>text >000000004L$000000000o"
                    ],
                    [ "*rga\t#)L\t@0\t!",
                      "\t@B/6n7T8JWK06+0000000007\t'h'",
                      "\t@)7\t'e'",
                      "\t@)8\t'l'",
                      "\t@)9\t'l'",
                      "\t@)A\t'o'",
                      "\t@B/6n7T8JWK04+0000000005\t'w'",
                      "\t@)5\t'o'",
                      "\t@)6\t'r'",
                      "\t@)7\t'l'",
                      "\t@)8\t'd'",
                      "."
                    ]
                  ]
          ),
          ( "000000000008M-000000000001J",
            Map.singleton "B00000000NN4M-2000000000012"
              $ mconcat
                  [ [ "*set\t#000000004M$000000000o\t!",
                      "\t@B/0000000qnM+000000000Y\t>end 17 6 19",
                      "\t@}ynM\t>start 22 11 24",
                      "\t@{1DnM\t>status >Active",
                      "\t@}TnM\t>text >000000004P$000000000o",
                      "\t@}inM\t>track >000000004Q$000000000o"
                    ],
                    [ "*rga\t#)P\t@0\t!",
                      "\t@B/6n7T8JWK06+0000000007\t'h'",
                      "\t@)7\t'e'",
                      "\t@)8\t'l'",
                      "\t@)9\t'l'",
                      "\t@)A\t'o'",
                      "\t@B/6n7T8JWK04+0000000005\t'w'",
                      "\t@)5\t'o'",
                      "\t@)6\t'r'",
                      "\t@)7\t'l'",
                      "\t@)8\t'd'"
                    ],
                    [ "*set\t#)Q\t@0\t!",
                      "\t@B/0000001ynM+000000000Y\t>externalId '54'",
                      "\t@{2DnM\t>provider 'github'",
                      "\t@}TnM\t>source 'ff-notes/ff'",
                      "\t@}inM\t>url 'https://github.com/ff-notes/ff/pull/54'",
                      "."
                    ]
                  ]
          )
        ]
