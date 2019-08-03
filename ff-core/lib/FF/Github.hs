{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module FF.Github (
    getIssueViews,
    getOpenIssueSamples,
    sampleMap,
) where

import           Control.Monad ((>=>))
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import           Data.Time (Day, UTCTime (..))
import           Data.Vector (Vector)
import           GitHub (FetchCount (..), Issue (Issue), IssueRepoMod,
                         IssueState (..), Milestone (..), URL (..),
                         executeRequest', issueBody, issueCreatedAt,
                         issueHtmlUrl, issueMilestone, issueNumber, issueState,
                         issueTitle, mkOwnerName, mkRepoName, stateAll,
                         stateOpen, unIssueNumber)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           RON.Data.RGA (RGA (RGA))
import           System.Process.Typed (proc, readProcessStdout_)

import           FF (splitModes, takeSamples)
import           FF.Types (Limit, ModeMap, Note (..), NoteStatus (..), Sample,
                           Status (..), Track (..))

getIssues
    :: Maybe Text
    -> Maybe Limit
    -> IssueRepoMod
    -> ExceptT Text IO (Text, Vector Issue)
getIssues mAddress mlimit issueState = do
    address <- case mAddress of
        Just address -> pure address
        Nothing      -> do
            url <- liftIO $
                fmap (Text.strip . TextL.toStrict . TextL.decodeUtf8) $
                readProcessStdout_ $
                proc "git" ["remote", "get-url", "--push", "origin"]
            case url of
                (stripPrefixSuffix "https://github.com/" ".git" -> Just repo) ->
                    pure repo
                (stripPrefixSuffix "git@github.com:" ".git" -> Just repo) ->
                    pure repo
                _ -> fail
                    $   "Sorry, only github repository expected, but you have "
                    <>  Text.unpack url
    (owner, repo) <- case Text.splitOn "/" address of
        [owner, repo] | not $ Text.null owner, not $ Text.null repo ->
            pure (owner, repo)
        _ ->
            throwError $ mconcat
                [ "Something is wrong with "
                , address
                , ". Please, check correctness of input."
                , " Right format is OWNER/REPO"
                ]
    response <-
        withExceptT (Text.pack . show)
        $ ExceptT
        $ executeRequest'
        $ issuesForRepoR
              (mkOwnerName owner)
              (mkRepoName repo)
              issueState
              (maybe FetchAll (FetchAtLeast . fromIntegral) mlimit)
    pure (address, response)

getOpenIssueSamples
    :: Maybe Text
    -> Maybe Limit
    -> Day
    -> ExceptT Text IO (ModeMap (Sample Note))
getOpenIssueSamples mAddress mlimit today = do
    (address, issues) <- getIssues mAddress mlimit stateOpen
    pure $ sampleMap address mlimit today issues

getIssueViews :: Maybe Text -> Maybe Limit -> ExceptT Text IO [Note]
getIssueViews mAddress mlimit = do
    (address, issues) <- getIssues mAddress mlimit stateAll
    pure $ noteViewList address mlimit issues

sampleMap
    :: Foldable t
    => Text -> Maybe Limit -> Day -> t Issue -> ModeMap (Sample Note)
sampleMap address mlimit today
    = takeSamples mlimit
    . splitModes today
    . map (issueToNote address)
    . maybe id (take . fromIntegral) mlimit
    . toList

noteViewList :: Foldable t => Text -> Maybe Limit -> t Issue -> [Note]
noteViewList address mlimit =
    map (issueToNote address) . maybe id (take . fromIntegral) mlimit . toList

issueToNote :: Text -> Issue -> Note
issueToNote address issue = Note
    { note_status = Just $ toStatus issueState
    , note_text   = Just $ RGA $ Text.unpack $ issueTitle <> body
    , note_start  = Just $ utctDay issueCreatedAt
    , note_end
    , note_track  = Just Track
        { track_provider   = Just "github"
        , track_source     = Just address
        , track_externalId = Just externalId
        , track_url        = Just trackUrl
        }
    }
  where
    Issue   { issueBody
            , issueCreatedAt
            , issueHtmlUrl
            , issueMilestone
            , issueNumber
            , issueState
            , issueTitle
            }
        = issue
    externalId = Text.pack . show @Int $ unIssueNumber issueNumber
    trackUrl = case issueHtmlUrl of
        Just (URL url) -> url
        Nothing -> "https://github.com/" <> address <> "/issues/" <> externalId
    note_end = case issueMilestone of
        Just Milestone { milestoneDueOn = Just UTCTime { utctDay } } ->
            Just utctDay
        _ -> Nothing
    body = case issueBody of
        Nothing -> ""
        Just b  -> if Text.null b then "" else "\n\n" <> b

toStatus :: IssueState -> NoteStatus
toStatus = \case
    StateOpen   -> TaskStatus Active
    StateClosed -> TaskStatus Archived

stripPrefixSuffix :: Text -> Text -> Text -> Maybe Text
stripPrefixSuffix prefix suffix =
    Text.stripPrefix prefix >=> Text.stripSuffix suffix
