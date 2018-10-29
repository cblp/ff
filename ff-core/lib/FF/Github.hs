{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( getIssueViews
    , getOpenIssueSamples
    , sampleMap
    )
where

import           Control.Error (failWith)
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           Data.Vector (Vector)
import           GitHub (FetchCount (..), Issue (..), IssueRepoMod,
                         IssueState (..), Milestone (..), URL (..),
                         executeRequest', issueCreatedAt, issueHtmlUrl, issueId,
                         issueMilestone, issueState, issueTitle, mkOwnerName,
                         mkRepoName, stateAll, stateOpen)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

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
            packed <- liftIO $ Text.pack <$> readProcess
                "git"
                ["remote", "get-url", "--push", "origin"]
                ""
            failWith "Sorry, only github repository expected."
                $   Text.stripPrefix "https://github.com/" packed
                >>= Text.stripSuffix ".git\n"
    (owner, repo) <- case Text.splitOn "/" address of
        [owner, repo] | not $ Text.null owner, not $ Text.null repo ->
            pure (owner, repo)
        _ ->
            throwError
                $ "Something is wrong with "
                <> address
                <> ". Please, check correctness of input. Right format is OWNER/REPO"
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
issueToNote address Issue{..} = Note
    { note_status = toStatus issueState
    , note_text   = Text.unpack $ issueTitle <> body
    , note_start  = utctDay issueCreatedAt
    , note_end
    , note_track  = Just Track
        { track_provider   = "github"
        , track_source     = address
        , track_externalId
        , track_url
        }
    }
  where
    track_externalId = Text.pack $ show issueNumber
    track_url = case issueHtmlUrl of
        Just (URL url) -> url
        Nothing ->
            "https://github.com/" <> address <> "/issues/" <> track_externalId
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
