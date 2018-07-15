{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( getIssueViews
    , getIssueSamples
    ) where

import           Control.Error (failWith)
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
import           Data.Maybe (maybe)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           Data.Vector (Vector)
import           GitHub (FetchCount (..), Id, Issue (..), IssueState (..),
                         Milestone (..), executeRequest', getUrl,
                         issueCreatedAt, issueHtmlUrl, issueId, issueMilestone,
                         issueState, issueTitle, mkOwnerName, mkRepoName,
                         untagId)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
import           FF.Storage (DocId (..))
import           FF.Types (Limit, ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..))

getIssues
    :: Maybe Text
    -> Maybe Limit
    -> ExceptT Text IO (Vector Issue)
getIssues mAddress mlimit = do
    address <- case mAddress of
        Just address -> pure address
        Nothing -> do
            packed <- liftIO $ Text.pack <$>
                readProcess "git" ["remote", "get-url", "--push", "origin"] ""
            failWith "Sorry, only github repository expected." $
                Text.stripPrefix "https://github.com/" packed
                >>= Text.stripSuffix ".git\n"
    (owner, repo) <- case Text.splitOn "/" address of
        [owner, repo] | not $ Text.null owner, not $ Text.null repo ->
            pure (owner, repo)
        _ -> throwError $
            "Something is wrong with " <> address <>
            ". Please, check correctness of input. Right format is OWNER/REPO"
    withExceptT (Text.pack . show) $ ExceptT $
        executeRequest' $ issuesForRepoR
            (mkOwnerName owner)
            (mkRepoName repo)
            mempty
            (maybe FetchAll (FetchAtLeast . fromIntegral) mlimit)

getIssueSamples
    :: Maybe Text
    -> Maybe Limit
    -> Day
    -> ExceptT Text IO (ModeMap Sample)
getIssueSamples mAddress mlimit today =
    sampleMap mlimit today <$> getIssues mAddress mlimit

getIssueViews
    :: Maybe Text
    -> Maybe Limit
    -> ExceptT Text IO [NoteView]
getIssueViews mAddress mlimit = noteViewList mlimit <$> getIssues mAddress mlimit

sampleMap :: Foldable t => Maybe Limit -> Day -> t Issue -> ModeMap Sample
sampleMap mlimit today issues =
    takeSamples mlimit
    . splitModes today
    . map issueToNoteView
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

noteViewList :: Foldable t => Maybe Limit -> t Issue -> [NoteView]
noteViewList mlimit issues =
    map issueToNoteView
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

issueToNoteView :: Issue -> NoteView
issueToNoteView Issue{..} = NoteView
    { nid      = toNoteId issueId
    , status   = toStatus issueState
    , text     = issueTitle
    , start    = utctDay issueCreatedAt
    , end      = maybeMilestone
    , provider = Just "github"
    , source   = maybeSource
    , extId    = pure . Text.pack . show $ issueNumber
    , url      = maybeUrl
    }
  where
    maybeUrl = getUrl <$> issueHtmlUrl
    maybeSource = fmap
        (Text.intercalate "/" . take 2 . Text.splitOn "/")
        (Text.stripPrefix "https://github.com/" =<< maybeUrl)
    maybeMilestone = case issueMilestone of
        Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
        _                                                      -> Nothing

toNoteId :: Id Issue -> NoteId
toNoteId = DocId . show . untagId

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
