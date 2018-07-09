{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( trackList
    , trackCopy
    ) where

import           Control.Error (failWith)
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
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

handleInput
    :: Maybe Text
    -> Maybe Limit
    -> ExceptT Text IO (Vector Issue)
handleInput mAddress mlimit = do
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

trackList
    :: Maybe Text
    -> Maybe Limit
    -> Day
    -> ExceptT Text IO (ModeMap Sample)
trackList mAddress mlimit today = do
    response <- handleInput mAddress mlimit
    pure $ sampleMaps mlimit today response

trackCopy
    :: Maybe Text
    -> ExceptT Text IO [NoteView]
trackCopy mAddress = do
    response <- handleInput mAddress Nothing
    pure $ noteViewList response

sampleMaps :: Foldable t => Maybe Limit -> Day -> t Issue -> ModeMap Sample
sampleMaps mlimit today issues =
    takeSamples mlimit
    . splitModes today
    . map toNoteView
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

noteViewList :: Foldable t => t Issue -> [NoteView]
noteViewList issues = map toNoteView $ toList issues

toNoteView :: Issue -> NoteView
toNoteView Issue{..} = NoteView
    { nid    = toNoteId issueId
    , status = toStatus issueState
    , text   = issueTitle
    , start  = utctDay issueCreatedAt
    , end    = maybeMilestone
    , extId  = Just . Text.pack . show . untagId $ issueId
    , source = getUrl <$> issueHtmlUrl
    }
  where
    maybeMilestone = case issueMilestone of
        Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
        _                                                      -> Nothing

toNoteId :: Id Issue -> NoteId
toNoteId = DocId . show . untagId

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
