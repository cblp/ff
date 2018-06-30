{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( runCmdGithub
    , sampleMaps
    ) where

import           Control.Monad (join)
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           GitHub (FetchCount (..), Id, Issue (..), IssueState (..),
                         Milestone (..), URL (..), executeRequest',
                         issueCreatedAt, issueHtmlUrl, issueId, issueMilestone,
                         issueState, issueTitle, mkOwnerName, mkRepoName,
                         untagId)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
import           FF.Storage (DocId (..))
import           FF.Types (Limit, ModeMap, NoteId, NoteView (..), Sample (..),
                           Status (..))

runCmdGithub
    :: Maybe Text
    -> Maybe Limit
    -> Day  -- ^ today
    -> IO (Either Text (ModeMap Sample))
runCmdGithub address mlimit today = do
    address' <- case address of
        Just a -> pure $ if all (not . Text.null) (splitter a)
            then Right (splitter a)
            else Left $ Text.concat
                ["Something is wrong with "
                , a
                ,". Please, check correctness of input. "
                ,"Right format is OWNER/REPO"
                ]
        Nothing -> do
            packed <- Text.pack <$> readProcess "git" ["remote", "get-url", "origin"] ""
            case join $ Text.stripSuffix ".git"
                <$> Text.stripPrefix "https://github.com/" packed of
                Nothing -> pure $ Left "Sorry, only github repositary expected."
                Just b  -> pure $ Right $ splitter b
    case address' of
        Left err    -> pure $ Left err
        Right input -> do
            let owner = mkOwnerName $ head input
            let repo  = mkRepoName $ last input
            let fetching = maybe FetchAll (FetchAtLeast . fromIntegral) mlimit
            let issues = issuesForRepoR owner repo mempty fetching
            result <- fmap (sampleMaps mlimit today) <$> executeRequest' issues
            case result of
                Left err -> pure $ Left $ Text.pack $ show err
                Right sm -> pure $ Right sm
  where
    splitter = Text.splitOn "/"

sampleMaps :: Foldable t => Maybe Limit -> Day -> t Issue -> ModeMap Sample
sampleMaps mlimit today issues =
    takeSamples mlimit
    . splitModes today
    . map toNoteView
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

toNoteView :: Issue -> NoteView
toNoteView Issue{..} = NoteView
    { nid    = toNoteId issueId
    , status = toStatus issueState
    , text   = issueTitle <> maybeUrl
    , start  = utctDay issueCreatedAt
    , end    = maybeMilestone
    }
  where
    maybeUrl = case issueHtmlUrl of
        Just (URL url) -> "\nurl " <> url
        Nothing        -> ""
    maybeMilestone = case issueMilestone of
        Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
        _                                                      -> Nothing

toNoteId :: Id Issue -> NoteId
toNoteId = DocId . show . untagId

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
