{-# LANGUAGE RecordWildCards #-}

module Gen (config, contact, day, note) where

import           Prelude hiding (maybe)

import           Data.Time (Day, fromGregorian)
import           Hedgehog (MonadGen)
import           Hedgehog.Gen (bool_, choice, enumBounded, integral, maybe,
                               string, text, unicode)
import qualified Hedgehog.Range as Range

import           FF.Config (Config (..), ConfigUI (..))
import           FF.Types (Contact (..), Note (..), NoteStatus (..), Status,
                           Track (..))

config :: MonadGen m => m Config
config = do
    dataDir <- maybe $ string (Range.linear 1 100) unicode
    ui <- configUI
    pure Config{..}

configUI :: MonadGen m => m ConfigUI
configUI = do
    shuffle <- bool_
    pure ConfigUI{..}

day :: MonadGen m => m Day
day = fromGregorian
    <$> integral (Range.constant 0 10000)
    <*> integral (Range.constant 1 12)
    <*> integral (Range.constant 1 31)

contact :: MonadGen m => m Contact
contact = Contact <$> string (Range.linear 1 100) unicode <*> status

note :: MonadGen m => m Note
note = Note
    <$> maybe day
    <*> day
    <*> noteStatus
    <*> string (Range.linear 1 100) unicode
    <*> maybe track

noteStatus :: MonadGen m => m NoteStatus
noteStatus = choice [TaskStatus <$> status, pure Wiki]

track :: MonadGen m => m Track
track = Track
    <$> text (Range.linear 1 100) unicode
    <*> text (Range.linear 1 100) unicode
    <*> text (Range.linear 1 100) unicode
    <*> text (Range.linear 1 100) unicode

status :: MonadGen m => m Status
status = enumBounded
