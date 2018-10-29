{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module FF.Serve
    ( cmdServe
    ) where

import           Prelude hiding (div, span)

import           Control.Monad.Extra (when, whenJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Default.Class (def)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time (Day)
import           Network.Wai.Handler.Warp (defaultSettings, setHost)
import           RON.Storage.IO (runStorage)
import qualified RON.Storage.IO as Storage
import           RON.Types (UUID)
import           System.IO (hPutStrLn, stderr)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 (Html, a, br, div, h1, li, p, section, span,
                                   stringValue, strong, style, toHtml, ul, (!))
import           Text.Blaze.Html5.Attributes (class_, href)
import           Web.Scotty (get, html, scottyOpts, settings, verbose)

import           FF (getNoteSamples, getUtcToday)
import           FF.Config (ConfigUI (..))
import           FF.Types (pattern Entity, ModeMap, Note (..), NoteSample,
                           Sample (..), TaskMode (..), Track (..), omitted)
import           FF.UI (sampleLabel)

cmdServe :: MonadIO m => Storage.Handle -> ConfigUI -> m ()
cmdServe h ui = liftIO $ do
    hPutStrLn stderr "serving at http://localhost:3000/"
    scottyOpts opts $ get "/" $ do
        today <- getUtcToday
        nvs <- liftIO $ runStorage h $ getNoteSamples ui Nothing today
        html $ renderHtml $ do
            style ".metaItem { color: #ccc; }"
            prettyHtmlSamplesBySections nvs
  where
    opts = def{verbose = 0, settings = defaultSettings & setHost "::1"}

prettyHtmlSamplesBySections :: ModeMap NoteSample -> Html
prettyHtmlSamplesBySections samples = do
    for_ (Map.assocs samples) $ uncurry prettyHtmlSample
    when (numOmitted > 0) $ p $ toHtml numOmitted <> " task(s) omitted"
  where
    numOmitted = sum $ fmap omitted samples

prettyHtmlSample :: TaskMode -> NoteSample -> Html
prettyHtmlSample mode = \case
    Sample{sample_total = 0} -> mempty
    Sample{sample_items} ->
        section $ do
            h1 $ toHtml (sampleLabel mode)
            ul $ for_ sample_items noteView
  where
    metaItem k v = span ! class_ "metaItem" $ " | " *> strong k *> " " *> v
    noteView (Entity entityId Note{..}) = li $ do
        p $
            case lines note_text of
                []            -> pure ()
                [_]           -> toHtml note_text
                header : body ->
                    mconcat $
                    intersperse br $
                    strong (toHtml header) : map toHtml body
        div $ do
            metaItem "id" $ toHtml $ show @UUID entityId
            metaItem "start" $ toHtml $ show @Day note_start
            whenJust note_track $ \Track{..} ->
                metaItem "tracking" $
                    a ! href (stringValue $ Text.unpack track_url) $
                        toHtml track_url
