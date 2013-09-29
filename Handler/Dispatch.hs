{-# LANGUAGE OverloadedStrings #-}

module Handler.Dispatch where

import Import
import Handler.Types (Composer, compoFromCompi)
import Handler.Markdown (markdownComposer)
import Handler.Publications (publicationsComposer)
import Handler.Resume (resumeComposer)

import qualified Data.Text as T (null, append)
import qualified Data.Text.Encoding as E (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString as B (readFile)
import Data.Maybe (fromJust)
import Data.Aeson.TH
import Data.Yaml
import System.FilePath ((</>))

import Data.Text (unpack)
import Data.Time.Calendar (Day)
import Data.Time.Format (readTime, formatTime)
import System.Locale (defaultTimeLocale)

data Article = Article { file     :: FilePath
                       , composer :: Text
                       } deriving Show
data Page = Page { title    :: Text
		 , articles :: [Article]
                 } deriving Show
data Footer = Footer { copyright    :: Text
                     , last_updated :: Text
                     } deriving Show
data TOC = TOC { home         :: Page
               , resume       :: Page
               , publications :: Page
               , code         :: Page
               , contact      :: Page
               , footer       :: Footer
               } deriving Show

$(deriveJSON defaultOptions ''Article)
$(deriveJSON defaultOptions ''Page)
$(deriveJSON defaultOptions ''Footer)
$(deriveJSON defaultOptions ''TOC)

renderArticle :: FilePath -> Article -> IO Widget
renderArticle dir (Article fp comp) = do
  contentRaw <- B.readFile $ dir </> fp
  return $ getComposer comp $ E.decodeUtf8With lenientDecode $ contentRaw

pageHandler :: Page -> Footer -> Handler Html
pageHandler (Page titl arts) (Footer copy upd) = do
  ext <- getExtra
  defaultLayout $ do
    setTitle $ toHtml $ T.append "Nicolas Dudebout" $ if T.null titl then titl
                                                      else T.append " - " titl
    articleWidgets <- liftIO $ mapM (renderArticle (contentDir ext)) arts
    let day = readTime defaultTimeLocale "%m/%d/%Y" (unpack upd) :: Day
        updateString = formatTime defaultTimeLocale "%B %e, %Y" day
        updateDatetime = formatTime defaultTimeLocale "%Y-%m-%d" day
    footerWidget <- return $(widgetFile "footer")
    sequence_ $ articleWidgets ++ [footerWidget]

tocEntryHandler :: (TOC -> Page) -> Handler Html
tocEntryHandler pageSelect = do
  ext <- getExtra
  toc <- liftIO $ readTOC $ contentDir ext </> "toc.yaml"
  pageHandler (pageSelect toc) (footer toc)

readTOC :: FilePath -> IO TOC
readTOC toc = do
  contentRaw <- B.readFile toc
  return $ fromJust $ decode contentRaw

getComposer :: Text -> Composer
getComposer name = case name of
                        "markdown"     -> markdownComposer
                        "resume"       -> resumeComposer
                        "publications" -> publicationsComposer
                        _              -> htmlComposer

htmlComposer :: Composer
htmlComposer = compoFromCompi toHtml

getHomeR :: Handler Html
getHomeR = tocEntryHandler home

getResumeR :: Handler Html
getResumeR = tocEntryHandler resume

getPublicationsR :: Handler Html
getPublicationsR = tocEntryHandler publications

getCodeR :: Handler Html
getCodeR = tocEntryHandler code

getContactR :: Handler Html
getContactR = tocEntryHandler contact
