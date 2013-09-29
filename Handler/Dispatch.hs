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


data Article = Article { file     :: FilePath
                       , composer :: Text
                       } deriving Show
data Page = Page { title    :: Text
		 , articles :: [Article]
                 } deriving Show
data TOC = TOC { home         :: Page
               , resume       :: Page
               , publications :: Page
               , code         :: Page
               , contact      :: Page
               } deriving Show

$(deriveJSON defaultOptions ''Article)
$(deriveJSON defaultOptions ''Page)
$(deriveJSON defaultOptions ''TOC)

renderArticle :: FilePath -> Article -> IO Widget
renderArticle dir (Article fp comp) = do
  contentRaw <- B.readFile $ dir </> fp
  return $ getComposer comp $ E.decodeUtf8With lenientDecode $ contentRaw

pageHandler :: Page -> Handler Html
pageHandler (Page titl arts) = do
  ext <- getExtra
  defaultLayout $ do
    setTitle $ toHtml $ T.append titleRoot $ if T.null titl then titl
                                             else T.append " - " titl
    widget <- liftIO $ mapM (renderArticle (contentDir ext)) arts
    sequence_ widget
      where titleRoot = "Nicolas Dubebout"

tocEntryHandler :: (TOC -> Page) -> Handler Html
tocEntryHandler select = do
  ext <- getExtra
  toc <- liftIO $ readTOC $ contentDir ext </> "toc.yaml"
  let page = select toc
  pageHandler page

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
