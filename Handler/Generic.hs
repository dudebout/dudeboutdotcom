{-# LANGUAGE OverloadedStrings #-}

module Handler.Generic ( Compiler
                       , Composer
                       , compoFromCompi
                       , genericHandler
                       , htmlComposer
                       ) where

import Import
import qualified Data.Text as T (null, append)
import qualified Data.Text.Encoding as E
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (readFile)
import System.FilePath ((</>))
import Text.Blaze.Html (toHtml)
import Control.Arrow (second)

type Compiler = Text -> Html
type Composer = Text -> Widget
type Article = (Composer, FilePath)

compoFromCompi :: Compiler -> Composer
compoFromCompi compiler contentRaw = let content = compiler contentRaw
                                     in toWidget [hamlet| <article>
  #{content} |]

htmlComposer :: Composer
htmlComposer = compoFromCompi toHtml

renderFile :: Article -> IO Widget
renderFile (comp, fp) = do
  contentRaw <- readFile fp
  return $ comp $ E.decodeUtf8With lenientDecode $ contentRaw

genericHandler :: [Article] -> Text -> Handler RepHtml
genericHandler articles title = do
  ext <- getExtra
  defaultLayout $ do
    setTitle $ toHtml $ T.append titleRoot $ if T.null title then title
                                             else T.append " - " title
    widget <- liftIO $ mapM (renderFile . second (contentDir ext </>)) articles
    sequence_ widget
      where titleRoot = "Nicolas Dubebout"
