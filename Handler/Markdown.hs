{-# LANGUAGE OverloadedStrings #-}

module Handler.Markdown where

import Import
import Text.Pandoc
import Handler.Types (Compiler, Composer, compoFromCompi)
import qualified Data.Text as T (unpack)

markdownToHtml :: String -> Html
markdownToHtml = writeHtml writerOptions . readMarkdown readerOptions
    where writerOptions = def { writerHtml5       = True
                              , writerSectionDivs = True
                              }
          readerOptions = def { readerSmart = True }

markdownCompiler :: Compiler
markdownCompiler = markdownToHtml . T.unpack

markdownComposer :: Composer
markdownComposer = compoFromCompi markdownCompiler
