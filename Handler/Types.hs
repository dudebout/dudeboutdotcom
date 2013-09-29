{-# LANGUAGE OverloadedStrings #-}

module Handler.Types ( Compiler
                       , Composer
                       , compoFromCompi
                       ) where

import Import

type Compiler = Text -> Html
type Composer = Text -> Widget

compoFromCompi :: Compiler -> Composer
compoFromCompi compiler contentRaw = let content = compiler contentRaw
                                     in toWidget [hamlet| <article>
  #{content} |]
