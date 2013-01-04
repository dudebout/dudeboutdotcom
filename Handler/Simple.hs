{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Simple where

import Import

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
             setTitle "Nicolas Dudebout"
             $(widgetFile "home")

getResearchR :: Handler RepHtml
getResearchR = defaultLayout $ do
                 setTitle "Research"
                 $(widgetFile "research")

getContactR :: Handler RepHtml
getContactR = defaultLayout $ do
                setTitle "Contact"
                $(widgetFile "contact")
