{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Resume where

import Import
import Data.Text (unpack)
import Data.Yaml
import Data.Aeson.TH
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Locale (defaultTimeLocale)
import Data.Time (Day)
import Data.Time.Format (readTime, formatTime)
import Text.Printf (printf)
import Data.Maybe (fromJust)

import Tools

getResumeR :: Handler RepHtml
getResumeR = do
  ext <- getExtra
  defaultLayout $ do
    setTitle "Resume"
    content <- liftIO $ resumeWidget $ unpack (contentDir ext) ++ "/resume.yaml"
    $(widgetFile "resume")

resumeWidget :: String -> IO Html
resumeWidget filepath = do
  x <- decodeFile filepath
  return $ resume $ fromJust x

data Date = Date String deriving (Show)

data DateRange = DateRange { start :: Date
                           , end   :: Date
                           } deriving (Show)

data Diploma = Diploma { degree        :: String
                       , gpa           :: Float
                       , honors        :: Maybe [String]
                       , diplomaDates  :: DateRange
                       } deriving (Show)

data Institution = Institution { name     :: String
                               , location :: String
                               } deriving (Show)

data School = School { schoolInstitution :: Institution
                     , diplomas          :: [Diploma]
                     } deriving (Show)

data Position = Position { role            :: String
                         , accomplishments :: Maybe [String]
                         , positionDates   :: DateRange
                         } deriving (Show)

data Company = Company { companyInstitution :: Institution
                       , companyPosition    :: Position
                       } deriving (Show)

data Resume = Resume { education  :: [School]
                     , experience :: [Company]
                     } deriving (Show)

date :: Date -> Html
date (Date d) = H.span $ toHtml $ formatDate d

formatDate :: String -> String
formatDate "Present" = "Present"
formatDate d         = formatTime defaultTimeLocale "%m/%Y" day
    where day = readTime defaultTimeLocale "%b %Y" d :: Day

dateRange :: DateRange -> Html
dateRange (DateRange s e) = H.div ! A.class_ "pull-right" $ do
                              date s
                              _ <- "–"
                              date e

diploma :: Diploma -> Html
diploma (Diploma deg g _ dr) = H.div ! A.class_ "diploma clearfix" $ do
  dateRange dr
  H.div $ do
    toHtml deg
    H.span ! A.class_ "gpa hidden-phone" $ toHtml $ formatGPA g

formatGPA :: Float -> String
formatGPA = printf "GPA: %.1f/4.0"

institution :: Institution -> Html
institution (Institution n l) = H.span $ do
                                H.span ! A.class_ "instiname" $ toHtml n
                                _ <- ", "
                                toHtml l

school :: School -> Html
school (School i ds) = do
  H.div $ institution i
  H.ul ! A.class_ "list-unstyled" $ mconcat $ map (H.li . diploma) ds

position :: Position -> Html
position (Position r mas dr) = H.div ! A.class_ "clearfix" $ do
  dateRange dr
  H.div $ toHtml r
  case mas of
    Nothing -> ""
    Just as  -> H.ul $ mconcat $ map (H.li . toHtml) as

company :: Company -> Html
company (Company i p) = do
  H.div $ institution i
  position p

institli :: Html -> Html
institli = H.li ! A.class_ "institution"

resume :: Resume -> Html
resume (Resume edu xp) = do
  H.section $ do
    H.header $ H.h1 "Education"
    H.ul ! A.class_ "list-unstyled" $ mconcat $ map (institli . school) edu
  H.section $ do
    H.header $ H.h1 "Experience"
    H.ul ! A.class_ "list-unstyled" $ mconcat $ map (institli . company) xp

$(deriveJSON id ''Date)
$(deriveJSON id ''DateRange)
$(deriveJSON (deuniquifyName "diploma") ''Diploma)
$(deriveJSON id ''Institution)
$(deriveJSON (deuniquifyName "school") ''School)
$(deriveJSON (deuniquifyName "position") ''Position)
$(deriveJSON (deuniquifyName "company") ''Company)
$(deriveJSON id ''Resume)