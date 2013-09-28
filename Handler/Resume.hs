{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Resume where

import Import
import Data.Yaml
import Data.List (intercalate)
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
import Handler.Generic (Composer)
import Handler.Markdown (markdownToHtml)
import qualified Data.Text.Encoding as E (encodeUtf8)


resumeComposer :: Composer
resumeComposer contentRaw = resumeWidget $ fromJust $ decode $ E.encodeUtf8 contentRaw

resumeWidget :: Resume -> Widget
resumeWidget (Resume schools companies skills) = $(widgetFile "resume")

prettifyDate :: String -> String
prettifyDate "Present" = "Present"
prettifyDate d         = formatTime defaultTimeLocale "%m/%Y" day
    where day = readTime defaultTimeLocale "%b %Y" d :: Day

formatDateRange :: DateRange -> Html
formatDateRange (DateRange (Date s) (Date e)) = toHtml $ intercalate "–" $ map prettifyDate [s, e]

formatGPA :: Diploma -> Html
formatGPA dip = toHtml $ (printf (if isInt g then "GPA: %.1f/4.0"
                                     else  "GPA: %.2f/4.0") g :: String)
    where isInt x = x == fromInteger (round x)
          g = gpa dip

formatInstitution :: Institution -> Html
formatInstitution (Institution n l) = H.h2 $ do
    toHtml n
    H.span ! A.class_ "subh2" $ toHtml $ ", " ++ l

formatRole :: String -> Html
formatRole = markdownToHtml

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
                       , companyPositions   :: [Position]
                       } deriving (Show)

data Resume = Resume { education         :: [School]
                     , experience        :: [Company]
                     , programmingSkills :: [String]
                     } deriving (Show)

$(deriveJSON defaultOptions ''Date)
$(deriveJSON defaultOptions ''DateRange)
$(deriveJSON defaultOptions{fieldLabelModifier = (deuniquifyName "diploma")} ''Diploma)
$(deriveJSON defaultOptions ''Institution)
$(deriveJSON defaultOptions{fieldLabelModifier = (deuniquifyName "school")} ''School)
$(deriveJSON defaultOptions{fieldLabelModifier = (deuniquifyName "position")} ''Position)
$(deriveJSON defaultOptions{fieldLabelModifier = (deuniquifyName "company")} ''Company)
$(deriveJSON defaultOptions ''Resume)
