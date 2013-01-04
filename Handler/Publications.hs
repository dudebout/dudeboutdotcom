{-# LANGUAGE OverloadedStrings #-}

module Handler.Publications where

import Import
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H (span, a)
import qualified Text.Blaze.Html5.Attributes as A (href, class_)
import Text.Parsec.String (parseFromFile)
import Text.BibTeX.Parse (splitAuthorList, skippingLeadingSpace, file)
import Text.BibTeX.Entry (T, entryType, identifier, fields)
import qualified Text.BibTeX.Format as F
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.List (intersperse, intercalate)
import Text.Highlighting.Kate (highlightAs, formatHtmlBlock, defaultFormatOpts)

getPublicationsR :: Handler RepHtml
getPublicationsR = do
		 ext <- getExtra
		 defaultLayout $ do
                     setTitle "Publications"
                     res <- liftIO $ parseFromFile (skippingLeadingSpace file) $ unpack (contentDir ext) ++ "/dudebout.bib"
                     let entries = case res of
                                     Left _ -> error "failed to parse"
                                     Right parsedEntries -> parsedEntries
                         pubs = map publiWidget entries
                     $(widgetFile "publications")

publiWidget :: T -> Widget
publiWidget ent =
  let typ              = entryType ent
      ide              = filter (/= ':') $ identifier ent
      entLookup        = (`bibtexLookup` ent)
      auths            = splitAuthorList $ entLookup "author"
      title            = toHtml $ entLookup "title"
      authors          = toHtml $ intersperse ", " $ map formatAuthor auths
      venue            = formatVenue ent
      abstract         = entLookup "abstract"
      fieldsNoAbstract = filter (\(x, _) -> x /= "abstract") $ fields ent
      bibtex           = toHtml $ formatHtmlBlock defaultFormatOpts
                        $ highlightAs "bibtex" $ F.entry ent{fields = fieldsNoAbstract}
  in $(widgetFile "publication")

knownAuthors :: [(String, Html -> Html)]
knownAuthors = [ ("Dudebout, Nicolas", H.span ! A.class_ "me")
               , ("Shamma, Jeff S.", H.a ! A.href "http://www.prism.gatech.edu/~jshamma3/" ! A.class_ "external")
               ]

knownEntryTypes :: [(String, T -> String)]
knownEntryTypes = [ ("MastersThesis", \entry -> "Master's Thesis, " ++ "school" `bibtexLookup` entry)
                  , ("InProceedings", ("booktitle" `bibtexLookup`))
                  ]

formatAuthor :: String -> Html
formatAuthor auth = fromMaybe id (lookup auth knownAuthors) $ toHtml $ flipAndShortenName auth

formatVenue :: T -> Html
formatVenue entry = toHtml $ intercalate ", " [venue entry, "year" `bibtexLookup` entry]
    where type_ = entryType entry
          venue = fromMaybe (const "NOtype") $ lookup type_ knownEntryTypes

bibtexLookup :: String -> T -> String
bibtexLookup fieldName entry = fromMaybe ("NO" ++ fieldName) $ lookup fieldName $ fields entry

flipAndShortenName :: String -> String
flipAndShortenName name =
   let (lastName, firstAndMiddleNameDirty) = break (','==) name
       firstAndMiddleName = dropWhile (`elem` [',', ' ']) firstAndMiddleNameDirty
       (first, middle) = break (' '==) firstAndMiddleName
       firstNameAbbrev = abbreviate first
       middleNameAbbrev = abbreviate $ dropWhile (`elem` [' ']) middle
       names = filter (/="") [firstNameAbbrev, middleNameAbbrev, lastName]
   in intercalate " " names

abbreviate :: String -> String
abbreviate "" = ""
abbreviate (n:_) = n:"."
