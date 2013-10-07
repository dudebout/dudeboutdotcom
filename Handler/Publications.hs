{-# LANGUAGE OverloadedStrings #-}

module Handler.Publications where

import Import
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H (span, a)
import qualified Text.Blaze.Html5.Attributes as A (href, class_)
import Text.Parsec.Text ()
import Text.Parsec (parse)
import Text.BibTeX.Parse (splitAuthorList, skippingLeadingSpace, file)
import Text.BibTeX.Entry (T(Cons), entryType, identifier, fields)
import Data.Maybe (fromMaybe)
import Data.List (intersperse, intercalate)
import Text.Highlighting.Kate (highlightAs, formatHtmlBlock, defaultFormatOpts)
import Handler.Types (Composer)
import qualified Data.Text as T (unpack)

publicationsComposer :: Composer
publicationsComposer contentRaw = let res = parse (skippingLeadingSpace file) "publication handler" $ T.unpack contentRaw
                                      entries = case res of
                                                  Left _ -> error "failed to parse"
                                                  Right parsedEntries -> parsedEntries
                                  in mapM_ publicationWidget entries

publicationWidget :: T -> Widget
publicationWidget ent =
  let typ              = entryType ent
      ide              = filter (/= ':') $ identifier ent
      entLookup        = (`bibtexLookup` ent)
      auths            = splitAuthorList $ entLookup "author"
      title            = toHtml $ entLookup "title"
      authors          = toHtml $ intersperse ", " $ map formatAuthor auths
      venue            = formatVenue ent
      maybeRepository  = lookup "repository" $ fields ent
      maybeCopyright   = lookup "copyright" $ fields ent
      abstract         = entLookup "abstract"
      fieldsNoAbstract = filter (\(x, _) -> not $ x `elem` ["abstract", "repository", "copyright"]) $ fields ent
      bibtex           = toHtml $ formatHtmlBlock defaultFormatOpts
                        $ highlightAs "bibtex" $ formatEntry ent{fields = fieldsNoAbstract}
  in $(widgetFile "publication")

formatEntry :: T -> String
formatEntry (Cons bibType bibId items) = "@" ++ bibType ++ "{" ++ bibId ++ ",\n" ++
                                         (concat $ intersperse ",\n" $ map formatItem items) ++ "\n}\n"

formatItem :: (String, String) -> String
formatItem (name, value) = "  "  ++  name ++ " = " ++ enclose value
    where enclose = if name `elem` ["year", "month"]
                    then id
                    else \val -> "\"" ++ val ++ "\""

knownAuthors :: [(String, Html -> Html)]
knownAuthors = [ ("Dudebout, Nicolas", H.span ! A.class_ "me")
               , ("Shamma, Jeff S.", H.a ! A.href "http://www.prism.gatech.edu/~jshamma3/" ! A.class_ "external")
               ]

knownEntryTypes :: [(String, T -> String)]
knownEntryTypes = [ ("MastersThesis", \entry -> "Master's Thesis, " ++ "school" `bibtexLookup` entry)
                  , ("Misc", miscFormatting)
                  , ("InProceedings", ("booktitle" `bibtexLookup`))
                  ]
    where miscFormatting entry = case "howpublished" `bibtexLookup` entry of
                                   "PhD Proposal" -> "PhD Proposal, " ++ "school" `bibtexLookup` entry
                                   _              -> "unknown_howpublished"

formatAuthor :: String -> Html
formatAuthor auth = fromMaybe id (lookup auth knownAuthors) $ toHtml $ flipAndShortenName auth

formatVenue :: T -> Html
formatVenue entry = toHtml $ intercalate ", " [venue entry, "year" `bibtexLookup` entry]
    where type_ = entryType entry
          venue = fromMaybe (const "unknown_type") $ lookup type_ knownEntryTypes

bibtexLookup :: String -> T -> String
bibtexLookup fieldName entry = fromMaybe ("unknown_" ++ fieldName) $ lookup fieldName $ fields entry

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
