{-# LANGUAGE OverloadedStrings #-}

module Handler.Dispatch where

import Import
import Handler.Generic (genericHandler)
import Handler.Markdown (markdownComposer)
import Handler.Publications (publicationsComposer)
import Handler.Resume (resumeComposer)

getHomeR :: Handler Html
getHomeR = genericHandler [ (markdownComposer, "home_introduction.md")
                          , (markdownComposer, "home_research.md")
                          ] ""

getResumeR :: Handler Html
getResumeR = genericHandler [ (resumeComposer, "resume.yaml")
                            ] "Resume"

getPublicationsR :: Handler Html
getPublicationsR = genericHandler [ (markdownComposer, "publications_reproducible_research.md")
                                  , (publicationsComposer, "publications.bib")
                                  , (markdownComposer, "publications_presentations.md")
                                  ] "Publications"

getCodeR :: Handler Html
getCodeR = genericHandler [ (markdownComposer, "code_projects.md")
                          , (markdownComposer, "code_open_source_contributions.md")
                          ] "Code"

getContactR :: Handler Html
getContactR = genericHandler [ (markdownComposer, "contact_email.md")
                             , (markdownComposer, "contact_pgp_public_key.md")
                             ] "Contact"
