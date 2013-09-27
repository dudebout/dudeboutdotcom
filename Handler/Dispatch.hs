{-# LANGUAGE OverloadedStrings #-}

module Handler.Dispatch where

import Import
import Handler.Generic (genericHandler)
import Handler.Markdown (markdownComposer)
import Handler.Publications (publicationsComposer)
import Handler.Resume (resumeComposer)

getHomeR :: Handler RepHtml
getHomeR = genericHandler [ (markdownComposer, "home_introduction.md")
                          , (markdownComposer, "home_research.md")
                          ] ""

getResumeR :: Handler RepHtml
getResumeR = genericHandler [ (resumeComposer, "resume.yaml")
                            ] "Resume"

getPublicationsR :: Handler RepHtml
getPublicationsR = genericHandler [ (markdownComposer, "publications_reproducible_research.md")
                                  , (publicationsComposer, "publications.bib")
                                  , (markdownComposer, "publications_presentations.md")
                                  ] "Publications"

getCodeR :: Handler RepHtml
getCodeR = genericHandler [ (markdownComposer, "code_projects.md")
                          , (markdownComposer, "code_open_source_contributions.md")
                          ] "Code"

getContactR :: Handler RepHtml
getContactR = genericHandler [ (markdownComposer, "contact_email.md")
                             , (markdownComposer, "contact_pgp_public_key.md")
                             ] "Contact"
