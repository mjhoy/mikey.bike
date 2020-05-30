{-# LANGUAGE OverloadedStrings #-}

module Rules.Journal
  ( rules
  )
where

import           Control.Category               ( (<<<) )
import           Control.Monad                  ( filterM )
import           Data.Maybe                     ( isNothing )
import           Hakyll

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

nonDrafts :: (MonadMetadata m) => [Item a] -> m [Item a]
nonDrafts = filterM f
  where f i = isNothing <$> getMetadataField (itemIdentifier i) "draft"

feed :: FeedConfiguration
feed = FeedConfiguration { feedTitle       = "mikey.bike - journal"
                         , feedDescription = ""
                         , feedAuthorName  = "Michael Hoy"
                         , feedAuthorEmail = "mjh@mjhoy.com"
                         , feedRoot        = "https://mikey.bike"
                         }

rules :: Rules ()
rules = do
  match "journal/**" $ do
    route
      (setExtension "html" `composeRoutes` gsubRoute "journal/" (const "j/"))
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/journal.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/journal-page.html" postCtx
      >>= loadAndApplyTemplate "templates/layout-j.html"     defaultContext

  create ["j/rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx  = postCtx `mappend` bodyField "description"
      let firstTen = fmap (take 10) <<< recentFirst
      posts <- firstTen =<< nonDrafts =<< loadAllSnapshots "journal/**"
                                                           "content"
      renderRss feed feedCtx posts

  create ["j/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< nonDrafts =<< loadAllSnapshots "journal/**"
                                                              "content"
      let layoutCtx = constField "title" "Journal" <> defaultContext
      let indexCtx  = listField "posts" postCtx (return posts) <> layoutCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/journal_index.html" indexCtx
        >>= loadAndApplyTemplate "templates/layout-j.html"      layoutCtx
