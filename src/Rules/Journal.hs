{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rules.Journal
  ( rules
  ) where

import           AssetHashing                   ( FileHashes
                                                , rewriteAssetUrls
                                                )
import           Control.Category               ( (<<<) )
import           Control.Monad                  ( filterM )
import           Data.Maybe                     ( isNothing )
import           Hakyll
import           Models.ArchiveGrouping

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

nonDrafts :: (MonadMetadata m) => [Item a] -> m [Item a]
nonDrafts = filterM f where f i = isNothing <$> getMetadataField (itemIdentifier i) "draft"

feed :: FeedConfiguration
feed = FeedConfiguration { feedTitle       = "mikey.bike - journal"
                         , feedDescription = ""
                         , feedAuthorName  = "Michael Hoy"
                         , feedAuthorEmail = "mjh@mjhoy.com"
                         , feedRoot        = "https://mikey.bike"
                         }

loadPosts :: Compiler [Item String]
loadPosts = recentFirst =<< nonDrafts =<< loadAllSnapshots "journal/**" "content"

postIndexCtx :: [Item String] -> Compiler (Context String)
postIndexCtx posts = do
  postsArchive <- groupPosts posts
  pure $ listField
    "yearmonths"
    (field "yearmonth" (return . fst . itemBody) <> listFieldWith "archivedposts" postCtx (return . snd . itemBody))
    (traverse (\(g, groupedPosts) -> makeItem (displayArchiveGrouping g, groupedPosts)) postsArchive)

rules :: FileHashes -> Rules ()
rules assetHashes = do
  match "journal/**" $ do
    route (setExtension "html" `composeRoutes` gsubRoute "journal/" (const "j/"))
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/journal.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/journal-page.html" postCtx
      >>= loadAndApplyTemplate "templates/layout-j.html"     defaultContext
      >>= rewriteAssetUrls assetHashes

  create ["j/rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx  = postCtx `mappend` bodyField "description"
      let firstTen = fmap (take 10) <<< recentFirst
      posts <- firstTen =<< nonDrafts =<< loadAllSnapshots "journal/**" "content"
      renderRss feed feedCtx posts

  create ["j/archive.html"] $ do
    route idRoute
    compile $ do
      posts    <- loadPosts
      indexCtx <- postIndexCtx posts

      let layoutCtx = constField "title" "Journal" <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/journal_archive.html" indexCtx
        >>= loadAndApplyTemplate "templates/layout-j.html"        layoutCtx
        >>= rewriteAssetUrls assetHashes

  create ["j/index.html"] $ do
    route idRoute
    compile $ do
      posts <- loadPosts
      let detailPosts = take 1 posts
      indexCtx <- postIndexCtx (drop 1 posts)

      let layoutCtx = constField "title" "Journal" <> defaultContext
      let homeCtx   = listField "posts" postCtx (return detailPosts) <> indexCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/journal_home.html" homeCtx
        >>= loadAndApplyTemplate "templates/layout-j.html"     layoutCtx
        >>= rewriteAssetUrls assetHashes
