{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rules.Journal
  ( rules
  )
where

import           Control.Category               ( (<<<)
                                                , (>>>)
                                                )
import           Control.Monad                  ( filterM )
import           Data.List                      ( foldl'
                                                , groupBy
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , isNothing
                                                )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )
import           Hakyll
import           Hakyll.Web.Template.Context    ( getItemUTC )

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

data ArchiveGrouping = ArchiveGrouping
  { year :: String
  , month :: String
  } deriving (Eq, Show)

-- Adapted from https://biosphere.cc/software-engineering/hakyll-group-posts-by-year/
groupPosts :: (MonadMetadata m) => [Item a] -> m [(ArchiveGrouping, [Item a])]
groupPosts posts = do
  tuples <- tupelize posts
  pure $ (group >>> mapMaybe merge) tuples
 where
  merge :: [(ArchiveGrouping, Item a)] -> Maybe (ArchiveGrouping, [Item a])
  merge = foldl' fn Nothing
   where
    fn Nothing         (g, i) = Just (g, [i])
    fn (Just (g, acc)) (_, i) = Just (g, acc ++ [i])

  group :: [(ArchiveGrouping, Item a)] -> [[(ArchiveGrouping, Item a)]]
  group = groupBy (\(g1, _) (g2, _) -> g1 == g2)

  tupelize :: (MonadMetadata m) => [Item a] -> m [(ArchiveGrouping, Item a)]
  tupelize = mapM $ \i -> do
    ag <- postToArchiveGrouping i
    pure (ag, i)

postToArchiveGrouping :: (MonadMetadata m) => Item a -> m ArchiveGrouping
postToArchiveGrouping i = do
  let identifier = itemIdentifier i
  utcTime <- getItemUTC defaultTimeLocale identifier
  let year  = formatTime defaultTimeLocale "%Y" utcTime
  let month = formatTime defaultTimeLocale "%B" utcTime
  pure $ ArchiveGrouping { year, month }

displayArchiveGrouping :: ArchiveGrouping -> String
displayArchiveGrouping (ArchiveGrouping year month) = month ++ " " ++ year

rules :: Rules ()
rules = do
  match "journal/**" $ do
    route (setExtension "html" `composeRoutes` gsubRoute "journal/" (const "j/"))
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
      posts <- firstTen =<< nonDrafts =<< loadAllSnapshots "journal/**" "content"
      renderRss feed feedCtx posts

  create ["j/index.html"] $ do
    route idRoute
    compile $ do
      posts         <- (fmap (take 1) <<< recentFirst) =<< nonDrafts =<< loadAllSnapshots "journal/**" "content"
      postsArchive' <- (fmap (drop 1) <<< recentFirst) =<< nonDrafts =<< loadAllSnapshots "journal/**" "content"
      postsArchive :: [(ArchiveGrouping, [Item String])] <- groupPosts postsArchive'

      let layoutCtx = constField "title" "Journal" <> defaultContext
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> listField
                   "yearmonths"
                   (  field "yearmonth" (return . fst . itemBody)
                   <> listFieldWith "archivedposts" postCtx (return . snd . itemBody)
                   )
                   (traverse (\(g, posts) -> makeItem (displayArchiveGrouping g, posts)) postsArchive)
              <> layoutCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/journal_index.html" indexCtx
        >>= loadAndApplyTemplate "templates/layout-j.html"      layoutCtx
