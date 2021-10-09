{-# LANGUAGE NamedFieldPuns #-}

module Models.ArchiveGrouping
  ( groupPosts
  , displayArchiveGrouping
  ) where

import           Data.Function                  ( (&) )
import           Data.List                      ( foldl'
                                                , groupBy
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Hakyll

data ArchiveGrouping = ArchiveGrouping
  { year  :: String
  , month :: String
  }
  deriving (Eq, Show)

-- Adapted from https://biosphere.cc/software-engineering/hakyll-group-posts-by-year/
groupPosts :: (MonadMetadata m, MonadFail m) => [Item a] -> m [(ArchiveGrouping, [Item a])]
groupPosts posts = do
  tuples <- tupelize posts
  pure $ tuples & group & mapMaybe merge
 where
  merge :: [(ArchiveGrouping, Item a)] -> Maybe (ArchiveGrouping, [Item a])
  merge = foldl' fn Nothing
   where
    fn Nothing         (g, i) = Just (g, [i])
    fn (Just (g, acc)) (_, i) = Just (g, acc ++ [i])

  group :: [(ArchiveGrouping, Item a)] -> [[(ArchiveGrouping, Item a)]]
  group = groupBy (\(g1, _) (g2, _) -> g1 == g2)

  tupelize :: (MonadMetadata m, MonadFail m) => [Item a] -> m [(ArchiveGrouping, Item a)]
  tupelize = mapM $ \i -> do
    ag <- postToArchiveGrouping i
    pure (ag, i)

postToArchiveGrouping :: (MonadMetadata m, MonadFail m) => Item a -> m ArchiveGrouping
postToArchiveGrouping i = do
  let identifier = itemIdentifier i
  utcTime <- getItemUTC defaultTimeLocale identifier
  let year  = formatTime defaultTimeLocale "%Y" utcTime
  let month = formatTime defaultTimeLocale "%B" utcTime
  pure $ ArchiveGrouping { year, month }

displayArchiveGrouping :: ArchiveGrouping -> String
displayArchiveGrouping (ArchiveGrouping year month) = month ++ " " ++ year
