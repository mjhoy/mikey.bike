{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rules.Lists (rules) where

import AssetHashing (FileHashes, rewriteAssetUrls)
import Contexts.Layout (Layout (..), layoutCtx)
import Data.Maybe (fromMaybe)
import Hakyll

starCtx :: Context String
starCtx = field "stars" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  let s = lookupString "stars" metadata
  let n = read (fromMaybe "0" s) :: Int
  pure $ replicate n '★'

listCtx :: Context String
listCtx = dateField "date" "%B %e, %Y" <> starCtx <> defaultContext

loadItems :: Compiler [Item String]
loadItems = loadAllSnapshots "lists/**" "content"

rules :: FileHashes -> Rules ()
rules assetHashes = do
  match "lists/**" $ do
    compile $ do
      pandocCompiler
        >>= loadAndApplyTemplate "templates/lists/item.html" listCtx
        >>= rewriteAssetUrls assetHashes
        >>= saveSnapshot "content"

  create ["lists.html"] $ do
    route idRoute
    compile $ do
      items <- loadItems
      let ctx = listField "items" listCtx (pure items)
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/lists/home.html" ctx
        >>= loadAndApplyTemplate "templates/base/layout.html" (layoutCtx $ Layout{title = Just "Lists", rssFeed = Nothing})
        >>= rewriteAssetUrls assetHashes
