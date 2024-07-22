{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rules.Lists (rules) where

import AssetHashing (FileHashes, rewriteAssetUrls)
import Rules.Journal (layoutCtx)
import Hakyll
import Data.Maybe (fromMaybe)
import Control.Applicative (empty)

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
      makeItem ""
        >>= loadAndApplyTemplate "templates/lists/home.html" ctx
        >>= loadAndApplyTemplate "templates/journal/layout.html" (layoutCtx $ Just "Lists")
        >>= rewriteAssetUrls assetHashes
