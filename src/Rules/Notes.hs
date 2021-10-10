{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rules.Notes where

import           Control.Monad                  ( forM )
import           Data.Char                      ( toUpper )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
import           FileHash                       ( FileHashes
                                                , rewriteAssetUrls
                                                )
import           Hakyll

noteCtx :: Context String
noteCtx = defaultContext

alphaOrder :: (MonadMetadata m) => [Item a] -> m [Item a]
alphaOrder items = do
  itemsWithTitle <- forM items $ \item -> do
    title <- getMetadataField (itemIdentifier item) "title"
    pure (fromMaybe "untitled" title, item)
  let sorted = sortOn (map toUpper . fst) itemsWithTitle
  pure (map snd sorted)

rules :: FileHashes -> Rules ()
rules assetHashes = do
  match "notes/**" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/note.html" noteCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/layout-notes.html" noteCtx
      >>= rewriteAssetUrls assetHashes

  create ["notes/index.html"] $ do
    route idRoute
    compile $ do
      notes <- alphaOrder =<< loadAllSnapshots ("notes/**" .&&. complement "notes/index.html") "content"
      let layoutCtx = constField "title" "Notes" <> defaultContext
      let indexCtx  = listField "notes" noteCtx (pure notes) <> layoutCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/notes_index.html"  indexCtx
        >>= loadAndApplyTemplate "templates/layout-notes.html" layoutCtx
        >>= rewriteAssetUrls assetHashes
