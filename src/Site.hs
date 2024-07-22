{-# LANGUAGE OverloadedStrings #-}

module Site
  ( site
  ) where

import AssetHashing
  ( assetHashRoute
  , mkFileHashes
  , rewriteAssetUrls
  )
import Control.Arrow ((>>>))
import Hakyll
import System.FilePath
  ( replaceExtension
  , splitDirectories
  )

import Debug.Trace (traceShowM)
import qualified Rules.Journal as Journal
import qualified Rules.Lists as Lists
import qualified Rules.Notes as Notes
import qualified Rules.Writing as Writing

site :: IO ()
site = do
  writingSections <- Writing.buildWritingSections "writings"

  hakyll $ do
    imageHashes <- preprocess (mkFileHashes "images")
    cssHashes <- preprocess (mkFileHashes "stylesheets")
    jsHashes <- preprocess (mkFileHashes "js")

    let assetHashes = imageHashes <> cssHashes <> jsHashes

    match "images/*" $ do
      route $ assetHashRoute imageHashes
      compile copyFileCompiler

    match "images/**/*" $ do
      route $ assetHashRoute imageHashes
      compile copyFileCompiler

    match "js/*" $ do
      route $ assetHashRoute jsHashes
      compile copyFileCompiler

    match "js/**/*" $ do
      route idRoute
      compile copyFileCompiler

    match "stylesheets/*.css" $ do
      route $ assetHashRoute cssHashes
      compile copyFileCompiler

    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

    match "index.html" $ Journal.indexRoute assetHashes

    match "resume.html" $ do
      route idRoute
      compile $
        getResourceBody
          >>= loadAndApplyTemplate "templates/layout-no-footer.html" defaultContext
          >>= rewriteAssetUrls assetHashes

    Writing.rules writingSections assetHashes

    Journal.rules assetHashes

    Notes.rules assetHashes

    match "misc/**" $ do
      -- Drop "misc/" from the URL.
      route $ customRoute $ toFilePath >>> splitDirectories >>> drop 1 >>> concat >>> flip replaceExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/writing.html" defaultContext
          >>= loadAndApplyTemplate "templates/layout.html" defaultContext
          >>= rewriteAssetUrls assetHashes

    Lists.rules assetHashes

    match "templates/**" $ compile templateBodyCompiler
