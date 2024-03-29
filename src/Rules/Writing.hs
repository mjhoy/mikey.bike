{-# LANGUAGE OverloadedStrings #-}

module Rules.Writing where

import AssetHashing
  ( FileHashes
  , rewriteAssetUrls
  )
import Contexts.NextPrevNav (nextPrevNav)
import Control.Monad
  ( filterM
  , forM
  , forM_
  )
import Hakyll
import System.Directory
import System.FilePath

data WritingSection = WritingSection
  { writingSectionDir :: FilePath
  , writingSectionIndex :: Maybe FilePath
  }
  deriving (Eq, Show)

buildWritingSections :: FilePath -> IO [WritingSection]
buildWritingSections path = do
  contents <- listDirectory path
  sections <- filterM (doesDirectoryExist . (path </>)) contents
  build sections
 where
  build sections = forM sections $ \sectionName -> do
    let indexPath = path </> sectionName <> ".md"
    hasIndex <- doesFileExist indexPath
    let maybeIndex = if hasIndex then Just indexPath else Nothing
    pure $ WritingSection{writingSectionDir = path </> sectionName, writingSectionIndex = maybeIndex}

topicCtx :: Context String
topicCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

rules :: [WritingSection] -> FileHashes -> Rules ()
rules sections assetHashes = do
  match "writings/**.png" $ do
    route idRoute
    compile copyFileCompiler

  match "writings/**.jpg" $ do
    route idRoute
    compile copyFileCompiler

  forM_ (writingSectionDir <$> sections) $ \dir -> do
    let glob = dir </> "**"
    match (fromGlob glob) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/writing.html" nextPrevNav
          >>= loadAndApplyTemplate "templates/layout.html" nextPrevNav
          >>= rewriteAssetUrls assetHashes

  forM_ sections $ \section -> do
    let maybeIdx = writingSectionIndex section
    let dir = writingSectionDir section
    case maybeIdx of
      Just idx -> match (fromGlob idx) $ do
        route $ setExtension "html"
        compile $ do
          let writingsGlob = fromGlob (dir </> "**")
          writings <- chronological =<< loadAll writingsGlob
          let context = listField "posts" defaultContext (return writings) <> defaultContext
          pandocCompiler >>= loadAndApplyTemplate "templates/writing_section.html" context
      Nothing -> pure ()

  create ["writings.html"] $ do
    route idRoute
    compile $ do
      writingIndices <- recentFirst =<< loadAll "writings/*.md"
      let layoutCtx = constField "title" "Writings | mikey.bike" <> defaultContext
      let indexCtx = listField "topics" topicCtx (return writingIndices) <> layoutCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/writing_index.html" indexCtx
        >>= loadAndApplyTemplate "templates/layout.html" layoutCtx
        >>= rewriteAssetUrls assetHashes
