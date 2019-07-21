{-# LANGUAGE OverloadedStrings #-}

module Rules.Writing where

import           Hakyll
import           Contexts.NextPrevNav (nextPrevNav)
import           System.FilePath
import           System.Directory
import           Control.Monad.Extra (whenMaybeM)
import           Control.Monad (forM, forM_, filterM)
import           Data.Maybe (catMaybes)

data WritingSection = WritingSection
  { writingSectionDir :: FilePath
  , writingSectionIndex :: FilePath
  } deriving (Eq, Show)

buildWritingSections :: FilePath -> IO [WritingSection]
buildWritingSections path = do
  contents <- listDirectory path
  sections <- filterM (doesDirectoryExist . (path </>)) contents
  catMaybes <$> build sections

  where

    build sections = do
      forM sections $ \sectionName -> do
        let indexPath = path </> sectionName <> ".md"
        whenMaybeM (doesFileExist indexPath) $ do
          pure $ WritingSection { writingSectionDir = path </> sectionName
                                , writingSectionIndex = indexPath }

topicCtx :: Context String
topicCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

rules :: [WritingSection] -> Rules ()
rules sections = do
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
      compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/writing.html" nextPrevNav
            >>= loadAndApplyTemplate "templates/layout.html" nextPrevNav

  forM_ sections $ \section -> do
    match (fromGlob $ writingSectionIndex section) $ do
      route $ setExtension "html"
      compile pandocCompiler

  create ["writings.html"] $ do
    route idRoute
    compile $ do
      writingIndices <- recentFirst =<< loadAll "writings/*.md"
      let indexCtx =
            listField "topics" topicCtx (return writingIndices) <>
            defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/writing_index.html" indexCtx
        >>= loadAndApplyTemplate "templates/layout.html" defaultContext
