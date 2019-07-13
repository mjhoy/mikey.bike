{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Arrow ((>>>))
import           Hakyll
import           System.FilePath (replaceExtension, splitDirectories)

import qualified Rules.Writing as Writing

main :: IO ()
main = hakyll $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/**/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/**/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "stylesheets/*.css" $ do
      route $ idRoute
      compile copyFileCompiler

    match "index.html" $ do
      route idRoute
      compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/layout.html" defaultContext

    Writing.rules

    match "misc/**" $ do
      -- Drop "misc/" from the URL.
      route $ customRoute $ toFilePath
                        >>> splitDirectories
                        >>> drop 1
                        >>> concat
                        >>> (flip replaceExtension) "html"
      compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/writing.html" defaultContext
            >>= loadAndApplyTemplate "templates/layout.html" defaultContext


    match "templates/*" $ compile templateBodyCompiler
