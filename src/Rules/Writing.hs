{-# LANGUAGE OverloadedStrings #-}

module Rules.Writing where

import           Hakyll
import           Contexts.NextPrevNav (nextPrevNav)

rules :: Rules ()
rules = do
  match "writings/**.png" $ do
    route idRoute
    compile copyFileCompiler

  match "writings/**.jpg" $ do
    route idRoute
    compile copyFileCompiler

  match "writings/**" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/writing.html" nextPrevNav
          >>= loadAndApplyTemplate "templates/layout.html" nextPrevNav
