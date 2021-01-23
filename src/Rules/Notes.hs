{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rules.Notes where

import Hakyll

noteCtx :: Context String
noteCtx = defaultContext

rules :: Rules ()
rules = do
  match "notes/*" $ do
    route $ setExtension "html"
    compile
      $ pandocCompiler
      >>= loadAndApplyTemplate "templates/note.html" noteCtx
      >>= loadAndApplyTemplate "templates/layout-notes.html" noteCtx
