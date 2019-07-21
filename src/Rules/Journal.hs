{-# LANGUAGE OverloadedStrings #-}

module Rules.Journal where

import           Hakyll

rules :: Rules ()
rules = do
  match "journal/**" $ do
    route (setExtension "html" `composeRoutes` gsubRoute "journal/" (const "j/"))
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/writing.html" defaultContext
          >>= loadAndApplyTemplate "templates/layout-j.html" defaultContext
