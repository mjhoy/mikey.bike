{-# LANGUAGE OverloadedStrings #-}

module Rules.Journal where

import           Hakyll
import           Contexts.NextPrevNav (nextPrevNav)

rules :: Rules ()
rules = do
  match "journal/**" $ do
    route (setExtension "html" `composeRoutes` gsubRoute "journal/" (const "j/"))
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/writing.html" defaultContext
          >>= loadAndApplyTemplate "templates/layout-j.html" defaultContext
