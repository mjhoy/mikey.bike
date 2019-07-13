{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Monoid ((<>))
import           Control.Applicative (empty)
import           Control.Arrow ((>>>))
import           Hakyll
import           Data.Time.Clock (UTCTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           System.FilePath ((</>), takeDirectory, replaceExtension, splitDirectories)
import           Text.XML.HXT.DOM.XmlNode
import           Text.XML.HXT.DOM.TypeDefs
import           Text.XML.HXT.DOM.ShowXml (xshow)
import           Data.List (sort)

data NavDirection = Prev | Next

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

    match "writings/**.png" $ do
      route idRoute
      compile copyFileCompiler

    match "writings/**.jpg" $ do
      route idRoute
      compile copyFileCompiler

    match "writings/**" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/writing.html" nextPrevNavCtx
            >>= loadAndApplyTemplate "templates/layout.html" nextPrevNavCtx

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

nextPrevNavCtx :: Context String
nextPrevNavCtx =
    field "postNav" postNav <>
    defaultContext

  where

    -- generate next/prev links for the item's siblings.
    postNav :: Item String -> Compiler String
    postNav post = do
      let ident = itemIdentifier post
      r <- getRoute ident
      case r of
        Nothing -> pure ""
        Just path -> do
          let siblingsGlob = fromGlob $ (takeDirectory path) </> "**"
          sortedSiblings <- getMatches siblingsGlob >>= sortByDate
          let next = itemAfter sortedSiblings ident
          let prev = itemBefore sortedSiblings ident
          nextHtml <- maybe (return "") (toNavLink Next) next
          prevHtml <- maybe (return "") (toNavLink Prev) prev
          return $ concat $ [prevHtml, " ", nextHtml]

    toNavLink :: NavDirection -> Identifier -> Compiler String
    toNavLink dir ident = do
      url <- getRoute ident >>= return . fmap toUrl
      title <- getMetadataField ident "linkTitle"
      let link = do
            url' <- url
            title' <- title
            case dir of
              Next -> return $ ln "nav-next-link" url' (title' <> " &rarr;")
              Prev -> return $ ln "nav-prev-link" url' ("&larr; " <> title')
      maybe empty pure link

    ln :: String -> -- classname
          String -> -- href
          String -> -- title
          String
    ln klass href title = xshow [(mkElement (mkName "a") [ mkAttr (mkName "class") [mkText klass]
                                                         , mkAttr (mkName "href") [mkText href]
                                                         ]
                                                         [ mkText title]) ]

    sortByDate :: [Identifier] -> Compiler [Identifier]
    sortByDate xs = do
        withDate <- mapM f xs
        return $ map snd $ sort withDate
      where
        f :: Identifier -> Compiler (UTCTime, Identifier)
        f ident = do
          t <- getItemUTC defaultTimeLocale ident
          return (t, ident)

    itemAfter :: Eq a => [a] -> a -> Maybe a
    itemAfter xs x = lookup x $ zip xs (tail xs)

    itemBefore :: Eq a => [a] -> a -> Maybe a
    itemBefore xs x = lookup x $ zip (tail xs) xs
