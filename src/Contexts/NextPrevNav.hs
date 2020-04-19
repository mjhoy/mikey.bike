{-# LANGUAGE OverloadedStrings #-}

module Contexts.NextPrevNav (nextPrevNav) where

import           Data.Monoid ((<>))
import           Control.Applicative (empty)
import           Hakyll
import           Data.Time.Clock (UTCTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           System.FilePath ((</>), takeDirectory)
import           Text.XML.HXT.DOM.XmlNode
import           Text.XML.HXT.DOM.TypeDefs
import           Text.XML.HXT.DOM.ShowXml (xshow)
import           Data.List (sort)

data NavDirection = Prev | Next

nextPrevNav :: Context String
nextPrevNav =
    field "postNav" postNav <>
    defaultContext

-- generate next/prev links for the item's siblings.
postNav :: Item String -> Compiler String
postNav post = do
  let ident = itemIdentifier post
  r <- getRoute ident
  case r of
    Nothing -> pure ""
    Just path -> do
      let siblingsGlob = fromGlob $ takeDirectory path </> "**"
      sortedSiblings <- getMatches siblingsGlob >>= sortByDate
      let next = itemAfter sortedSiblings ident
      let prev = itemBefore sortedSiblings ident
      nextHtml <- maybe (return "") (toNavLink Next) next
      prevHtml <- maybe (return "") (toNavLink Prev) prev
      return $ concat [prevHtml, " ", nextHtml]

toNavLink :: NavDirection -> Identifier -> Compiler String
toNavLink dir ident = do
  url <- fmap toUrl <$> getRoute ident
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
ln klass href title = xshow [mkElement (mkName "a") [ mkAttr (mkName "class") [mkText klass]
                                                     , mkAttr (mkName "href") [mkText href]
                                                     ]
                                                     [ mkText title] ]

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

