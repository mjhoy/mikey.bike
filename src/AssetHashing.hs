{-# LANGUAGE BangPatterns #-}

module AssetHashing
  ( hash
  , mkFileHashes
  , assetHashRoute
  , FileHashes
  , rewriteAssetUrls
  , rewriteAssetUrls'
  , addHashToUrl
  , lookupHashForUrl
  )
where

import Control.Monad.Extra (forM)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import qualified Data.Map as Map
import Hakyll
  ( Compiler
  , Identifier
  , Item (itemIdentifier)
  , Routes
  , customRoute
  , fromFilePath
  , getRecursiveContents
  , getRoute
  , toFilePath
  , withUrls
  )
import System.FilePath ((</>))
import System.FilePath.Posix
  ( takeBaseName
  , takeDirectory
  , takeExtension
  )

-- Inspired/taken from: https://groups.google.com/g/hakyll/c/zdkQlDsj9lQ

type FileHashes = Map Identifier String

hash :: FilePath -> IO String
hash path = do
  !h <- SHA256.hash <$> BS.readFile path
  pure $! BS8.unpack $! Base16.encode h

mkFileHashes :: FilePath -> IO FileHashes
mkFileHashes dir = do
  allFiles <- getRecursiveContents (\_ -> pure False) dir
  fmap Map.fromList $ forM allFiles $ \innerPath -> do
    let fullPath = dir </> innerPath
    !h <- hash fullPath
    pure (fromFilePath fullPath, h)

assetHashRoute :: FileHashes -> Routes
assetHashRoute fileHashes = customRoute $ \identifier ->
  let maybeHash = Map.lookup identifier fileHashes
      path = toFilePath identifier
   in maybe path (addHashToUrl path) maybeHash

rewriteAssetUrls :: FileHashes -> Item String -> Compiler (Item String)
rewriteAssetUrls hashes item = do
  route <- getRoute $ itemIdentifier item
  pure $ case route of
    Nothing -> item
    Just r -> fmap (rewriteAssetUrls' hashes) item

rewriteAssetUrls' :: FileHashes -> String -> String
rewriteAssetUrls' hashes = withUrls rewrite
 where
  rewrite url = maybe url (addHashToUrl url) (lookupHashForUrl hashes url)

lookupHashForUrl :: FileHashes -> String -> Maybe String
lookupHashForUrl hashes url = Map.lookup (fromFilePath urlWithoutRootSlash) hashes
 where
  urlWithoutRootSlash = dropWhile (== '/') url

addHashToUrl :: FilePath -> String -> String
addHashToUrl path hsh =
  let baseName = takeBaseName path
      extension = takeExtension path
      dir = takeDirectory path
   in dir </> baseName <> "-" <> hsh <> extension
