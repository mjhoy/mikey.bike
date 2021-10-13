{-# LANGUAGE BangPatterns #-}

module AssetHashing
  ( hash
  , mkFileHashes
  , assetHashRoute
  , FileHashes
  , rewriteAssetUrls
  , addHashToUrl
  ) where

import           Control.Monad.Extra            ( forM )
import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Hakyll                         ( Compiler
                                                , Identifier
                                                , Item(itemIdentifier)
                                                , Routes
                                                , customRoute
                                                , fromFilePath
                                                , getRecursiveContents
                                                , getRoute
                                                , toFilePath
                                                , withUrls
                                                )
import           System.FilePath                ( (</>) )
import           System.FilePath.Posix          ( takeBaseName
                                                , takeDirectory
                                                , takeExtension
                                                )

type FileHashes = Map Identifier String

hash :: FilePath -> IO String
hash path = do
  !h <- SHA256.hashlazy <$> BSL.readFile path
  pure $ BS8.unpack $ Base16.encode h

mkFileHashes :: FilePath -> IO FileHashes
mkFileHashes dir = do
  allFiles <- getRecursiveContents (\_ -> pure False) dir
  fmap Map.fromList $ forM allFiles $ \path0 -> do
    let path1 = dir </> path0
    !h <- hash path1
    pure (fromFilePath path1, h)

assetHashRoute :: FileHashes -> Routes
assetHashRoute fileHashes = customRoute $ \identifier ->
  let maybeHash = Map.lookup identifier fileHashes
      path      = toFilePath identifier
  in  maybe path (addHashToUrl path) maybeHash

rewriteAssetUrls :: FileHashes -> Item String -> Compiler (Item String)
rewriteAssetUrls hashes item = do
  route <- getRoute $ itemIdentifier item
  pure $ case route of
    Nothing -> item
    Just r  -> fmap rewrite item
 where
  rewrite = withUrls $ \url -> maybe url (addHashToUrl url) (Map.lookup (fromFilePath (dropWhile (== '/') url)) hashes)

addHashToUrl :: FilePath -> String -> String
addHashToUrl path hash =
  let baseName  = takeBaseName path
      extension = takeExtension path
      dir       = takeDirectory path
  in  dir </> baseName <> "-" <> hash <> extension
