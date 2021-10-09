{-# LANGUAGE BangPatterns #-}

module FileHash
  ( hash
  , mkFileHashes
  ) where

import           Control.Monad.Extra            ( forM )
import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Hakyll                         ( Identifier
                                                , fromFilePath
                                                , getRecursiveContents
                                                )
import           System.FilePath                ( (</>) )

type FileHashes = Map Identifier String

mkFileHashes :: FilePath -> IO FileHashes
mkFileHashes dir = do
  allFiles <- getRecursiveContents (\_ -> pure False) dir
  fmap Map.fromList $ forM allFiles $ \path0 -> do
    let path1 = dir </> path0
    !h <- hash path1
    pure (fromFilePath path1, h)

hash :: FilePath -> IO String
hash path = do
  !h <- SHA256.hashlazy <$> BSL.readFile path
  pure $ BS8.unpack $ Base16.encode h
