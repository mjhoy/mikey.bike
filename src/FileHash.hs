{-# LANGUAGE BangPatterns #-}

module FileHash
  ( hash
  ) where

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as BSL

hash :: FilePath -> IO String
hash path = do
  !h <- SHA256.hashlazy <$> BSL.readFile path
  pure $ BS8.unpack $ Base16.encode h
