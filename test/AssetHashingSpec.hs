module AssetHashingSpec where

import           AssetHashing                   ( hash
                                                , mkFileHashes
                                                )
import qualified Data.Map                      as Map
import           Hakyll.Core.Identifier         ( fromFilePath )
import           System.FilePath                ( (</>) )
import           Test.Hspec

spec :: Spec
spec = do
  let testFile     = "test-file-hash.json"
      fixturesDir  = "test/fixtures"
      expectedHash = "b9cd2605ea75293b16b892a97c5e4b0bc18f3dafd0cbdf897c80258d57415c80"

  describe "hash" $ do
    it "should correctly hash a file" $ do
      res <- hash (fixturesDir </> testFile)
      res `shouldBe` expectedHash

  describe "mkFileHashes" $ do
    it "should return a map of file hashes for a directory" $ do
      map <- mkFileHashes fixturesDir
      let res = Map.lookup (fromFilePath $ fixturesDir </> testFile) map
      res `shouldBe` Just expectedHash
