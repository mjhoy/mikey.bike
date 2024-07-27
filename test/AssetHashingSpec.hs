module AssetHashingSpec where

import           AssetHashing                   ( addHashToUrl
                                                , hash
                                                , lookupHashForUrl
                                                , mkFileHashes
                                                , rewriteAssetUrls'
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
      hashmap <- mkFileHashes fixturesDir
      let res = Map.lookup (fromFilePath $ fixturesDir </> testFile) hashmap
      res `shouldBe` Just expectedHash

  describe "addHashToUrl" $ do
    it "should add the hash properly" $ do
      let res = addHashToUrl (fixturesDir </> testFile) expectedHash
      res
        `shouldBe` "test/fixtures/test-file-hash-b9cd2605ea75293b16b892a97c5e4b0bc18f3dafd0cbdf897c80258d57415c80.json"

  describe "lookupHashForUrl" $ do
    it "should find the hashes url without a root slash" $ do
      hashes <- mkFileHashes fixturesDir
      let res = lookupHashForUrl hashes "test/fixtures/test-file-hash.json"
      res `shouldBe` Just expectedHash

    it "should find the hashes url with a root slash" $ do
      hashes <- mkFileHashes fixturesDir
      let res = lookupHashForUrl hashes "/test/fixtures/test-file-hash.json"
      res `shouldBe` Just expectedHash

  describe "rewriteAssetUrls'" $ do
    it "should rewrite URLs in a string" $ do
      hashes <- mkFileHashes fixturesDir
      let htmlString = "<p>foo</p><a href=\"/test/fixtures/test-file-hash.json\">"
      let res        = rewriteAssetUrls' hashes htmlString
      res
        `shouldBe` "<p>foo</p><a href=\"/test/fixtures/test-file-hash-b9cd2605ea75293b16b892a97c5e4b0bc18f3dafd0cbdf897c80258d57415c80.json\">"
