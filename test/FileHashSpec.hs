module FileHashSpec where

import FileHash (hash)
import Test.Hspec

spec :: Spec
spec = do
  describe "hash" $ do
    it "should correctly hash a file" $ do
      res <- hash "test/fixtures/test-file-hash.json"
      res `shouldBe` "this should fail"
