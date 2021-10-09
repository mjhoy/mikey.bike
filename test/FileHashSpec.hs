module FileHashSpec where

import           FileHash                       ( hash )
import           Test.Hspec

spec :: Spec
spec = do
  describe "hash" $ do
    it "should correctly hash a file" $ do
      res <- hash "test/fixtures/test-file-hash.json"
      res `shouldBe` "b9cd2605ea75293b16b892a97c5e4b0bc18f3dafd0cbdf897c80258d57415c80"
