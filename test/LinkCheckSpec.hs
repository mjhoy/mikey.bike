module LinkCheckSpec where

import LinkCheck
  ( cssUrls
  , hasScheme
  , percentDecode
  , target
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "target" $ do
    let t = target "_site" "_site/j/a.html"
    it "resolves root-relative urls" $
      t "/images/x.png" `shouldBe` Just "_site/images/x.png"
    it "resolves page-relative urls" $ do
      t "b.html" `shouldBe` Just "_site/j/b.html"
      t "../x.html" `shouldBe` Just "_site/j/../x.html"
    it "strips query and fragment" $
      t "/a.css?v=1#f" `shouldBe` Just "_site/a.css"
    it "ignores external and empty urls" $
      mapM_ (\u -> (u, t u) `shouldBe` (u, Nothing)) ["https://x.com/a", "mailto:a@b.c", "//cdn.x/y", "#top", ""]
    it "percent-decodes" $
      t "/my%20photo.jpg" `shouldBe` Just "_site/my photo.jpg"

  describe "cssUrls" $ do
    it "handles quoting, spacing and case" $ do
      cssUrls "url(/a.png)" `shouldBe` ["/a.png"]
      cssUrls "url(\"/b.png\")" `shouldBe` ["/b.png"]
      cssUrls "url( '/c.png' )" `shouldBe` ["/c.png"]
      cssUrls "URL(/d.png)" `shouldBe` ["/d.png"]
    it "finds multiple urls" $
      cssUrls "a { background: url(/a.png) } b { background: url(/b.png) }" `shouldBe` ["/a.png", "/b.png"]
    it "returns nothing when there are none" $
      cssUrls "body { color: red }" `shouldBe` []

  describe "hasScheme" $ do
    it "detects schemes" $
      mapM_ (\u -> (u, hasScheme u) `shouldBe` (u, True)) ["https://x", "mailto:x", "data:image/png;base64,x"]
    it "rejects non-schemes" $
      mapM_ (\u -> (u, hasScheme u) `shouldBe` (u, False)) ["/a:b", "a/b:c", "foo.html", ""]

  describe "percentDecode" $ do
    it "decodes escapes" $ do
      percentDecode "%20" `shouldBe` " "
      percentDecode "caf%C3%A9" `shouldBe` "caf\233"
    it "leaves malformed escapes" $ do
      percentDecode "100%" `shouldBe` "100%"
      percentDecode "%zz" `shouldBe` "%zz"

