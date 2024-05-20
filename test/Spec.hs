import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Example" $ do
    it "should work" $ do
      True `shouldBe` True
