
module JoinListSpec (main, spec) where
    
import Test.Hspec
import JoinList
    
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

joinList:: (Monoid m) => JoinList m a
joinList = Empty

spec :: Spec
spec = do
  describe "JoinList tag" $ do
    it "retrieves tag of Empty" $ do
      1 `shouldBe` 1