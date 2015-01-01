module TestRunnerSpec (spec) where

import           Test.Hspec
import           TestRunner (runTest)

sampleOkCompilation = "import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \x = True\n\
                       \main :: IO ()\n\
                       \main = hspec $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    x `shouldBe` True"

sampleNotOkCompilation = "import Test.Hspec\n\
                        \import Test.QuickCheck\n\
                        \x = False\n\
                        \main :: IO ()\n\
                        \main = hspec $ do\n\
                        \describe \"x\" $ do\n\
                        \  it \"should be True\" $ do\n\
                        \    x `shouldBe` True"
spec :: Spec
spec = do
  describe "TestRunnerSpec.runTest" $ do
    it "passes when test is ok" $ do
      (fmap fst . runTest) sampleOkCompilation `shouldReturn` "passed"

    it "failes when test is not ok" $ do
      (fmap fst. runTest) sampleNotOkCompilation `shouldReturn` "failed"
