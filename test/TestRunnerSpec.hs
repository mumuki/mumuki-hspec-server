module TestRunnerSpec (spec) where

import           Test.Hspec
import           TestRunner (runTest)
import           Data.List (isInfixOf)

sampleOkCompilation = "import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \import Test.Hspec.JsonFormatter\n\
                       \x = True\n\
                       \main :: IO ()\n\
                       \main = hspec $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    x `shouldBe` True"

sampleNotOkCompilation = "import Test.Hspec\n\
                        \import Test.QuickCheck\n\
                        \import Test.Hspec.JsonFormatter\n\
                        \x = False\n\
                        \main :: IO ()\n\
                        \main = hspec $ do\n\
                        \describe \"x\" $ do\n\
                        \  it \"should be True\" $ do\n\
                        \    x `shouldBe` True"

spec :: Spec
spec = do
  describe "TestRunnerSpec.runTest" $ do

    context "when test is ok" $ do
      let result = runTest sampleOkCompilation

      it "passes" $ do
        (fmap fst result) `shouldReturn` "passed"

      it "outputs proper message" $ do
        (_, out) <- result
        out `shouldSatisfy` (isInfixOf "1 example, 0 failures")

    it "fails when test is not ok" $ do
      (fmap fst. runTest) sampleNotOkCompilation `shouldReturn` "failed"

