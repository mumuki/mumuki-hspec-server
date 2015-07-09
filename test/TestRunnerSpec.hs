module TestRunnerSpec (spec) where

import           Test.Hspec
import           TestRunner (runTest)
import           Data.List (isInfixOf)

sampleOkCompilation = "import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \import Test.Hspec.Formatters.Structured\n\
                       \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                       \x = True\n\
                       \main :: IO ()\n\
                       \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    x `shouldBe` True"

sampleNotOkCompilation = "import Test.Hspec\n\
                        \import Test.QuickCheck\n\
                        \import Test.Hspec.Formatters.Structured\n\
                        \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                        \x = False\n\
                        \main :: IO ()\n\
                        \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                        \describe \"x\" $ do\n\
                        \  it \"should be True\" $ do\n\
                        \    x `shouldBe` True"

sampleNotCompilingCompilation = "import Test.Hspec\n\
                                \import Test.QuickCheck\n\
                                \import Test.Hspec.Formatters.Structured\n\
                                \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                                \main :: IO ()\n\
                                \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
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
        out `shouldSatisfy` (isInfixOf "1 exmple, 0 failures")

    context "when test is not ok" $ do
      let result = runTest sampleNotOkCompilation

      it "fails" $ do
        (fmap fst result) `shouldReturn` "failed"

      it "outputs proper message" $ do
        (_, out) <- result
        out `shouldSatisfy` (isInfixOf "1 exmple, 0 failures")

    context "when test does not compile" $ do
      let result = runTest sampleNotCompilingCompilation

      it "fails" $ do
        (fmap fst result) `shouldReturn` "failed"

      it "outputs proper message" $ do
        (_, out) <- result
        out `shouldSatisfy` (isInfixOf "1 exmple, 0 failures")

