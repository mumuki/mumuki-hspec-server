module TestRunnerSpec (spec) where

import           Test.Hspec
import           Common
import           Protocol.Test.Test
import           Server.Test.TestRunner
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

      it "answers structured data" $ do
        result `shouldReturn` Ok [TestResult "x should be True" "passed" ""]

    context "when test is not ok" $ do
      let result = runTest sampleNotOkCompilation

      it "answers structured data" $ do
        result `shouldReturn` Ok [TestResult "x should be True" "failed" "expected: True\n but got: False"]

    context "when test does not compile" $ do
      let result = runTest sampleNotCompilingCompilation

      it "fails" $ do
        Error (exit, _) <- result
        exit `shouldBe` Errored

      it "outputs proper message" $ do
        Error (_, out) <- result
        out `shouldSatisfy` (isInfixOf "Not in scope: `x'")

