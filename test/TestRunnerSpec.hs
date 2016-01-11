module TestRunnerSpec (spec) where

import           Test.Hspec
import           Common
import           Protocol.Test.Test
import           Server.Test.TestRunner
import           Data.List (isInfixOf)

sampleOkCompilation = "{-# OPTIONS_GHC -fdefer-type-errors #-}\n\
                       \import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \import Test.Hspec.Formatters.Structured\n\
                       \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                       \x = True\n\
                       \main :: IO ()\n\
                       \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    x `shouldBe` True"

sampleOkCompilationWithWarnings = "{-# OPTIONS_GHC -fdefer-type-errors #-}\n\
                       \import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \import Test.Hspec.Formatters.Structured\n\
                       \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                       \x = True\n\
                       \foo x = True\n\
                       \foo _ = False\n\
                       \main :: IO ()\n\
                       \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    x `shouldBe` True"

sampleNotOkCompilationWithTypeErrors = "{-# OPTIONS_GHC -fdefer-type-errors #-}\n\
                       \import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \import Test.Hspec.Formatters.Structured\n\
                       \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                       \foo x = x + True\n\
                       \main :: IO ()\n\
                       \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                       \describe \"foo\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    foo 2 `shouldBe` True"

sampleNotOkCompilationWithTypeErrorsInTest = "{-# OPTIONS_GHC -fdefer-type-errors #-}\n\
                       \import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \import Test.Hspec.Formatters.Structured\n\
                       \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                       \foo x = x + 2\n\
                       \main :: IO ()\n\
                       \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                       \describe \"foo\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    foo 2 `shouldBe` True"


sampleOkWithEscapedStringsCompilation = "{-# OPTIONS_GHC -fdefer-type-errors #-}\n\
                       \import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \import Test.Hspec.Formatters.Structured\n\
                       \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                       \x = \"hello\"\n\
                       \main :: IO ()\n\
                       \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be \\\"hello\\\"\" $ do\n\
                       \    x `shouldBe` \"hello\""


sampleNotOkCompilation = "{-# OPTIONS_GHC -fdefer-type-errors #-}\n\
                        \import Test.Hspec\n\
                        \import Test.QuickCheck\n\
                        \import Test.Hspec.Formatters.Structured\n\
                        \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                        \x = False\n\
                        \main :: IO ()\n\
                        \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                        \describe \"x\" $ do\n\
                        \  it \"should be True\" $ do\n\
                        \    x `shouldBe` True"

sampleNotCompilingCompilation = "{-# OPTIONS_GHC -fdefer-type-errors #-}\n\
                                \import Test.Hspec\n\
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

    context "when test is ok but has warnings" $ do
      let result = runTest sampleOkCompilationWithWarnings

      it "answers structured data" $ do
        result `shouldReturn` Ok [TestResult "x should be True" "passed" ""]

    context "when test is ok with escaped strings" $ do
      let result = runTest sampleOkWithEscapedStringsCompilation

      it "answers structured data" $ do
        result `shouldReturn` Ok [TestResult "x should be \"hello\"" "passed" ""]

    context "when test is not ok" $ do
      let result = runTest sampleNotOkCompilation

      it "answers structured data" $ do
        result `shouldReturn` Ok [TestResult "x should be True" "failed" "expected: True\n but got: False"]

    context "when test is not ok with type errors" $ do
      let result = runTest sampleNotOkCompilationWithTypeErrors

      it "answers structured data" $ do
        Ok [TestResult _ "failed" message] <- result
        message `shouldSatisfy` (isInfixOf "deferred type error")

    context "when test is not ok with type errors in test" $ do
      let result = runTest sampleNotOkCompilationWithTypeErrorsInTest

      it "answers structured data" $ do
        Ok [TestResult _ "failed" message] <- result
        message `shouldSatisfy` (isInfixOf "deferred type error")

    context "when test does not compile" $ do
      let result = runTest sampleNotCompilingCompilation

      it "fails" $ do
        Error (exit, _) <- result
        exit `shouldBe` Errored

      it "outputs proper message" $ do
        Error (_, out) <- result
        out `shouldSatisfy` (isInfixOf "Not in scope: `x'")

