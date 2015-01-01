module TestRunnerSpec (main, spec) where

import           Test.Hspec
import           TestRunner (runTest)
import           Control.Monad.Trans (liftIO)


main :: IO ()
main = hspec spec

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

spec = do
  describe "TestRunnerSpec.runTest" $ do
    it "passes when test is ok" $ do
      (exit, out) <- liftIO $ runTest sampleOkCompilation
      exit `shouldBe` "passed"

    it "failes when test is not ok" $ do
      (exit, out) <- liftIO $ runTest sampleNotOkCompilation
      exit `shouldBe` "failed"
