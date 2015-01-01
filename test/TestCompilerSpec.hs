module TestCompilerSpec (main, spec) where

import           Test.Hspec
import           TestCompiler (compile)

main :: IO ()
main = hspec spec

sampleContent = "x = True"

sampleTest = "describe \"x\" $ do\n\
              \  it \"should be True\" $ do\n\
              \    x `shouldBe` True"

expectedCompilation = "import Test.Hspec\n\
                       \import Test.QuickCheck\n\
                       \x = True\n\
                       \main :: IO ()\n\
                       \main = hspec $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    x `shouldBe` True"
spec = do
  describe "TestCompiler.compile" $ do
    it "joins test and content" $ do
      compile sampleTest sampleContent `shouldBe` expectedCompilation
