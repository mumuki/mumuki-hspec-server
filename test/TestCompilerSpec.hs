module TestCompilerSpec (main, spec) where

import           Test.Hspec
import           TestCompiler (compile)

main :: IO ()
main = hspec spec

sample_content = "x = True"

sample_test = "describe \"x\" $ do\n\
              \  it \"should be True\" $ do\n\
              \    x `shouldBe` True"

expected_compilation = "import Test.Hspec\n\
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
      compile sample_test sample_content `shouldBe` expected_compilation
