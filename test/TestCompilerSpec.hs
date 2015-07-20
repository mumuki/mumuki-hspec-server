module TestCompilerSpec (spec) where

import           Test.Hspec
import           TestCompiler (compile)

sampleContent = "x = True"

sampleTest = "describe \"x\" $ do\n\
              \  it \"should be True\" $ do\n\
              \    x `shouldBe` True"

expectedCompilation = "import Test.Hspec\n\
                       \import Test.Hspec.Formatters.Structured\n\
                       \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
                       \import Test.QuickCheck\n\
                       \x = True\n\n\
                       \main :: IO ()\n\
                       \main = hspecWith defaultConfig {configFormatter = Just structured} $ do\n\
                       \describe \"x\" $ do\n\
                       \  it \"should be True\" $ do\n\
                       \    x `shouldBe` True"
spec :: Spec
spec = do
  describe "TestCompiler.compile" $ do
    it "joins test and content" $ do
      compile sampleTest "" sampleContent `shouldBe` expectedCompilation
