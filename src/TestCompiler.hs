module TestCompiler (compile) where

  compile :: String -> String -> String -> String
  compile testSrc extraSrc contentSrc =
     "import Test.Hspec\n\
     \import Test.Hspec.JsonFormatter\n\
     \import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter))\n\
     \import Test.QuickCheck\n" ++
     contentSrc ++ "\n" ++
     extraSrc ++ "\n\
     \main :: IO ()\n\
     \main = hspecWith defaultConfig {configFormatter = Just jsonFormatter} $ do\n" ++
      testSrc
