module TestCompiler (compile) where

  compile :: String -> String -> String
  compile testSrc contentSrc =
     "import Test.Hspec\nimport Test.QuickCheck\n" ++
     contentSrc ++
     "main :: IO ()\nmain = hspec $ do\n\t" ++
      testSrc
