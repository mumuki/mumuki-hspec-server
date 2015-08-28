module Interpreter.Exit(
  toStatus) where

import           System.Exit

toStatus :: ExitCode -> String
toStatus (ExitFailure _) = "errored"
toStatus _               = "passed"