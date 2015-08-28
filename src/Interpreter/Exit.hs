module Interpreter.Exit(
  toStatus) where

import           Common
import           System.Exit

toStatus :: ExitCode -> Status
toStatus (ExitFailure _) = Errored
toStatus _               = Passed