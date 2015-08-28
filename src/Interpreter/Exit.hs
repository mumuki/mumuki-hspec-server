module Interpreter.Exit(
  toStatus,
  toRaw) where

import           Common
import           System.Exit

toStatus :: ExitCode -> Status
toStatus (ExitFailure _) = Errored
toStatus _               = Passed

toRaw (exit, out, err) =  (toStatus exit, out ++ err)