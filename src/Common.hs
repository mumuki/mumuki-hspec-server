module Common(
  Status(..),
  Raw
) where

data Status = Passed | Aborted | Errored | Failed deriving Eq

type Raw = (Status, String)

instance Show Status where
  show Passed = "passed"
  show Aborted = "aborted"
  show Errored = "errored"
  show Failed = "failed"