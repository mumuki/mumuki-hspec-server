module Common(
  Status(..)
) where

data Status = Passed | Aborted | Errored | Failed deriving Eq

instance Show Status where
  show Passed = "passed"
  show Aborted = "aborted"
  show Errored = "errored"
  show Failed = "failed"