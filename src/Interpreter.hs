module Interpreter (
  runCode,
  CommandExit,
  CommandReader,
  RunnerResult(..)) where

import qualified Config

import           System.Process (readProcessWithExitCode)
import           System.Exit
import           System.IO (hClose, hPutStr)
import           System.Directory (removeFile)
import           System.IO.Temp (openTempFile)
import           System.Directory (getTemporaryDirectory)
import           Control.Concurrent
import           Control.Concurrent.Async (race)

type CommandExit = (ExitCode, String, String)

type CommandReader a = CommandExit -> RunnerResult a

data RunnerResult a = Ok a | Error (String, String) deriving (Show, Eq)

runCode :: CommandReader a -> String -> IO (RunnerResult a)
runCode f code = do
  path <- writeTempFile code
  commandExit <- runCommand path
  removeFile path
  return.readCommandExit f $ commandExit

writeTempFile :: String -> IO FilePath
writeTempFile code = do
  base <- getTemporaryDirectory
  (path, fileHandle) <- openTempFile base "compilation"
  hPutStr fileHandle code
  hClose fileHandle
  return path

readCommandExit :: CommandReader a -> Maybe CommandExit -> RunnerResult a
readCommandExit f (Just result) = f result
readCommandExit _ Nothing       = Error ("aborted", message)
    where message = "Command took more than 4.5 seconds. Command was aborted"

runCommand :: String -> IO (Maybe CommandExit)
runCommand path = limited 4500000 command
    where commandArgs = [ "1024", "4", "runhaskell" ] ++ Config.runhaskellArgs ++ [ path ]
          command = readProcessWithExitCode "./limit" commandArgs ""

limited :: Int -> IO a -> IO (Maybe a)
limited n f = fmap get $ race f (threadDelay n)
  where get (Left a) = Just a
        get _        = Nothing
