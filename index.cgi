#!runhaskell

module Main
  ( main
  ) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output)
import System.IO (hFlush, stdout)
import System.Process (readProcess)

------------------------------------------------------------------
-- Constants
dir = "./test/"
src = "hs.hs"
test = "Mrraa!"

------------------------------------------------------------------
-- Running an external program via a new process
runProgram :: String -> [String] -> String -> IO String
runProgram program args input = do
  readProcess program args input
  
runhaskell :: String -> IO String
runhaskell = runProgram "runhaskell" [dir ++ src]
  
------------------------------------------------------------------
-- Entry functions
main :: IO ()
main = do
  result <- runhaskell test
  runCGI (handleErrors $ output result)