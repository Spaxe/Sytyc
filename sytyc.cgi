#!runhaskell

module Main
  ( main
  ) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, setHeader)
import System.IO (readFile)
import System.Process (readProcess)

------------------------------------------------------------------
-- Constants
dir = "./test/"
src = "hs.hs"
html = "problem.html"
test = "Mrraa!"
runhaskell = "runhaskell"

------------------------------------------------------------------
-- Page Construction
parseTemplate :: String -> IO String
parseTemplate template = do
  readFile template
  
------------------------------------------------------------------
-- Entry functions
main :: IO ()
main = do
  result <- readProcess runhaskell [dir ++ src] test
  page <- parseTemplate html
  runCGI $ handleErrors $ output $ page