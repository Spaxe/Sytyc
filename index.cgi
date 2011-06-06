#!runhaskell

module Main
  ( main
  ) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, setHeader)
import System.IO (hFlush, stdout, openFile, ReadMode)
import System.Process (readProcess)

------------------------------------------------------------------
-- Constants
dir = "./test/"
src = "hs.hs"
test = "Mrraa!"
runhaskell = "runhaskell"

------------------------------------------------------------------
-- Page Construction
createPage :: String -> CGI CGIResult
createPage result = do
  -- Turns out setting the UTF-8 encoding is not easy with Network.CGI
  -- setHeader "charset" "utf-8" 
  output result
  
parseTemplate :: String -> IO String
parseTemplate template = do
  file <- openFile template ReadMode
  contents <- getContents file
  
------------------------------------------------------------------
-- Entry functions
main :: IO ()
main = do
  result <- readProcess runhaskell [dir ++ src] test
  runCGI $ handleErrors $ createPage result