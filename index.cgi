#!runhaskell

module Main
  ( main
  ) where

import System.IO (hFlush, stdout)
import System.Process (readProcess)
import Network.HTTP


siteURL = "http://sytyc.dev"
test = "Mrraa!"


main :: IO ()
main = do
  -- Prints header
  putStrLn "Content-type: text/html\n"
  hFlush stdout
  
  -- Runs the program
  putStrLn "Original Input:"
  putStrLn test
  
  let directory = "./test/"
  output <- runProgram "runhaskell" [directory ++ "hs.hs"] test
  
  putStrLn "Program output:"
  putStrLn output
  
  
runProgram :: String -> [String] -> String -> IO String
runProgram program args input = do
  readProcess program args input
  
  
buildRequestBody :: [String] -> String
buildRequestBody [] = ""
buildRequestBody (name:info:s) = buildRequestBody' s ("\"" ++ name ++ "\": \"" ++ info ++ "\"")
  where
    buildRequestBody' [] output = "{" ++ output ++ "}"
    buildRequestBody' (name:info:s) output = buildRequestBody' s (output ++ ", \"" ++ name ++ "\": \"" ++ info ++ "\"")