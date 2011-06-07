#!runhaskell
-- This file requires GHC version >= 7.0.3

module Main
  ( main
  ) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, setHeader)
import System.IO (readFile)
import System.Process (readProcess)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)

------------------------------------------------------------------
-- Constants
src_dir = "./test/"
src = "hs.hs"

html = "problem.html"

problem_file = "001_Summation-of-Integers.md"
problem_dir = "./problems/"

test = "Mrraa!"
runhaskell = "runhaskell"

------------------------------------------------------------------
-- Page Construction
parseTemplate :: String -> IO String
parseTemplate template = do
  readFile template
  
parseProblem :: String -> IO String
parseProblem problem = do
  file <- readFile problem
  return $ writeHtmlString defaultWriterOptions 
         $ readMarkdown defaultParserState file
  
  
------------------------------------------------------------------
-- Entry functions
main :: IO ()
main = do
  result <- readProcess runhaskell [src_dir ++ src] test
  page <- parseTemplate html
  problem <- parseProblem (problem_dir ++ problem_file)
  runCGI $ handleErrors $ output $ problem