#!runhaskell

-- This file requires GHC version >= 7.0.3

module Main
  ( main
  ) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, getInput,
                    liftIO)
import System.IO (readFile)
import System.Process (readProcess)
import System.Random (getStdRandom, getStdGen)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)
import Text.Pandoc.Templates (renderTemplate)
import Data.Hash (hash, Hash)

------------------------------------------------------------------
-- Constants

src_dir = "./test/"
src = "hs.hs"

problem_html = "problem.html"
problem_file = "001_Summation-of-Integers.md"
problem_dir = "./problems/"

------------------------------------------------------------------
-- Page Construction
-- | parseTemplate takes a HTML file with Pandoc template styles, and replaces
-- | the variables in between $ $ with template_strings.
parseTemplate :: [(String, String)] -> String -> IO String
parseTemplate template_strings template = do
  t <- readFile template
  return $ renderTemplate template_strings t
  
-- | parseProblem takes a markdown template, and outputs HTML.
parseProblem :: String -> IO String
parseProblem problem = do
  file <- readFile problem
  return $ writeHtmlString defaultWriterOptions 
         $ readMarkdown defaultParserState file
  
------------------------------------------------------------------
-- Solution verification
checkSolution :: String -> String -> IO String
checkSolution compiler solution = do
  
  case compiler of
    "haskell" ->
    
    "mash" ->

runProgram :: String -> [String] -> String -> IO String
runProgram program args input = do
  readProcess program args input
  
------------------------------------------------------------------
-- Saves temporary file for compilation
-- | generateTempPath takes a string and hashes it together randomly.
generateTempPath :: String -> IO String
generateTempPath s = do
  randomNum <- getStdRandom
  

------------------------------------------------------------------
-- Entry functions
cgiMain :: CGI CGIResult
cgiMain = do
  result <- getInput "solution"
  let r = case result of
            Just a -> a
            Nothing -> "No"
  problem <- liftIO $ parseProblem (problem_dir ++ problem_file)
  let template_strings = [ ("NAME", "Sytyc - Programming Judge")
                         , ("PROBLEM", problem)
                         , ("RESULT", r)
                         ]
  page <- liftIO $ parseTemplate template_strings problem_html
  output page

main :: IO ()
main = do
  runCGI $ handleErrors cgiMain