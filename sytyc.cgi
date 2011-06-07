#!runhaskell

-- This file requires GHC version >= 7.0.3

module Main
  ( main
  ) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, getInput)
import System.IO (readFile)
import System.Process (readProcess)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)
import Text.Pandoc.Templates (renderTemplate)

------------------------------------------------------------------
-- Constants

src_dir = "./test/"
src = "hs.hs"

problem_html = "problem.html"
problem_file = "001_Summation-of-Integers.md"
problem_dir = "./problems/"

test = "Mrraa!"
runhaskell = "runhaskell"

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
-- Entry functions
main :: IO ()
main = do
  problem <- parseProblem (problem_dir ++ problem_file)
  result <- getInput "solution"
  let template_strings = [ ("NAME", "Sytyc - Programming Judge")
                         , ("PROBLEM", problem)
                         , ("RESULT", result)
                         ]
  -- result <- readProcess runhaskell [src_dir ++ src] test
  page <- parseTemplate template_strings problem_html
  runCGI $ handleErrors $ output $ page
