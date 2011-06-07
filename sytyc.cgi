#!runhaskell

-- This file requires GHC version >= 7.0.3

module Main
  ( main
  ) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, getInput,
                    liftIO)
import System.IO (openTempFile, hPutStr, hClose)
import System.Process (readProcess)
import System.Directory (removeFile)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)
import Text.Pandoc.Templates (renderTemplate)
import Data.Hash (hash, Hash, combine, asWord64)

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

-- | Runs a Haskell file
runHaskellFile :: String -> IO String
runHaskellFile content = do
  (tmpName, tmpHandle) <- openTempFile "./tmp" "temp"
  hPutStr tmpHandle content
  hClose tmpHandle
  s <- readProcess "runhaskell" [tmpName] []
  removeFile tmpName
  return s

------------------------------------------------------------------
-- Entry functions
cgiMain :: CGI CGIResult
cgiMain = do
  result <- getInput "solution"
  let result' = case result of
                  Just a -> a
                  Nothing -> ""
  problem <- liftIO $ parseProblem (problem_dir ++ problem_file)
  r <- liftIO $ runHaskellFile result'
  let template_strings = [ ("NAME", "Sytyc - Programming Judge")
                         , ("PROBLEM", problem)
                         , ("RESULT", r)
                         ]
  page <- liftIO $ parseTemplate template_strings problem_html
  output page

main :: IO ()
main = do
  runCGI $ handleErrors cgiMain