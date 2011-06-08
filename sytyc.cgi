#!runghc
-- | Overview
-- | ========
-- | Sytyc is a web-based problem database with a compiler on the server side.
-- | A list of problems is presented to the user, from which they can pick a
-- | problem to work on, and submit their source code. The code is (unsafely)
-- | compiled server-side, and the result is then printed back onto the web
-- | page. Kudos is given when the program computes the correct results.
-- |
-- | Github repository: [https://github.com/SpaXe/Sytyc]
-- |
-- | Credits
-- | -------
-- | Author: Xavier Ho [contact@xavierho.com]
-- | Supervisor: Andrew Rock [a.rock@griffith.edu.au]
-- | And other Functional Programming classmates.
-- |
-- | Installation & Usage
-- | --------------------
-- | @TODO
-- | 
-- | Dependencies
-- | ------------
-- | Haskell dependencies:
-- | *   GHC version >= 7.0.3. As of Feb 2011, the official Haskell Platform 
-- | supports GHC 7.0.3. Sytyc was compiled against the GHC libraries.
-- | *   data-hash-0.1.0.0
-- | *   pandoc-1.8
-- |
-- | Server dependencies:
-- | *   Apache, or other server programs that support CGI.
-- |
-- | Other dependencies:
-- | *   Java SDK SE 6
-- | *   MaSH compiler [http://www.ict.griffith.edu.au/arock/MaSH/index.html]
-- |
-- | Customisation
-- | -------------
-- | @TODO
-- |
-- | License
-- | -------
-- | Sytyc by Xavier Ho is licensed under
-- | Creative Commons Attribution-ShareAlike 3.0 Unported License.
-- | [http://creativecommons.org/licenses/by-sa/3.0/]

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
problem_dir = "./problems/"

problem_html = "problem.html"
problem_file = "001_Summation-of-Integers.md"

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
runghcFile :: String -> IO String
runghcFile content = do
  (tmpName, tmpHandle) <- openTempFile "./tmp" "temp"
  hPutStr tmpHandle content
  hClose tmpHandle
  s <- readProcess "runghc" [tmpName] []
  removeFile tmpName
  return s
  
-- | Constructs the result
constructResult :: String -> String

------------------------------------------------------------------
-- Entry functions
cgiMain :: CGI CGIResult
cgiMain = do
  result <- getInput "solution"
  let result' = case result of
                  Just a -> a
                  Nothing -> ""
  problem <- liftIO $ parseProblem (problem_dir ++ problem_file)
  r <- liftIO $ runghcFile result'
  let template_strings = [ ("NAME", "Sytyc - Programming Judge")
                         , ("PROBLEM", problem)
                         , ("RESULT", r)
                         ]
  page <- liftIO $ parseTemplate template_strings problem_html
  output page

main :: IO ()
main = do
  runCGI $ handleErrors cgiMain