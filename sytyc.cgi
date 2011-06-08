#!runghc
-- See README.md for an overview.

module Main
  ( main
  ) where

import Prelude hiding (catch)
import Control.Exception (IOException, catch)

import System.IO (openTempFile, hPutStr, hClose, FilePath)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, createDirectoryIfMissing, 
                         removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import Data.String.Utils (replace)
import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, getInput,
                    liftIO)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)
import Text.Pandoc.Templates (renderTemplate)

------------------------------------------------------------------
-- Constants
src_dir = "./test/"
problem_dir = "./problems/"
template_dir = "./templates/"
tmp_dir = "tmp" -- Do NOT add the slash at the end!

supported_languages = [ "haskell"
                      , "java"
                      ]
                      
result_file = "result.md"

problem_html = "problem.html"
problem_file = "001_Summation-of-Integers.md"

------------------------------------------------------------------
-- Page Construction
-- | A shorthand because I get them confused otherwise.
parseTemplate = renderTemplate


-- | Generic exception handling for reading a file.
exReadFile :: FilePath -> IO String
exReadFile f = 
  catch (readFile f)
        (\e -> return 
               $ nToBR 
               $ "readFile failed: " 
              ++ show (e :: IOException)
              ++ "\n\nPlease contact the admin about this error." )
                                   

-- | Takes a dictionary pairs of mapping (a, b) from a to b, and a
-- | file to be opened for parsing. Returns the template with the variables
-- | substituted. Variables should be wrapped with $ signs, like $this$.
-- | For more information, consult pandoc's renderTemplate documentation.
parseTemplateFile :: [(String, String)] -> String -> IO String
parseTemplateFile template_strings template = do
  t <- exReadFile template
  return $ parseTemplate template_strings t
  
  
-- | Takes a markdown template, and outputs HTML.
parseMarkdownFile :: String -> IO String
parseMarkdownFile mdFile = do
  file <- exReadFile mdFile
  return $ writeHtmlString defaultWriterOptions 
         $ readMarkdown defaultParserState file
         

-- | Takes the program feedback and put it into the result template in HTML.
-- | If the result is empty, the result HTML is also empty.
parseResultTemplate :: String -> IO String
parseResultTemplate result
  | result == "" = return ""
  | otherwise    = do
      template <- parseMarkdownFile (template_dir ++ result_file)
      return $ parseTemplate [("RESULT", result)] template
  
  
-- | Takes a problem name (filename) and returns the HTML.
parseProblemTemplate :: String -> IO String
parseProblemTemplate file = parseMarkdownFile (problem_dir ++ file)


-- | Takes a string and replaces all newline characters \n with <br>.
nToBR :: String -> String
nToBR = replace "\n" "<br>"
  
------------------------------------------------------------------
-- External process execution
-- Unfortunately it's not exactly unified.
-- 
-- Edit this section with care. Chances are, more things break if changed.


-- Haskell
runghc :: String -> IO String
runghc source = do
  createDirectoryIfMissing False tmp_dir
  (tmpName, tmpHandle) <- openTempFile tmp_dir "Main.hs"
  hPutStr tmpHandle source
  hClose tmpHandle
  (exitcode, out_msg, err_msg) <- readProcessWithExitCode
                                     "runghc" [tmpName] []
  let msg = case exitcode of
              ExitSuccess -> out_msg
              ExitFailure code -> failure_msg
                where 
                  failure_msg = replace (tmpName ++ ":") ""
                                $ nToBR ((show exitcode)
                                         ++ "\n"
                                         ++ out_msg
                                         ++ "\n"
                                         ++ err_msg)                                               
  removeDirectoryRecursive tmp_dir
  return msg
  
-- Java
runJava :: String -> IO String
runJava source = do
  createDirectoryIfMissing False tmp_dir
  (tmpName, tmpHandle) <- openTempFile tmp_dir "Main.java"
  let className = replace "tmp\\" "" 
                $ replace "tmp/" "" 
                $ replace ".java" ""
                  tmpName
  let bytecodeName = replace ".java" ".class" tmpName
  -- Hacky stuff. Could be improved.
  let source' = replace "class Main" ("class " ++ className) source
  hPutStr tmpHandle source'
  hClose tmpHandle
  -- putStrLn tmpName
  (exitcode, out_msg, err_msg) <- readProcessWithExitCode
                                     "javac" [tmpName] []
  (exitcode', out_msg', err_msg') <- readProcessWithExitCode
                                     "java" [bytecodeName] []
  let msg = case (exitcode, exitcode') of
              (_, ExitSuccess) -> out_msg'
              (ExitFailure code, _) -> compiler_error
                where
                  compiler_error = replace (tmpName ++ ":") ""
                                  $ nToBR ((show exitcode)
                                           ++ "\n"
                                           ++ out_msg
                                           ++ "\n"
                                           ++ err_msg)  
              (_, _) -> runtime_error
                where 
                  runtime_error = nToBR $ out_msg' ++ "\n" ++ err_msg'
      
  removeDirectoryRecursive tmp_dir
  return msg

------------------------------------------------------------------
-- Entry functions
cgiMain :: CGI CGIResult
cgiMain = do
  r <- getInput "solution"
  let r' = case r of
             Just a -> a
             Nothing -> ""
  result <- liftIO $ runJava r'
  
  result_partial <- liftIO $ parseResultTemplate result
  problem_partial <- liftIO $ parseProblemTemplate problem_file
  
  let template_strings = [ ("NAME", "Sytyc - Programming Judge")
                         , ("PROBLEM", problem_partial)
                         , ("RESULT_TEMPLATE", result_partial)
                         ]
  page <- liftIO $ parseTemplateFile template_strings problem_html
  output page

  
main :: IO ()
main = do
  runCGI $ handleErrors cgiMain