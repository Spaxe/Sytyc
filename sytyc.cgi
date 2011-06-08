#!runghc
-- See README.md for an overview.

module Main
  ( main
  ) where

import Prelude hiding (catch)
import Control.Exception (IOException, catch)

import System.IO (openTempFile, hPutStr, hClose, FilePath, hGetContents)
import System.Process (readProcessWithExitCode, createProcess, waitForProcess,
                       proc, CreateProcess(..), StdStream(..))
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
-- The source code must have "public class Main".
-- Pain in the ass.
runJava :: String -> IO String
-- Java does not generate .class files if the source is empty. Annoying.
runJava "" = return ""
runJava source = do
  createDirectoryIfMissing False tmp_dir
  (tmpName, tmpHandle) <- openTempFile tmp_dir "Main.java"
  let className = replace "tmp\\" "" 
                $ replace "tmp/" "" 
                $ replace ".java" ""
                  tmpName
  -- Hacky stuff. Could be improved.
  let source' = replace "class Main" ("class " ++ className) source
  hPutStr tmpHandle source'
  hClose tmpHandle

  (exitcode, out_msg, err_msg) <- readProcessWithExitCode
                                  "javac" [tmpName] []
  
  -- Java is annoying in the way that, you must somehow pass the path
  -- of the class file before it can run the class name. And its -cp flag
  -- doesn't work with the System.Process flags. We resort back to raw
  -- system command with changed working directory.
  (Just hin, Just hout, Just herr, hJava) <-
    createProcess (proc "java" [className])
                  { cwd = Just tmp_dir
                  , std_in = CreatePipe
                  , std_err = CreatePipe
                  , std_out = CreatePipe
                  }
  hClose hin
  exitcode' <- waitForProcess hJava
  out_msg' <- hGetContents hout
  err_msg' <- hGetContents herr
  hClose hout
  hClose herr
  -- (exitcode', out_msg', err_msg') <- readProcessWithExitCode
  --                                   "java" [ "-cp" ++ tmp_dir
  --                                          , className] []
  let msg = case (exitcode, exitcode') of
              (_, ExitSuccess) -> "Program works! Woot." ++ out_msg'
              -- (ExitSuccess, _) -> "Mrraa"
              (ExitFailure code, _) -> compiler_error
                where
                  compiler_error = replace (tmpName ++ ":") "Line "
                                  $ nToBR ("Compilation failed with " 
                                           ++ (show exitcode)
                                           ++ "\n"
                                           ++ out_msg
                                           ++ "\n"
                                           ++ err_msg
                                          ) 
              (_, _) -> runtime_msg
                where 
                  runtime_msg = nToBR $ "Runtime Error!\n"
                                      ++ out_msg' 
                                      ++ "\n" 
                                      ++ err_msg' 
      
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