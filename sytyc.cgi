#!runghc

module Main (main) where

import Control.DeepSeq (rnf)

import System.IO (openTempFile, hPutStr, hClose, FilePath, hGetContents)
import System.Process (readProcessWithExitCode, createProcess, waitForProcess,
                       proc, CreateProcess(..), StdStream(..))
import System.Directory (removeFile, createDirectoryIfMissing, 
                         removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, getInput,
                    liftIO)
import Data.String.Utils (replace)
import Sytyc
  
------------------------------------------------------------------
-- Constants
problem_file = "001_Summation-of-Integers.md"
  
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
  hClose hin -- TODO: add stdin based on problem
  out_msg' <- hGetContents hout
  err_msg' <- hGetContents herr
  -- Here we _force_ the file to be read.
  -- Dark magic of Haskell
  rnf out_msg' `seq` hClose hout
  rnf err_msg' `seq` hClose herr
  let out_msg'' = replace className "Main" out_msg'
  let err_msg'' = replace className "Main" err_msg'
  exitcode' <- waitForProcess hJava
  let msg = case (exitcode, exitcode') of
              (ExitFailure code, _) -> compiler_error
                where
                  compiler_error = replace (tmpName ++ ":") "Line "
                                  $ nToBR $ -- "Compilation failed with " 
                                           -- ++ (show exitcode)
                                           -- ++ "\n"
                                           out_msg
                                           ++ "\n"
                                           ++ err_msg
              (ExitSuccess, ExitFailure code) -> runtime_msg
                where 
                  runtime_msg = nToBR $ -- "Execution failed with "
                                      -- ++ (show exitcode)
                                      -- ++ "\n"
                                      out_msg''
                                      ++ "\n" 
                                      ++ err_msg''
              (_, _) -> out_msg''
  removeDirectoryRecursive tmp_dir
  return msg

------------------------------------------------------------------
-- Entry functions
cgiMain :: CGI CGIResult
cgiMain = do
  r <- getInput "solution"
  lang <- getInput "language"
  let r' = case r of
             Just a -> a
             Nothing -> ""
  let lang' = case lang of
                Just a -> a
                Nothing -> ""
  result <- case lang' of
              "haskell" -> liftIO $ runghc r'
              "java"    -> liftIO $ runJava r'
              _         -> return ""
  
  result_partial <- liftIO $ parseResultTemplate $ nToBR result
  problem_partial <- liftIO $ parseMarkdownFile $ problem_dir ++ problem_file
  template <- liftIO $ exReadFile template_html
  this_page <- liftIO $ exReadFile problem_html
  footer <- liftIO $ footer_text
  let page = parseTemplate [ ("TEMPLATE_CONTENT", this_page)
                           , ("NAME", prog_name)
                           , ("FOOTER", footer)
                           ] template
  let template_strings = [ ("PROBLEM", problem_partial)
                         , ("RESULT_TEMPLATE", result_partial)
                         , ("SOURCE_CODE", r')
                         ]
  output $ parseTemplate template_strings page

  
main :: IO ()
main = do
  runCGI $ handleErrors cgiMain