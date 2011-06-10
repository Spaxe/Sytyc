#!runghc

module Main (main) where

import Control.DeepSeq (rnf)

import System.IO (openTempFile, hPutStr, hClose, FilePath, hGetContents, hFlush)
import System.Process (readProcessWithExitCode, createProcess, waitForProcess,
                       proc, CreateProcess(..), StdStream(..))
import System.Directory (removeFile, createDirectoryIfMissing, 
                         removeDirectoryRecursive, getDirectoryContents)
import System.Exit (ExitCode(..))
import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, getInput,
                    liftIO)
import Data.String.Utils (replace, strip)
import Data.List (delete)
import Sytyc

------------------------------------------------------------------
-- External process execution
-- Unfortunately it's not exactly unified.
-- 
-- Edit this section with care. Chances are, more things break if changed.

-- Haskell
runghc :: String -> String -> IO String
runghc source input = do
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
  removeFile tmpName
  return msg
  
-- Java
-- The source code must have "public class Main".
-- Pain in the ass.
-- "why do you hate your sanity?" ~ Nick Hodge, Microsoft Australia
runJava :: String -> String -> IO String
-- Java does not generate .class files if the source is empty. Annoying.
runJava "" _ = return ""
runJava source input = do
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
  hPutStr hin input
  hFlush hin
  hClose hin
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
                                  $ nToBR $ out_msg
                                          ++ "\n"
                                          ++ err_msg
              (ExitSuccess, ExitFailure code) -> runtime_msg
                where 
                  runtime_msg = nToBR $ out_msg''
                                      ++ "\n" 
                                      ++ err_msg''
              (_, _) -> out_msg''
  removeFile tmpName
  removeFile $ replace ".java" ".class" tmpName
  return msg
  

-- | Runs a Mash program. Delegates most of the work to runJava.
runMash :: String -> String -> IO String
runMash source input = do
  createDirectoryIfMissing False tmp_dir
  (tmpName, tmpHandle) <- openTempFile tmp_dir "Main.mash"
  let className = replace "tmp\\" "" 
                $ replace "tmp/" "" 
                  tmpName
  let className' = replace ".mash" "" className
  hPutStr tmpHandle source
  hClose tmpHandle
  (Just hin, Just hout, Just herr, hMash) <-
    createProcess (proc "mashc" [className])
                    { cwd = Just tmp_dir
                    , std_in = CreatePipe
                    , std_err = CreatePipe
                    , std_out = CreatePipe
                    }
  hClose hin -- Not used. Passed onto Java instead
  out_msg <- hGetContents hout
  err_msg <- hGetContents herr
  -- Here we _force_ the file to be read.
  -- Dark magic of Haskell
  rnf out_msg `seq` hClose hout
  rnf err_msg `seq` hClose herr
  let out_msg' = replace className "Main" out_msg
  let err_msg' = replace className "Main" err_msg
  exitcode <- waitForProcess hMash
  msg <- case exitcode of
           ExitFailure code -> return compiler_error
             where
               compiler_error = replace (className ++ ":") "Line "
                                $ nToBR $ out_msg
                                        ++ className
                                        ++ "\n"
                                        ++ err_msg
           ExitSuccess -> do
             java_source <- exReadFile $ replace ".mash" ".java" tmpName 
             runJava 
               (replace ("class " ++ className') "class Main" java_source)
               input
  removeFile tmpName
  removeFile $ replace ".mash" ".java" tmpName
  return msg
  
------------------------------------------------------------------
-- Verify program's correctness
verifyProgram :: String -> String -> [FilePath] -> [FilePath] -> IO String
verifyProgram source language inputs outputs = do
  let compiler = case language of
                   "haskell" -> runghc
                   "java"    -> runJava
                   _         -> runMash -- Defaults to mash
  r <- verifyProgram' source compiler inputs outputs "" True
  -- r <- verifyProgram' source compiler [(head inputs)] [(head outputs)] "" True
  let correctness = case r of
                      "" -> "Correct!"
                      _    -> r
  return correctness
    where
      verifyProgram' :: String
                     -> (String -> String -> IO String) 
                     -> [FilePath] 
                     -> [FilePath] 
                     -> String
                     -> Bool
                     -> IO String
      verifyProgram' _ _ _ _ msg False = return msg
      verifyProgram' _ _ [] _ msg _ = return msg
      verifyProgram' _ _ _ [] msg _ = return msg
      verifyProgram' source compiler (i:inputs) (o:outputs) m correctness = do
        input <- exReadFile i
        let input' = strip input
        r <- compiler source input'
        let r' = strip r
        answer <- exReadFile o
        let answer' = strip answer
        let result = (r' == answer') && correctness
        let msg = case result of 
                    True -> ""
                    _    -> "Incorrect.\n\nYour result:\n"
                            ++ r'
                            ++ "\n\n"
                            ++ "Expected result:\n"
                            ++ answer'
                            ++ "\n\n"
                            ++ "Input used:\n"
                            ++ input'
        verifyProgram' source compiler inputs outputs msg result
      
      
-- | Given a problem name, finds all of its test inputs and outputs and return
-- | their file paths.
getProblemIO :: String -> IO ([FilePath], [FilePath])
getProblemIO problem = do
  let this_problem_dir = problem_dir ++ problem ++ "/"
  let p_input = this_problem_dir ++ "input/"
  let p_output = this_problem_dir ++ "output/"
  input_contents <- getDirectoryContents p_input
  output_contents <- getDirectoryContents p_output
  let input_contents' = delete "." $ delete ".." input_contents
  let output_contents' = delete "."  $ delete ".." output_contents
  let inputs = map ((++) p_input) input_contents'
  let outputs = map ((++) p_output) output_contents'
  return (inputs, outputs)
      
------------------------------------------------------------------
-- Entry functions
cgiMain :: CGI CGIResult
cgiMain = do
  r <- getInput "solution"
  let r' = case r of
             Just a -> a
             Nothing -> ""
  -- Uncomment the following lines to support other languages. 
  -- See the documentation for more instructions. Warning, untested.
  {-
  lang <- getInput "language"
  let lang' = case lang of
                Just a -> a
                Nothing -> ""
  result <- case lang' of
              "haskell" -> liftIO $ verifyProgram r' "haskell" inputs outputs
              "java"    -> liftIO $ verifyProgram r' "java" inputs outputs
              "mash"    -> liftIO $ verifyProgram r' "mash" inputs outputs
              _         -> return "Don't forget to choose a language."
  -}
  -- And comment these ones out. 
  problem_number <- getInput "p"
  let problem_name = case problem_number of
                       Just a -> a
                       Nothing -> "null"
  (inputs, outputs) <- liftIO $ getProblemIO problem_name
  result <- case r' of
              "" -> return ""
              _  -> liftIO $ verifyProgram r' "mash" inputs outputs
  -- ^
  result_partial <- liftIO $ parseResultTemplate $ nToBR result
  -- TODO stop hard coding problem names
  problem_partial <- liftIO $ parseMarkdownFile $ problem_dir 
                                                ++ problem_name ++ "/"
                                                ++ problem_file
  template <- liftIO $ exReadFile template_html
  this_page <- liftIO $ exReadFile problem_html
  footer <- liftIO $ footer_text
  let page = parseTemplate [ ("TEMPLATE_CONTENT", this_page)
                           , ("NAME", prog_name ++ " " ++ p)
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