-- Common functions for Sytyc
module Sytyc
  ( parseTemplate
  , exReadFile
  , parseTemplateFile
  , parseMarkdownFile
  , parseResultTemplate
  , nToBR
  , problem_dir
  , template_dir
  , tmp_dir
  , supported_languages
  , result_file
  , template_html
  , problem_html
  , problem_file
  , prog_name
  , version
  , build_time
  , footer_text
  , problems_html
  , getProblemList
  ) where
  
import Prelude hiding (catch)
import Control.Exception (IOException, catch)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)
import Data.String.Utils (replace)
import System.Time (getClockTime, toCalendarTime, calendarTimeToString)
import System.Directory (getDirectoryContents)
import Data.List (delete, sort)

------------------------------------------------------------------
-- Constants
prog_name = "Sytyc - Programming Judge"
version = "1.0"
problem_dir = "./problems/"
template_dir = "./templates/"
tmp_dir = "tmp" -- Do NOT add the slash at the end!

supported_languages = [ "haskell"
                      , "java"
                      , "mash"
                      ]
                      
problem_file = "description.md"
                      
result_file = "result.md"

template_html = template_dir ++ "template.html"
problems_html = template_dir ++ "problems.html"
problem_html = template_dir ++ "problem.html"

------------------------------------------------------------------
-- Page construction
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
parseTemplateFile :: [(String, String)] -> FilePath -> IO String
parseTemplateFile template_strings template = do
  t <- exReadFile template
  return $ parseTemplate template_strings t
  
  
-- | Takes a markdown template, and outputs HTML.
parseMarkdownFile :: FilePath -> IO String
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


-- | Takes a string and replaces all newline characters \n with <br>.
nToBR :: String -> String
nToBR = replace "\n" "<br>"

------------------------------------------------------------------
-- Problem discovery
getProblemList :: IO [String]
getProblemList = do
  problems <- getDirectoryContents problem_dir
  return $ sort $ delete "." $ delete ".." problems 
  
------------------------------------------------------------------
-- Extra stuff
build_time :: IO String
build_time = do
  time <- getClockTime
  time' <- toCalendarTime time
  return $ calendarTimeToString time'
  
footer_text :: IO String
footer_text = do
  time <- build_time
  return $ "Sytyc " ++ version ++ " (Built on " ++ time ++ ")"