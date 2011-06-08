-- Common functions for Sytyc
module Sytyc
  ( parseTemplate
  , exReadFile
  , parseTemplateFile
  , parseMarkdownFile
  , parseResultTemplate
  , nToBR
  , src_dir
  , problem_dir
  , template_dir
  , tmp_dir
  , supported_languages
  , result_file
  , template_html
  , problem_html
  , prog_name
  , footer
  , version
  ) where
  
import Prelude hiding (catch)
import Control.Exception (IOException, catch)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.Pandoc.Parsing (defaultParserState)
import Data.String.Utils (replace)

------------------------------------------------------------------
-- Constants
prog_name = "Sytyc - Programming Judge"
version = "1.0"
src_dir = "./test/"
problem_dir = "./problems/"
template_dir = "./templates/"
tmp_dir = "tmp" -- Do NOT add the slash at the end!

supported_languages = [ "haskell"
                      , "java"
                      ]
                      
result_file = "result.md"

template_html = "template.html"
problem_html = "problem.html"
footer = "Sytyc " ++ version

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


-- | Takes a string and replaces all newline characters \n with <br>.
nToBR :: String -> String
nToBR = replace "\n" "<br>"