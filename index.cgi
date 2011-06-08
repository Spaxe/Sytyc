#!runghc
-- See README.md for an overview.

module Main (main) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, liftIO)
import Sytyc

------------------------------------------------------------------
-- Constants
index_file = "index.md"

cgiMain :: CGI CGIResult
cgiMain = do
  template <- liftIO $ exReadFile template_html
  index_partial <- liftIO $ parseMarkdownFile $ template_dir ++ index_file
  footer <- liftIO $ footer_text
  let page = parseTemplate [ ("TEMPLATE_CONTENT", index_partial)
                           , ("NAME", prog_name)
                           , ("VERSION", footer)] 
                           template
  output page

  
main :: IO ()
main = do
  runCGI $ handleErrors cgiMain