#!runghc
-- See README.md for an overview.

module Main (main) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, liftIO)
import Sytyc

cgiMain :: CGI CGIResult
cgiMain = do
  template <- liftIO $ exReadFile template_html
  problems_partial <- liftIO $ exReadFile problems_html
  footer <- liftIO $ footer_text
  let page = parseTemplate [ ("TEMPLATE_CONTENT", problems_partial)
                           , ("NAME", prog_name)
                           , ("FOOTER", footer)] 
                           template
  output page

  
main :: IO ()
main = do
  runCGI $ handleErrors cgiMain