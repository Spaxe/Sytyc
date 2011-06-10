#!runghc
-- See README.md for an overview.

module Main (main) where

import Network.CGI (CGI, CGIResult, runCGI, handleErrors, output, liftIO)
import Sytyc

cgiMain :: CGI CGIResult
cgiMain = do
  template <- liftIO $ exReadFile template_html
  footer <- liftIO $ footer_text
  problem_list <- liftIO $ getProblemList
  let p = unlines $ map wrapLi problem_list
  problems_partial <- liftIO
                      $ parseTemplateFile [("PROBLEM_LIST", p)] problems_html
  let page = parseTemplate [ ("TEMPLATE_CONTENT", problems_partial)
                           , ("NAME", prog_name)
                           , ("FOOTER", footer)] 
                           template
  output page
    where
      wrapLi s = "<a href=\"problem.cgi?p=" 
              ++ s
              ++ "\"><li><p>" 
              ++ s 
              ++ "</p></li></a>"

  
main :: IO ()
main = do
  runCGI $ handleErrors cgiMain