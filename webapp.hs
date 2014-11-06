module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.List (intercalate, isInfixOf)
import Data.String.Utils (replace, split, strip)
import qualified LogBrowse.Parse as Parse
import Network.CGI (CGI, CGIResult, runCGI, handleErrors, liftIO, output, requestMethod, requestURI,
                    -- setStatus, redirect,
                    getInput)
import System.Environment (getEnv)
import Text.XHtml.Strict ((<<), (+++), (!))
import qualified Text.XHtml.Strict as H
import qualified Text.XHtml.Table as HT

cgiMain :: CGI CGIResult
cgiMain = do
  uri <- requestURI
  --let path = pathList uri
  method <- requestMethod
  --inputVars <- getInputs
  logDir <- liftIO $ getEnv "LOG_DIR"
  maybeFname <- getInput "f"
  (title, mainContent) <- case maybeFname of
    Just f -> do
      linesOfFile <- liftIO $ readFileAsUTF8 $ pathJoin [logDir, f]
      let nonBlankLines = filter (\l -> strip l /= "") linesOfFile
      contains <- getInput "contains"
      let lns = filter (lineContains contains) nonBlankLines
      return (f, renderLogLines lns)
    Nothing -> return $ ("No log file", H.paragraph << "No log file specified!")
  outputHtml $ (standardHeader title) +++ (H.body << mainContent)
  where
    lineContains contains ln = case contains of
      Nothing -> True
      Just s -> s `isInfixOf` ln

readFileAsUTF8 :: String -> IO [String]
readFileAsUTF8 path = do
  content <- BS.readFile path
  return $ map UTF8.toString (BS.split 10 content)

renderLogLines :: [String] -> H.Html
renderLogLines lns =
  case lns of
    [] -> H.paragraph << "No lines matched the given parameters!"
    _  -> HT.simpleTable [] [] rows
    where rows = map toRow lns
          toRow ln = map H.stringToHtml $ Parse.lineToParts ln

--page f bod = (standardHeader f) +++ (H.body << bod)

standardHeader logFile = H.header << (
  (H.thetitle << (logFile ++ " - log-browse")) +++
  --(H.thelink ! [H.rel "stylesheet", H.thetype "text/css", H.href "/style.css"] << "")
  (H.style ! [H.thetype "text/css"] << H.stringToHtml css))

css = unlines [
  "body { background-color: #ddd; }",
  "table {",
  "  font-family: monospace;",
  "}",
  "table tr td {",
  "  padding: 0.4em;",
  "}",
  "table tr:nth-child(odd) { background-color: #fafafa; }",
  "table tr:nth-child(even) { background-color: #efefef; }",
  "table tr td:first-of-type { whitespace: nowrap; }"]

pathJoin parts = intercalate "/" parts

outputHtml = output . H.renderHtml

main = runCGI $ handleErrors cgiMain

{-
permanentRedirect path = do
  setStatus 301 "Moved Permanently"
  redirect $ makeFullUrl path

noSuchResource = do4xxResponse 404 "Not Found"
resourceRemoved = do4xxResponse 410 "Gone"
do4xxResponse statusCode statusMsg = do
  uri <- requestURI
  method <- requestMethod
  inputVars <- getInputs
  envVars <- getCGIVars
  setStatus statusCode statusMsg
  outputHtml UI.pageNotFound

showKeyValueList :: [(String, String)] -> String
showKeyValueList l = intercalate "\n" $ map (\(k, v) -> k ++ ": " ++ v) l

indent s = unlines $ map ("  " ++) (lines s)
-}
