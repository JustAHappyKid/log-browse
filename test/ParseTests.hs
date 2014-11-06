module Main where

import Test.HUnit
import LogBrowse.Parse (lineToParts)

testParsingLogLine = TestCase $ do
  assertEqual "simple log message containing date/time and severity"
    ["2012-03-19 10:59", "info", "The meat of the message"]
    (lineToParts "[2012-03-19 10:59] [info] The meat of the message")
  assertEqual "log-line containing square brackets in the payload of the message"
    ["2012-03-20 12:35", "notice", "POSTing the following for contact[name]: 'fred'"]
    (lineToParts "[2012-03-20 12:35] [notice] POSTing the following for contact[name]: 'fred'")

tests = TestList
  [TestLabel "test breaking log-line into its component parts works properly" testParsingLogLine]

main = runTestTT tests
