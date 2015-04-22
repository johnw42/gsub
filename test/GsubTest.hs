module GsubTest where

import Gsub

import FindReplace
import Options
import OptionsTest
import Plan
import PlanTest

import Control.Monad
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.Directory
import System.FilePath
import System.Exit
import System.Process
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

instance Show Plan where
    show p = "Plan {options = " ++ show (options p) ++ "}"

prop_transformLine plan before after =
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    replacement `isInfixOf` result' &&
    not (pattern `isInfixOf` result')
  where pattern = patternString plan
        replacement = replacementString plan
        content = before ++ pattern ++ after
        content' = L8.pack content
        xfrm = transformation plan
        result = transformLine xfrm content'
        result' = L8.unpack result

prop_transformFileContent plan before after =
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    replacement `isInfixOf` result' &&
    not (pattern `isInfixOf` result')
  where pattern = patternString plan
        replacement = replacementString plan
        content' = before ++ pattern ++ after
        content = L8.pack content'
        result = transformFileContent plan content
        result' = L8.unpack result

testDataDir = "dist/test_tmp"
testBin = "dist/build/hs-gsub/hs-gsub"

prepareTestFiles = do
    callCommand "cabal build"
    callProcess "rsync" ["-a", "--delete", "test_data/", testDataDir]

case_noArgs = do
    prepareTestFiles
    (exitCode, stdout, stderr) <-
        readProcessWithExitCode testBin [] ""
    assertEqual "bad exit code" (ExitFailure 1) exitCode
    assertEqual "wrong stdout" "" stdout
    assertBool "empty stderr" ("" /= stderr)

case_badFileArgs = do
    prepareTestFiles
    (exitCode, stdout, stderr) <-
        readProcessWithExitCode
        testBin
        ["a", "b", testDataDir, testDataDir </> "no_such_file"]
        ""
    assertEqual "bad exit code" (ExitFailure 2) exitCode
    -- TODO: Output should be on stderr.
    assertEqual "wrong stdout" "" stdout
    assertEqual "wrong stderr" expectedOutput stderr
  where
    expectedOutput = unlines [
        testDataDir ++ ": is a directory",
        testDataDir ++ "/no_such_file: no such file"
        ]

case_badBackref = do
    prepareTestFiles
    (exitCode, stdout, stderr) <-
        readProcessWithExitCode
        testBin
        ["a", "\\1", testDataDir </> "a"]
        ""
    assertEqual "bad exit code" (ExitFailure 1) exitCode
    -- TODO: Output should be on stderr.
    assertEqual "wrong stdout" "" stdout
    assertEqual "wrong stderr" expectedOutput stderr
  where
    expectedOutput = unlines [
        "hs-gsub: pattern has fewer than 1 groups"
        ]

tests = do
    testGroup "Gsub" [
        testProperty "transformLine" prop_transformLine,
        testProperty "transformFileContent" prop_transformFileContent,
        testCase "case_noArgs" case_noArgs,
        testCase "case_badFileArgs" case_badFileArgs,
        testCase "case_badBackref" case_badBackref
        ]
