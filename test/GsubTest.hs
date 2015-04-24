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
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
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
built = unsafePerformIO (newIORef False)

testFile = (testDataDir </>)

writeTestFile path = writeFile (testFile path)

assertTestFile path content = do
    content' <- readFile (testFile path)
    assertEqual ("wrong content for " ++ path) content content'

run args = do
    readProcessWithExitCode testBin args ""

setUp = do
    built' <- readIORef built
    unless built' $ do
        callCommand "cabal build"
        writeIORef built True
    callCommand "rm -rf test_data"
    callCommand "mkdir test_data"

expectStdout :: [String] -> [String] -> IO ()
expectStdout args stdoutLines = do
    (exitCode', stdout', stderr') <- run args
    assertEqual "output on stderr" "" stderr'
    assertEqual "wrong stdout" stdoutLines (lines stdout')
    assertEqual "wrong exit code" ExitSuccess exitCode'

expectStderr :: [String] -> Int -> [String] -> IO ()
expectStderr args exitCode stderrLines = do
    (exitCode', stdout', stderr') <- run args
    assertEqual "wrong stderr" stderrLines (lines stderr')
    assertEqual "output on stdout" "" stdout'
    assertEqual "wrong exit code" (ExitFailure exitCode) exitCode'

case_noArgs = do
    setUp
    (exitCode, stdout, stderr) <- run []
    assertEqual "bad exit code" (ExitFailure 1) exitCode
    assertEqual "wrong stdout" "" stdout
    assertBool "empty stderr" ("" /= stderr)

case_badFileArgs = do
    setUp
    expectStderr
        ["a", "b", file1, file2]
        2
        [ file1 ++ ": is a directory"
        , file2 ++ ": no such file"
        ]
  where file1 = testDataDir
        file2 = testFile "no_such_file"

case_badBackref = do
    setUp
    expectStderr
        ["a", "\\1", testFile "a"]
        1
        ["hs-gsub: pattern has fewer than 1 groups"]

case_simpleReplace = do
    setUp
    writeTestFile "a" "foo\n"
    expectStdout
        ["foo", "bar", testFile "a"]
        [testFile "a"]
    assertTestFile "a" "bar\n"

case_regexReplace = do
    setUp
    writeTestFile "a" "foo\n"
    expectStdout
        ["o+", "x", testFile "a"]
        [testFile "a"]
    assertTestFile "a" "fx\n"

case_groupReplace = do
    setUp
    writeTestFile "a" "ax by cz\n"
    expectStdout
        ["([a-c])([x-z])", "\\0:\\2\\1", testFile "a"]
        [testFile "a"]
    assertTestFile "a" "ax:xa by:yb cz:zc\n"

case_simpleDiff = do
    setUp
    writeTestFile "a" "a\nb\nc\n"
    expectStdout
        ["--diff", "b", "y", testFile "a"]
        [ "--- dist/test_tmp/a"
        , "+++ dist/test_tmp/a"
        , "@@ -1,3 +1,3 @@"
        , " a"
        , "-b"
        , "+y"
        , " c"]
    assertTestFile "a" "a\nb\nc\n"

case_simpleDryRun = do
    setUp
    writeTestFile "a" "foo\n"
    expectStdout
        ["--no-modify", "foo", "bar", testFile "a"]
        [testFile "a"]
    assertTestFile "a" "foo\n"

case_simpleUndo = do
    setUp
    writeTestFile "z" "foo\n"
    expectStdout args [testFile "z"]
    assertTestFile "z" "bar\n"
    expectStdout ("--undo" : args) ["patching file " ++ testFile "z"]
    assertTestFile "z" "foo\n"
  where
    args = ["foo", "bar", testFile "z"]

tests =
    testGroup "Gsub"
    [ testProperty "transformLine" prop_transformLine
    , testProperty "transformFileContent" prop_transformFileContent
    , testCase "case_noArgs" case_noArgs
    , testCase "case_badFileArgs" case_badFileArgs
    , testCase "case_badBackref" case_badBackref
    , testCase "case_simpleReplace" case_simpleReplace
    , testCase "case_regexReplace" case_regexReplace
    , testCase "case_groupReplace" case_groupReplace
    , testCase "case_simpleDiff" case_simpleDiff
    , testCase "case_simpleDryRun" case_simpleDryRun
    , testCase "case_simpleUndo" case_simpleUndo
    ]
