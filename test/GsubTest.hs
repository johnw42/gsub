module GsubTest where

import Gsub

import FindReplace
import Options
import OptionsTest
import Plan
import PlanTest

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import qualified Text.Regex.PCRE.Heavy as Heavy

instance Show Plan where
    show p = "Plan {options = " ++ show (options p) ++ "}"

instance Arbitrary CaseHandling where
    arbitrary = elements [IgnoreCase, ConsiderCase]

prop_transformLineFixed1 ch pattern replacement before after =
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    printTestCase (show result) $
        replacement `isInfixOf` result &&
        not (pattern `isInfixOf` result)
  where
    content = before ++ pattern ++ after
    result = transformLineFixed ch pattern replacement content

prop_transformLineFixed2 pattern replacement before after =
    not (u pattern `isInfixOf` (u $ replacement ++ after)) ==>
    not (u pattern `isInfixOf` (u $ before ++ replacement)) ==>
    printTestCase (show result) $
        replacement `isInfixOf` result &&
        not (pattern `isInfixOf` result)
  where
    content = before ++ pattern ++ after
    u = map toUpper
    result = transformLineFixed IgnoreCase (u pattern) replacement content

prop_transformLineRegex
    :: AlphaString
    -> AlphaString
    -> String
    -> String
    -> Property
prop_transformLineRegex (Alpha patStr) (Alpha repStr) before after =
    not (patStr `isInfixOf` (repStr ++ after)) ==>
    not (patStr `isInfixOf` (before ++ repStr)) ==>
    printTestCase (show result) $
    repStr `isInfixOf` result &&
    not (patStr `isInfixOf` result)
  where
    content = before ++ patStr ++ after
    result = transformLineRegex regex rep content
    Right regex = Heavy.compileM (B8.pack patStr) []
    rep = literalReplacement repStr

prop_transformLine plan before after =
    planMode plan == RunMode ==>
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    printTestCase ("pat: " ++ show pattern) $
    printTestCase ("rep: " ++ show replacement) $
    printTestCase ("content: " ++ show content) $
    printTestCase ("transformed: " ++ show result) $
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
    printTestCase ("transformed: " ++ show result') $
    replacement `isInfixOf` result' &&
    not (pattern `isInfixOf` result')
  where pattern = patternString plan
        replacement = replacementString plan
        content' = before ++ pattern ++ after
        content = L8.pack content'
        result = transformFileContent plan content
        result' = L8.unpack result

case_xxx =
    Heavy.gsub regex "b" "\253a\158" @?= "\253b\158"
  where
    Right regex = Heavy.compileM (B8.pack "a") []

tests = testGroup "Gsub" [
    testCase "case_xxx" case_xxx,
    testProperty "transformLineFixed1" prop_transformLineFixed1,
    testProperty "transformLineFixed2" prop_transformLineFixed2,
    testProperty "transformLineRegex" prop_transformLineRegex,
    testProperty "transformLine" prop_transformLine,
    testProperty "transformFileContent" prop_transformFileContent
    ]
