module GsubTest where

import Gsub

import Plan
import OptionsTest ()
import PlanTest ()

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

instance Show Plan where
    show p = "Plan {options = " ++ show (options p) ++ "}"

instance Arbitrary CaseHandling where
    arbitrary = elements [IgnoreCase, ConsiderCase]

prop_transformLineFixed1 ch pattern replacement before after =
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    printTestCase (show rkesult) $
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

prop_transformLine plan before after =
    useFixedStrings plan ==>  -- XXX
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    printTestCase ("transformed: " ++ show result) $
        replacement `isInfixOf` result &&
        not (pattern `isInfixOf` result)
    where pattern = patternString plan
          replacement = replacementString plan
          content = before ++ pattern ++ after
          xfrm = transformation plan
          result = transformLine xfrm content

prop_transformFileContent plan before after =
    useFixedStrings plan ==>  -- XXX
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    printTestCase (show result') $
        replacement `isInfixOf` result' &&
        not (pattern `isInfixOf` result')
    where pattern = patternString plan
          replacement = replacementString plan
          content' = before ++ pattern ++ after
          content = L8.pack content'
          result = transformFileContent plan content
          result' = L8.unpack result

tests = testGroup "Gsub" [
  testProperty "transformLineFixed1" prop_transformLineFixed1,
  testProperty "transformLineFixed2" prop_transformLineFixed2,
    testProperty "transformLine(XXX)" prop_transformLine,
    testProperty "transformFileContent(XXX)" prop_transformFileContent
    ]
