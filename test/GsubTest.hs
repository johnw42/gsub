module GsubTest where

import Gsub

import Plan
import OptionsTest ()
import PlanTest ()

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

instance Show Plan where
    show p = "Plan {options = " ++ show (options p) ++ "}"

case_matchRangesToGroups =
    matchRangesToGroups ranges "abcdef" @?= groupMatches
  where
    ranges = [
        ((0,2), [(0,1), (0,2)]),
        ((3,5), [(4,5), (3,3)])
        ]
    groupMatches = [
        [GroupMatch 0 2 "ab",
         GroupMatch 0 1 "a",
         GroupMatch 0 2 "ab"
        ],
        [GroupMatch 3 5 "de",
         GroupMatch 4 5 "e",
         GroupMatch 3 3 ""]
        ]

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
    testCase "matchRangesToGroups" case_matchRangesToGroups,
    testProperty "transformLine(XXX)" prop_transformLine,
    testProperty "transformFileContent(XXX)" prop_transformFileContent
    ]
