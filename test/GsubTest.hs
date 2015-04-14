module GsubTest where

import Gsub

import Plan
import PlanTest ()

import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

prop_transformFileContent (Blind plan) before after =
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
        replacement `isInfixOf` result && not (pattern `isInfixOf` result)
    where pattern = patternString plan
          replacement = replacementString plan
          content = before ++ pattern ++ after
          Right result = transformFileContent plan content

tests = testGroup "Gsub" [
  testProperty "transformFileContent" prop_transformFileContent
  ]
