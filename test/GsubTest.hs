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
          result = transformFileContent plan content

-- Test that firstJust works.
case_firstJust_empty = firstJust ([] :: [Maybe Int]) @?= Nothing
prop_firstJust_allNothing  =
  forAll (choose (1, 100)) $ \n ->
    isNothing $ firstJust $ replicate n Nothing
prop_firstJust_typical (NonEmpty items) =
    case firstJust (items :: [Maybe Int])
    of Nothing -> all isNothing items
       Just x -> Just x == head (dropWhile isNothing items)

tests = testGroup "Gsub" [
  testProperty "transformFileContent" prop_transformFileContent,
  testCase "firstJust_empty" case_firstJust_empty,
  testProperty "firstJust_allNothing" prop_firstJust_allNothing,
  testProperty "firstJust_typical" prop_firstJust_typical
  ]
