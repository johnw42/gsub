module GsubTest where

import Gsub

import Plan
import PlanTest hiding (PosArg)
import Utils

import TestUtils

import Control.Monad
import Data.Either
import Data.List as L
import Data.Maybe
import System.IO (stdout)
import System.Random
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

prop_transformFileContent :: Plan -> String -> String -> Property
prop_transformFileContent plan before after =
    within 100000 $
    not (pattern `L.isInfixOf` (replacement ++ after)) ==>
    not (pattern `L.isInfixOf` (before ++ replacement)) ==>
        replacement `L.isInfixOf` result && not (pattern `L.isInfixOf` result)
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
