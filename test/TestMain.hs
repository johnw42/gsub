module Main where

import qualified FindReplaceTest
import qualified GsubTest
import qualified OptionsTest
import qualified PlanTest

import Test.Framework (defaultMain)

main = defaultMain
       [ FindReplaceTest.tests
       , OptionsTest.tests
       , PlanTest.tests
       ,  GsubTest.tests
       ]
