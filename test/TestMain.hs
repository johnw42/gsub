module Main where

import qualified FindReplaceTest
import qualified GsubTest
import qualified PlanTest

import Test.Framework (defaultMain)

main = defaultMain [
  FindReplaceTest.tests,
  GsubTest.tests,
  PlanTest.tests
  ]
