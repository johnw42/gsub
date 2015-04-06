module Main where

import TestUtils
import FindReplaceTest
import GsubTest
import PlanTest

import Test.Framework (defaultMain)

main = defaultMain [
  FindReplaceTest.tests,
  GsubTest.tests,
  PlanTest.tests
  ]
