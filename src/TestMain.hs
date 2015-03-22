module TestMain where

import TestUtils
import qualified FindReplace
import qualified MainTest

main = do
    MainTest.test
    FindReplace.test
