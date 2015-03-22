module TestMain where

import TestUtils
import qualified FindReplace
import qualified MainTest2

main = do
    MainTest2.test
    FindReplace.test