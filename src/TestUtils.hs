module TestUtils
    ( quickCheckProp
    , module Test.QuickCheck
    ) where

import Test.QuickCheck

quickCheckProp :: Property -> IO Result
quickCheckProp prop = do
    result <- quickCheckWithResult stdArgs { chatty = False, maxDiscardRatio = 10 } prop
    putStr $ output result
    return result