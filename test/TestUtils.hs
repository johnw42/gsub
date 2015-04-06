module TestUtils where

import Test.QuickCheck

-- Equality check for use in tests.
infix 5 ==?
(==?) :: (Eq a, Show a) => a -> a -> Property
a ==? b =
    printTestCase ("expecting " ++ show b ++ ", got " ++ show a) (a == b)

-- Apply and check for equality.
apEq :: (Show a, Show b, Eq b) => (a -> b) -> a -> b -> Property
apEq f a b =
    printTestCase
        ("Expecting " ++ show a ++ " -> " ++ show b ++ ", got -> " ++ show fa)
        (fa == b)
    where fa = f a
