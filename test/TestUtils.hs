module TestUtils where

import Control.Applicative
import Test.QuickCheck

-- Type of strings of letters.
newtype AlphaString = Alpha { fromAlpha :: String } deriving Show
type RegexPattern = AlphaString
type ReplacementPattern = AlphaString

instance Arbitrary AlphaString where
    arbitrary = Alpha <$> listOf1 (elements ['a'..'z'])

-- Equality check for use in tests.
infix 5 ==?
(==?) :: (Eq a, Show a) => a -> a -> Property
a ==? b =
    counterexample ("expecting " ++ show b ++ ", got " ++ show a) (a == b)
