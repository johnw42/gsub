module PlanTest (tests) where

import Plan
import OptionsTest ()

import Control.Monad (liftM)
import qualified Data.ByteString as B
import Data.Char (isHexDigit)
import Data.Either (isRight)
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Plan where
    arbitrary = do
        path <- arbitrary
        opts <- arbitrary
        Right plan <- return (makePlan' opts path) `suchThat` isRight
        return plan
    shrink plan = do
        opts <- shrink $ options plan
        path <- shrink $ patchFilePath plan
        Right plan <- return (makePlan' opts path)
        return plan

prop_toHexString1 s = length (toHexString (B.pack s)) == 2 * length s
prop_toHexString2 s = all isHexDigit (toHexString (B.pack s))

tests = testGroup "Plan" [
  testProperty "toHexString1" prop_toHexString1,
  testProperty "toHexString2" prop_toHexString2
  ]
