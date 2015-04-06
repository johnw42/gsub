module TestUtils
    ( quickCheckResult
    , quickCheckWithResult
    , stdArgs
    , (==?)
    , apEq
    , module Test.QuickCheck
    ) where

import Test.QuickCheck hiding
    ( quickCheckResult
    , quickCheckWithResult
    , stdArgs
    , apEq
    )
--Args(..), Property, Result, output)
import Control.Applicative ((<$>), (<*>), Applicative(..))
import Control.Monad (ap, liftM, liftM2)
import qualified Test.QuickCheck as QC

stdArgs :: Args
stdArgs = QC.stdArgs { chatty = False, maxDiscardRatio = 20 }

quickCheckResult :: Property -> IO Result
quickCheckResult = quickCheckWithResult stdArgs

quickCheckWithResult :: Args -> Property -> IO Result
quickCheckWithResult args prop = do
    result <- QC.quickCheckWithResult args prop
    putStr $ output result
    return result

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

-- Crazy experimental stuff:

-- Property combinator.
type PropComb = Property -> Property

data PropValue a = PropValue PropComb a

instance Functor PropValue where
    fmap = liftM

instance Applicative PropValue where
    pure = return
    (<*>) = ap

instance Monad PropValue where
    return = PropValue id
    (PropValue g a) >>= f = let (PropValue g' a') = f a
                            in PropValue (g . g') a'

(.==.) :: (Eq a) => PropValue a -> PropValue a -> Property
(PropValue _ a) .==. (PropValue _ b) = property $ a == b

(=?) :: (Eq a, Show a) => PropValue a -> PropValue a -> Property
a =? b = annotate collect a .==. annotate collect b

annotate :: (a -> PropComb) -> PropValue a -> PropValue a
annotate g (PropValue f a) = PropValue (g a . f) a
