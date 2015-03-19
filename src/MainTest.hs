module MainTest (main) where

import Main hiding (main)

import Control.Monad
import Control.Monad.Random
import Data.Either
import Data.List
import System.IO (stdout)
import Test.QuickCheck
import Test.HUnit

-- My implementation.
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = tail ++ map (x:) tail
    where tail = powerset xs

-- Implementation stolen from the internet.
powerset' :: [a] -> [[a]]
powerset' = filterM (const [False, True])

-- Given two lists and a source of randomness, combine elements from the
-- two lists randomly while preserving the order of elements within each
-- list.
shuffle :: (RandomGen g) => g -> [a] -> [a] -> [a]
shuffle g as bs = evalRand (loop as (length as) bs (length bs)) g where
    loop :: (RandomGen g) => [a] -> Int -> [a] -> Int -> Rand g [a]
    loop as _ [] _ = return as
    loop [] _ bs _ = return bs
    loop (a:as) aLen (b:bs) bLen = do
        r <- getRandomR (1, aLen + bLen)
        if r <= aLen
        then do rest <- loop as (aLen - 1) (b:bs) bLen
                return $ a:rest
        else do rest <- loop (a:as) aLen bs (bLen - 1)
                return $ b:rest

prop_shuffle (Large seed) (Small leftLen) (Small rightLen) =
    lefts == lefts' && rights == rights'
    where
        lefts = [1..leftLen]
        rights = [1..rightLen]
        shuf = shuffle (mkStdGen seed) (map Left lefts) (map Right rights)
        (lefts', rights') = partitionEithers shuf

newtype PosArg = PosArg { getPosArg :: String } deriving Show
newtype ArgList = ArgList [String] deriving Show

instance Arbitrary PosArg where
    arbitrary = do
        s <- arbitrary `suchThat` (not . (elem '-'))
        return $ PosArg s

nonFlagString :: Gen String
nonFlagString = arbitrary `suchThat` (not . ("-" `isPrefixOf`))

-- Type of positional arguments.
newtype PosArgs = PosArgs [String] deriving Show
instance Arbitrary PosArgs where
    arbitrary = do
        (Positive (Small numFiles)) <- arbitrary
        fmap PosArgs $ vectorOf (2 + numFiles) nonFlagString

prop_PosArgs (PosArgs args) = length args >= 3

-- Given a generator for a list of positional arguments, randomly add some valid flags.
withFlags :: Gen [String] -> Gen [String]
withFlags posArgsGen = do
    posArgs <- posArgsGen
    runModeFlag <- elements ["--diff", "-D", "--no-modify", "-N", "-u", "--undo"]
    patternModeFlag <- elements ["-F", "--fixed-strings"]
    backupFileName <- listOf1 arbitrary
    backupFlag <- elements ["-i" ++ backupFileName, "--backup-suffix=" ++ backupFileName]
    flags <- elements $ powerset [runModeFlag, patternModeFlag, backupFlag]
    flags <- elements $ permutations $ flags
    (Large seed) <- arbitrary
    return $ shuffle (mkStdGen seed) flags posArgs

-- Test with too few arguments.
prop_parseArgs_notEnough name =
    forAll (withFlags $ resize 2 $ listOf nonFlagString) $ \args ->
    case parseArgs name args of
        Left error -> counterexample error $ property $ error == name ++ ": not enough arguments\n"
        Right _ -> property False

-- Test with no flags.
prop_parseArgs_noFlags name (PosArgs args@(pattern:replacement:files)) =
    parseArgs name args == Right (defaultPlan pattern replacement files)

-- Test with valie flags.
prop_parseArgs_withFlags name (PosArgs (pattern:replacement:files)) =
    forAll (withFlags $ return $ [pattern, replacement] ++ files) $ \args ->
    case parseArgs name args of
        Right plan -> conjoin
            [ filesToProcess plan == files
            , patternString plan == pattern
            , replacementString plan == replacement
            ]
        Left error -> counterexample error $ property False

parseArgs_tests = "parseArgs" ~: test
    [ parseArgs "gsub" [] ~?= Left "gsub: not enough arguments\n"
    , parseArgs "gsub" ["--xyzzy"] ~?= Left "gsub: unrecognized option `--xyzzy'\n"
    , parseArgs "gsub" ["a", "b"] ~?= Left "gsub: not enough arguments\n"
    , parseArgs "gsub" ["a", "b", "c"] ~?= Right (defaultPlan "a" "b" ["c"])
    , parseArgs "gsub" ["a", "b", "c", "d"] ~?= Right (defaultPlan "a" "b" ["c", "d"])
    ]

firstJust_tests = "firstJust" ~: test
    [ firstJust [] ~?= (Nothing :: Maybe ())
    , firstJust [Just 1] ~?= Just 1
    , firstJust [Just 1, Just 2] ~?= Just 1
    , firstJust [Nothing, Just 1, Just 2] ~?= Just 1
    ]

return []
main = do
    runTestText (putTextToHandle stdout False) $ test
        [ firstJust_tests
        , parseArgs_tests
        ]
    $forAllProperties $ \prop -> do
        result <- quickCheckWithResult stdArgs { chatty = False } prop
        putStr $ output result
        return result