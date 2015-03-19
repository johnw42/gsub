module MainTest (main) where

import Main hiding (main)

import Control.Monad
import Control.Monad.Random
import Data.Either
import Data.List
import Data.Maybe
import System.IO (stdout)
import Test.QuickCheck

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
shuffleMerge :: (RandomGen g) => g -> [a] -> [a] -> [a]
shuffleMerge g as bs = evalRand (loop as (length as) bs (length bs)) g where
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

prop_shuffleMerge (Large seed) (Small leftLen) (Small rightLen) =
    lefts == lefts' && rights == rights'
    where
        lefts = [1..leftLen]
        rights = [1..rightLen]
        shuf = shuffleMerge (mkStdGen seed) (map Left lefts) (map Right rights)
        (lefts', rights') = partitionEithers shuf

newtype PosArg = PosArg { getPosArg :: String } deriving Show
newtype ArgList = ArgList [String] deriving Show

instance Arbitrary PosArg where
    arbitrary = do
        s <- arbitrary `suchThat` notElem '-'
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

-- Type of command-line argument lists.
newtype Arguments = Arguments [String] deriving Show
instance Arbitrary Arguments where
    arbitrary = do
       (PosArgs posArgs) <- arbitrary
       fmap Arguments $ withFlags posArgs

modeFlags = ["--diff", "-D", "--no-modify", "-N", "-u", "--undo"]

-- A generator for random valid flags.
flagsGen :: Gen [String]
flagsGen = do
    runModeFlag <- elements modeFlags
    patternModeFlag <- elements ["-F", "--fixed-strings"]
    backupFileName <- listOf1 arbitrary
    backupFlag <- elements ["-i" ++ backupFileName, "--backup-suffix=" ++ backupFileName]
    flagSets <- shuffle $ powerset [runModeFlag, patternModeFlag, backupFlag]
    flags <- elements flagSets
    shuffle flags

-- Given a generator for a list of positional arguments, randomly add some valid flags.
withFlags :: [String] -> Gen [String]
withFlags posArgs = do
    flagArgs <- flagsGen
    (Large seed) <- arbitrary
    return $ shuffleMerge (mkStdGen seed) flagArgs posArgs

prop_withFlags (PosArgs posArgs) =
    forAll (withFlags posArgs) $ \allArgs ->
    forAll (elements posArgs) $ \posArg ->
    posArg `elem` allArgs

-- Test with too few arguments.
prop_parseArgs_notEnough name =
    forAll (resize 2 $ listOf nonFlagString) $ \posArgs ->
    forAll (withFlags posArgs) $ \args ->
    case parseArgs name args of
        Left error -> counterexample error $ property $ error == name ++ ": not enough arguments\n"
        Right _ -> property False

-- Test with no flags.
prop_parseArgs_noFlags name (PosArgs args@(pattern:replacement:files)) =
    parseArgs name args == Right (defaultPlan pattern replacement files)

-- Test with valid flags.
prop_parseArgs_withFlags name (PosArgs (pattern:replacement:files)) =
    forAll (withFlags $ [pattern, replacement] ++ files) $ \args ->
    case parseArgs name args of
        Right plan -> conjoin
            [ filesToProcess plan == files
            , patternString plan == pattern
            , replacementString plan == replacement
            ]
        Left error -> counterexample error $ property False

prop_parseArgs_withDiff name (Arguments args) =
    ("-D" `elem` args || "--diff" `elem` args) ==> case parseArgs name args of
        Left _ -> True
        Right plan -> planMode plan == DiffMode

prop_parseArgs_withDryRun name (Arguments args) =
    ("-N" `elem` args || "--no-modify" `elem` args) ==> case parseArgs name args of
        Left _ -> True
        Right plan -> planMode plan == DryRunMode

prop_parseArgs_withUndo name (Arguments args) =
    ("-u" `elem` args || "--undo" `elem` args) ==> case parseArgs name args of
        Left _ -> True
        Right plan -> planMode plan == UndoMode

prop_parseArgs_withDefaultMode name (Arguments args) =
    not (any (`elem` modeFlags) args) ==> case parseArgs name args of
        Left _ -> False
        Right plan -> planMode plan == RunMode

-- Test that firstJust works.
prop_firstJust_empty = once $ isNothing $ firstJust []
prop_firstJust_allNothing (Positive (Small n)) =
    isNothing $ firstJust $ replicate n Nothing
prop_firstJust_typical (NonEmpty items) =
    case firstJust items of
        Nothing -> all isNothing items
        Just x -> Just x == head (dropWhile isNothing items)

return []
main =
    $forAllProperties $ \prop -> do
        result <- quickCheckWithResult stdArgs { chatty = False, maxDiscardRatio = 10 } prop
        putStr $ output result
        return result