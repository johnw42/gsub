module OptionsTest where

import Options
import TestUtils
import Types

import Control.Applicative
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import System.IO (stdout)
import System.Random
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Abitrary option sets.
instance Arbitrary Options where
    arbitrary = Options
        <$> fmap fromAlpha arbitrary
        <*> fmap fromAlpha arbitrary
        <*> arbitrary -- filesOpt
        <*> elements [RunMode, DryRunMode, DiffMode]
        <*> arbitrary -- backupSuffixOpt
        <*> elements [FixedMode, RegexMode]
        <*> arbitrary -- patchFilePathOpt
        <*> arbitrary -- keepGoingOpt
        <*> elements [ConsiderCase, IgnoreCase]
        <*> listOf (fmap fromAlpha arbitrary)
        <*> fmap getSmall arbitrary
        <*> fmap getSmall arbitrary
    shrink opts = do
        files <- shrink (filesOpt opts)
        bs <- shrink (backupSuffixOpt opts)
        pfp <- shrink (patchFilePathOpt opts)
        ps <- shrink (toReplaceStringOpt opts)
        rs <- shrink (replacementStringOpt opts)
        return $ opts
            { filesOpt = files
            , toReplaceStringOpt = ps
            , replacementStringOpt = rs
            , backupSuffixOpt = bs
            , patchFilePathOpt = pfp
            }

type FlagPart = String
type PosArg = String

-- | Generator for arbitrary positional arguments.
arbPosArg :: Gen PosArg
arbPosArg = arbitrary `suchThat` (not . ("-" `isPrefixOf`))

-- | Generator for an arbitrary list of positional arguments.
arbPosArgList :: Gen [PosArg]
arbPosArgList = liftM2 (++) (vectorOf 3 arbPosArg) (listOf arbPosArg)

modeFlags = ["--diff", "-D", "--no-modify", "-N", "-u", "--undo"]
otherFlags = ["-F", "--fixed-strings", "-i", "--ignore-case"]
shortFlagsWithArg = ["-b"]
longFlagsWithArg = ["--backup-suffix"]

-- Generator for arbitrary flags.
arbFlag :: Gen [FlagPart]
arbFlag = do
    modeFlag <- elements modeFlags
    otherFlag <- elements otherFlags
    someChar <- arbitrary
    flagArg <- arbitrary
    shortFlag <- elements shortFlagsWithArg
    longFlag <- elements longFlagsWithArg
    frequency
        [ (2, return [modeFlag])
        , (2, return [otherFlag])
        , (1, return [shortFlag ++ (someChar:flagArg)])
        , (1, return [shortFlag, flagArg])
        , (1, return [longFlag ++ "=" ++ flagArg])
        , (1, return [longFlag, flagArg])
        ]

-- Abitrary flags have one or two parts.
prop_arbFlag_length =
    forAll arbFlag (\flag -> length flag `elem` [1..2])

-- The first part of an arbitrary flag starts with a hypen.
prop_arbFlag_dash =
    forAll arbFlag (\flag -> "-" `isPrefixOf` head flag)

-- Arbitrary list of flags to apply at one time.
arbFlagList :: Gen [[FlagPart]]
arbFlagList = resize 3 (listOf arbFlag)

-- Randomly insert flags into a list of positional arguments.
withFlags :: [PosArg] -> [[FlagPart]] -> Gen [String]
withFlags posArgs flags = concat `liftM` foldM insertFlag initSegments flags
    where
        initSegments = map (:[]) posArgs

        insertFlag :: [[String]] -> [FlagPart] -> Gen [[String]]
        insertFlag segments flag = do
            i <- choose (0, length segments)
            let (before, after) = splitAt i segments
            return (before ++ [flag] ++ after)


-- Arbitrary complete argument list.
arbFullArgList :: Gen [String]
arbFullArgList = do
     (FullArgList _ _ args) <- arbFullArgList'
     return args

-- An argument list in two forms: separate flags and conditional arguments, and
-- a combined list.
data FullArgList = FullArgList
    [String]    -- Positional arguments.
    [[String]]  -- Flags.
    [String]    -- Combined argument list.
    deriving Show

-- Arbitrary complete argument list with separate flags and positional args.
arbFullArgList' :: Gen FullArgList
arbFullArgList' = do
    posArgs <- arbPosArgList
    flags <- arbFlagList
    args <- posArgs `withFlags` flags
    return (FullArgList posArgs flags args)

-- Test with too few arguments.
prop_parseArgs_notEnough name =
  forAll (resize 2 (listOf arbPosArg)) $ \posArgs ->
  forAll arbFlagList $ \flags ->
  forAll (posArgs `withFlags` flags) $ \args ->
  conjoin
  [ isLeft (parseArgs name posArgs)
  , isLeft (parseArgs name args)
  ]

-- Test with no flags.
prop_parseArgs_noFlags name =
    forAll arbPosArgList $ \(args@(p:r:fs)) ->
    case parseArgs name args of
        Right opts -> conjoin
            [ filesOpt opts == fs
            , toReplaceStringOpt opts == p
            , replacementStringOpt opts == r
            ]
        Left _ -> property False

-- Test with valid flags.
prop_parseArgs_withFlags name =
  forAll arbFullArgList' $ \(FullArgList (p:r:fs) flags args) ->
  case parseArgs name args
  of Right opts -> conjoin
                   [ filesOpt opts ==? fs
                   , toReplaceStringOpt opts ==? p
                   , replacementStringOpt opts ==? r
                   ]
     Left _ -> discard

-- Check that --diff is parsed correctly.
prop_parseArgs_withDiff name  =
    forAll arbFullArgList $ \args ->
    ("-D" `elem` args || "--diff" `elem` args) ==> case parseArgs name args of
        Left _     -> discard
        Right opts -> planModeOpt opts == DiffMode

-- Test that the dry-run flag is parsed correctly.
prop_parseArgs_withDryRun name =
    forAll arbFullArgList $ \args ->
    ("-N" `elem` args || "--no-modify" `elem` args) ==> case parseArgs name args of
        Left _     -> discard
        Right opts -> planModeOpt opts == DryRunMode

-- Test that the default mode is correct.
prop_parseArgs_withDefaultMode name =
    forAll arbFullArgList $ \args ->
    not (any (`elem` modeFlags) args) ==> case parseArgs name args of
        Left _     -> discard
        Right opts -> planModeOpt opts == RunMode

tests =
    testGroup "Options"
    [ testProperty "arbFlag_length" prop_arbFlag_length
    , testProperty "arbFlag_dash" prop_arbFlag_dash
    , testProperty "parseArgs_notEnough" prop_parseArgs_notEnough
    , testProperty "parseArgs_noFlags" prop_parseArgs_noFlags
    , testProperty "parseArgs_withFlags" prop_parseArgs_withFlags
    , testProperty "parseArgs_withDiff" prop_parseArgs_withDiff
    , testProperty "parseArgs_withDryRun" prop_parseArgs_withDryRun
    , testProperty "parseArgs_withDefaultMode" prop_parseArgs_withDefaultMode
    ]
