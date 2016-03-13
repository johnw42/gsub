module Plan where

import Directory
import FindReplace
import Options
import Utils

import Control.Applicative ((<$>))
import Control.Exception (assert)
import Control.Monad (liftM, liftM2, when)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B8
import System.FilePath ((</>))
import Text.Printf (printf)

import qualified Text.Regex.PCRE.Heavy as Heavy
import qualified Text.Regex.PCRE.Light as Light

data Transformation
    = TransformRegex Light.Regex Replacement
    | TransformFixed CaseHandling String String

-- An execution plan.
data Plan
    = Plan
      { options :: Options
      , transformation :: Transformation
      , patchFilePath :: Maybe FilePath
      }

-- A facade for options.
patternString = patternStringOpt . options
replacementString = replacementStringOpt . options
filesToProcess = filesOpt . options
planMode = planModeOpt . options
backupSuffix = backupSuffixOpt . options
useFixedStrings = fixedStringsOpt . options
keepGoing = keepGoingOpt . options
ignoreCase = ignoreCaseOpt . options

makePlan :: Options -> IO Plan
makePlan opts = do
    patchPath <-
        case planModeOpt opts of
        DiffMode -> return Nothing
        _ -> Just <$> findPatchPath (patchFilePathOpt opts)
    case makePlan' opts patchPath of
        Left e -> error e
        Right plan -> return plan

-- | Converts options into an execution plan.
makePlan' :: Options -> Maybe FilePath -> Either String Plan
makePlan' opts patchPath = do
    xfrm <- if fixedStringsOpt opts
            then Right fixed
            else checkRegex compileRegexM regexReplacementM
    return (Plan opts xfrm patchPath)
  where
    fixed = TransformFixed caseHandling fixedPattern fixedReplacement
    checkRegex reM repM = do
        re <- reM
        rep <- repM
        let maxGroup = repMaxGroup rep
        if maxGroup < Light.captureCount re
            then Right (TransformRegex re rep)
            else Left ("pattern has fewer than " ++
                       show maxGroup ++ " groups")
    caseHandling = if ignoreCaseOpt opts
                   then IgnoreCase
                   else ConsiderCase
    fixedPattern = patternStringOpt opts
    fixedReplacement = replacementStringOpt opts
    regexReplacementM = parseReplacement (replacementStringOpt opts)
    pattern = "(" ++ patternStringOpt opts ++ ")"
    compileRegexM = compileRegex (ignoreCaseOpt opts) pattern

-- Count the number of '#' characters in a file path.
countPatternSlots :: FilePath -> Int
countPatternSlots p = length $ L.elemIndices '#' p

-- Find a nonexistant file matching the given pattern, which consists
-- of a file path with zero or one '#' charcters.  If the pattern
-- contains a '#' character, it may be replaces with any integer to
-- produce a nonexistant file name.
findPatchPath :: FilePath -> IO FilePath
findPatchPath p = case countPatternSlots p of
    0 -> do
        exists <- doesPathExist p
        when exists $
            error ("Patch file already exists: " ++ p)
        return p
    1 -> loop (expandDiffPattern p)
    _ -> error ("Invalid patch file pattern: " ++ p)
  where
    loop (p:ps) = do
        exists <- doesPathExist p
        if not exists
            then return p
            else loop ps
        
-- Produce an infinite list of candidate file paths corresponding to a
-- pattern containing a single '#' character.
expandDiffPattern :: FilePath -> [FilePath]
expandDiffPattern p =
    assert (countPatternSlots p == 1) $
    [prefix ++ show i ++ suffix | i <- [0..]]
  where
    (prefix, _:suffix) = break (=='#') p
