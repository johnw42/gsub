module Plan where

import FindReplace
import Options
import Types
import Utils

import           Control.Applicative ((<$>))
import           Control.Exception (assert)
import           Control.Monad (liftM, liftM2, msum, when)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import           System.FilePath ((</>))
import           System.Posix.Files
import           Text.Printf (printf)

import qualified Text.Regex.PCRE.Light as RE

data Transformation
    = TransformRegex RE.Regex Replacement
    | TransformFixed CaseHandling String String

-- An execution plan.
data Plan
    = Plan
      { options            :: Options
      , transformation     :: Transformation
      , contextPatterns    :: [RE.Regex]
      , notContextPatterns :: [RE.Regex]
      , patchFilePath      :: Maybe FilePath
      }

-- A facade for options.
patternString = toReplaceStringOpt . options
replacementString = replacementStringOpt . options
filesToProcess = filesOpt . options
planMode = planModeOpt . options
backupSuffix = backupSuffixOpt . options
replacementMode = replacementModeOpt . options
keepGoing = keepGoingOpt . options
caseHandling = caseHandlingOpt . options
contextStrings = contextStringsOpt . options
notContextStrings = notContextStringsOpt . options
contextBefore = contextBeforeOpt . options
contextAfter = contextAfterOpt . options

makePlan :: Options -> IO Plan
makePlan opts = do
    patchPath <-
        case planModeOpt opts of
        DiffMode -> return Nothing
        _        -> Just <$> findPatchPath (patchFilePathOpt opts)
    case makePlan' opts patchPath of
        Left e     -> error e
        Right plan -> return plan

-- | Converts options into an execution plan.
makePlan' :: Options -> Maybe FilePath -> Either String Plan
makePlan' opts patchPath = do
    xfrm <- case replacementModeOpt opts of
        FixedMode -> Right fixed
        RegexMode -> checkRegexM compileRegexM regexReplacementM
    ctx <- mapM (compileRegex caseHandling) (contextStringsOpt opts)
    ctx' <- mapM (compileRegex caseHandling) (notContextStringsOpt opts)
    return $ Plan opts xfrm ctx ctx' patchPath
  where
    fixed =
        TransformFixed
        (caseHandlingOpt opts)
        fixedPattern
        fixedReplacement
    fixedPattern = toReplaceStringOpt opts
    fixedReplacement = replacementStringOpt opts
    regexReplacementM = parseReplacement (replacementStringOpt opts)
    caseHandling = caseHandlingOpt opts
    compileRegexM =
        compileRegex caseHandling (toReplaceStringOpt opts)

-- | Check that a regex and replacement string are compatible.
checkRegexM :: Either String RE.Regex
            -> Either String Replacement
            -> Either String Transformation
checkRegexM reM repM = do
    re <- reM
    rep <- repM
    let maxGroup = repMaxGroup rep
    if maxGroup < RE.captureCount re
        then Right (TransformRegex re rep)
        else Left ("pattern has fewer than " ++
                   show maxGroup ++ " groups")

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
        when (p /= "/dev/null") $ do
            exists <- fileExist p
            when exists $
                error ("Patch file already exists: " ++ p)
        return p
    1 -> loop (expandDiffPattern p)
    _ -> error ("Invalid patch file pattern: " ++ p)
  where
    loop (p:ps) = do
        exists <- fileExist p
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
