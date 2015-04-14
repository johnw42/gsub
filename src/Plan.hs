module Plan where

import FindReplace
import Options
import Utils

import Control.Monad (liftM)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

import qualified Text.Regex.PCRE.Heavy as Heavy
import qualified Text.Regex.PCRE.Light as Light

data CaseHandling = IgnoreCase | ConsiderCase

data Transformation
    = TransformRegex Light.Regex Replacement
    | TransformFixed CaseHandling String String

-- An execution plan.
data Plan = Plan
    { options :: Options
    , transformation :: Transformation
    , patchFilePath :: FilePath
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
    tempDir <- getTemporaryDirectory
    let basename = defaultPatchFileName opts
        patchPath = tempDir </> basename
    case makePlan' opts patchPath of
        Left e -> error e
        Right plan -> return plan

-- | Converts options into an execution plan.
makePlan' :: Options -> FilePath -> Either String Plan
makePlan' opts path = do
    xfrm <- if fixedStringsOpt opts
            then Right (TransformFixed
                        caseHandling
                        fixedPattern
                        fixedReplacement)
            else do
                regex <- doCompile
                return $ TransformRegex regex regexReplacement
    case patchFilePathOpt opts of
        Nothing ->
            return $ Plan opts xfrm path
        Just path' ->
            return $ Plan opts xfrm path'
  where
    caseHandling = if ignoreCaseOpt opts
                   then IgnoreCase
                   else ConsiderCase
    fixedPattern = patternStringOpt opts
    fixedReplacement = replacementStringOpt opts
    regexReplacement = parseReplacement $ replacementStringOpt opts
    pcreOpts = if ignoreCaseOpt opts
               then [Light.utf8, Light.caseless]
               else [Light.utf8]
    doCompile = Heavy.compileM (B.pack $ patternStringOpt opts) pcreOpts

-- | Converts a ByteString to a string of hexadecimal digits.
toHexString :: B.ByteString -> String
toHexString = concat . map (printf "%02x") . B.unpack

hashOptions :: Options -> String
hashOptions opts = 
    toHexString $ SHA1.hash $ B.pack $ L.intercalate "\0" planStrings
    where
        planStrings =
            [ patternStringOpt opts
            , replacementStringOpt opts
            , if ignoreCaseOpt opts then "ignoreCase" else ""
            , if fixedStringsOpt opts then "fixedStrings" else ""
            ] ++ filesOpt opts

defaultPatchFileName :: Options -> String
defaultPatchFileName opts = "gsub_" ++ (hashOptions opts) ++ ".diff"
