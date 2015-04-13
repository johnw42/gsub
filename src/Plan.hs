module Plan where

import FindReplace
import Options
import Utils

import Control.Monad (liftM)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits ((.|.))
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)

import Control.Exception.Base
import qualified Text.Regex.PCRE.Heavy as Heavy
import qualified Text.Regex.PCRE.Light as Light

data Transformation
    = TransformRegex Light.Regex Replacement
    | TransformFixed String String

-- An execution plan.
data Plan = Plan
    { options :: Options
    , patchFilePath :: FilePath
    , transformation :: Transformation
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

defaultPlan p r fs =
    let (Right plan) = parseArgs "gsub" (p:r:fs) in plan
                                                    
execParseArgsToPlan :: IO (Either String Plan)
execParseArgsToPlan = makePlan `liftM` execParseArgs

-- | Converts options into an execution plan.
makePlan :: Options -> Either String Plan
makePlan opts = do
    xfrm <- if fixedStringsOpt opts
            then Right $ TransformFixed fixedPattern fixedReplacement
            else do
                regex <- doCompile
                return $ TransformRegex regex regexReplacement
    return $ Plan opts
        (defaultPatchFileName opts)
        xfrm
    where
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
defaultPatchFileName opts = ".gsub_" ++ (hashOptions opts) ++ ".diff"
