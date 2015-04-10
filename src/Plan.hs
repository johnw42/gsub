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
import Text.Regex.Base (makeRegexOptsM)
import Text.Regex.PCRE.String (Regex, compUTF8, compCaseless, execBlank)

import Control.Exception.Base
import Text.Regex.Base
import Text.Regex.PCRE.String
import System.IO.Unsafe

-- An execution plan.
data Plan = Plan
    { options :: Options
    , patchFilePath :: FilePath
    , replacement :: Replacement
    , patternRegex :: Maybe Regex
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

makePlan :: Options -> Either String Plan
makePlan opts = do
    regex <- if fixedStringsOpt opts
        then Left "internal error"
        else doCompile (patternStringOpt opts)
    return $ Plan opts
        (defaultPatchFileName opts)
        (if fixedStringsOpt opts
            then literalReplacement (replacementStringOpt opts)
            else parseReplacement (replacementStringOpt opts))
        (if fixedStringsOpt opts then Nothing else Just regex)
    where
      doCompile :: String -> Either String Regex
      doCompile pat =
        either (Left . show) Right (unsafePerformIO $ compile compOpt execOpt pat)
      compOpt = if ignoreCaseOpt opts
                then compUTF8 .|. compCaseless
                else compUTF8
      execOpt = execBlank

-- Convert a ByteString to a string of hexadecimal digits
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
