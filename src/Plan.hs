module Plan where

import FindReplace
import Options

import TestUtils

import Data.Char (isHexDigit)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits ((.|.))
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Control.Monad (foldM, liftM)
import Text.Printf (printf)
import Text.Regex.Base (makeRegexOptsM)
import Text.Regex.PCRE.String (Regex, compUTF8, compCaseless, execBlank)

-- An execution plan.
data Plan = Plan
    { options :: Options
    , patchFilePath :: FilePath
    , replacement :: Replacement
    , patternRegex :: Either String Regex
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

instance Show Plan where
    show _ = "<Plan>"

instance Arbitrary Plan where
    arbitrary = makePlan `liftM` arbitrary

execParseArgsToPlan :: IO Plan
execParseArgsToPlan = makePlan `liftM` execParseArgs

makePlan opts = plan
    where
        plan = Plan opts
            (defaultPatchFileName opts)
            (if fixedStringsOpt opts
                then literalReplacement (replacementStringOpt opts)
                else parseReplacement (replacementStringOpt opts))
            (if fixedStringsOpt opts
                then Left "internal error"
                else compile (patternStringOpt opts))
        compile pat = makeRegexOptsM compOpt execOpt pat
        compOpt = if ignoreCaseOpt opts
            then compUTF8 .|. compCaseless
            else compUTF8
        execOpt = execBlank

-- Convert a ByteString to a string of hexadecimal digits
toHexString :: B.ByteString -> String
toHexString = concat . map (printf "%02x") . B.unpack

prop_toHexString1 s = length (toHexString (B.pack s)) == 2 * length s
prop_toHexString2 s = all isHexDigit (toHexString (B.pack s))

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
defaultPatchFileName plan = ".gsub_" ++ (hashOptions plan) ++ ".diff"