module Plan where

import FindReplace
import Options

import TestUtils

import Control.Monad (foldM, liftM)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits ((.|.))
import qualified Data.ByteString.Char8 as B
import Data.Char (isHexDigit)
import Data.Either (isLeft, isRight)
import qualified Data.List as L
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

instance Show Plan where
    show plan = "<Plan> { " ++ show (options plan) ++ " }"

instance Arbitrary Plan where
    arbitrary = do
        (Right plan) <- liftM makePlan arbitrary `suchThat` isRight
        return plan

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
        doCompile pat = either (Left . show) Right (unsafePerformIO $ compile compOpt execOpt pat)
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

return []
test = do
    $forAllProperties quickCheckResult