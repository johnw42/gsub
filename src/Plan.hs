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
data Plan = Plan {
    options :: Options,
    patchFilePath :: FilePath,
    replacement :: Replacement,
    patternRegex :: Either String Regex
    }

instance Show Plan where
    show _ = "<Plan>"


instance Arbitrary Plan where
    arbitrary = makePlan `liftM` arbitrary

makePlan opts = plan
    where
        plan = Plan opts
            (defaultPatchFileName opts)
            (if fixedStrings opts
                then literalReplacement (replacementString opts)
                else parseReplacement (replacementString opts))
            (if fixedStrings opts
                then Left "internal error"
                else compile (patternString opts))
        compile pat = makeRegexOptsM compOpt execOpt pat
        compOpt = if ignoreCase opts
            then compUTF8 .|. compCaseless
            else compUTF8
        execOpt = execBlank

-- Convert a ByteString to a string of hexadecimal digits
toHexString :: B.ByteString -> String
toHexString = concat . map (printf "%02x") . B.unpack

prop_toHexString1 s = length (toHexString (B.pack s)) == 2 * length s
prop_toHexString2 s = all isHexDigit (toHexString (B.pack s))

hashPlan :: Options -> String
hashPlan plan = 
    toHexString $ SHA1.hash $ B.pack $ L.intercalate "\0" planStrings
    where
        planStrings =
            [ patternString plan
            , replacementString plan
            ] ++ filesToProcess plan

defaultPatchFileName :: Options -> String
defaultPatchFileName plan = ".gsub_" ++ (hashPlan plan) ++ ".diff"