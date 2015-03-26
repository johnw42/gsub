module Plan (Options(..), Plan(..), PlanMode(..), defaultPlan, parseArgs, execParseArgs) where

import FindReplace
import Utils

import TestUtils hiding (Success)

import Data.Bits ((.|.))
import Data.Char (isHexDigit)
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)
import Control.Monad (foldM, liftM)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as B
import Options.Applicative (
    (<>), (<$>), (<*>), (<|>), Parser, ParserResult(Success, Failure), execParser, execParserPure,
    flag, flag', getParseResult, help, helper, idm, info, long, many, metavar,
    optional, prefs, pure, short, some, strArgument, strOption, switch)
import Text.Printf (printf)
import Text.Regex.Base (makeRegexOptsM)
import Text.Regex.PCRE.String (Regex, compUTF8, compCaseless, execBlank)

type Error = String

data PlanMode
    = RunMode
    | DryRunMode
    | DiffMode
    | UndoMode
    deriving (Eq, Show)

-- Command-line options.
data Options = Options {
    patternString :: String,
    replacementString :: String,
    filesToProcess :: [FilePath],
    planMode :: PlanMode,
    backupSuffix :: Maybe String,
    fixedStrings :: Bool,
    specifiedPatchFilePath :: Maybe FilePath,
    keepGoingAfterErrors :: Bool,
    ignoreCase :: Bool
    } deriving (Eq, Show)

instance Arbitrary Options where
    arbitrary = Options 
        <$> arbitrary `suchThat` ('\n' `notElem`) `suchThat` (not . null)
        <*> arbitrary `suchThat` ('\n' `notElem`)
        <*> arbitrary
        <*> elements [RunMode, DryRunMode, DiffMode, UndoMode]
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

-- An execution plan.
data Plan = Plan {
    options :: Options,
    patchFilePath :: FilePath,
    replacement :: Replacement,
    patternRegex :: Either String Regex
    }

instance Show Plan where
    show _ = "<Plan>"

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

instance Arbitrary Plan where
    arbitrary = makePlan `liftM` arbitrary

parser :: Parser Options
parser = Options
    <$> (strArgument $
            metavar "<pattern>" <>
            help "Regex or string to replace.")
    <*> (strArgument $
            metavar "<replacement>" <>
            help "Replacement string.")
    <*> (some $ strArgument $
            metavar "<file>..." <>
            help "List of files to process.")
    <*> (pure RunMode
        <|> (flag' DiffMode $
                short 'D' <>
                long "diff" <>
                help "Don't modify files or create a patch file, just show the diff.")
        <|> (flag' DryRunMode $
                short 'N' <>
                long "no-modify" <>
                help "Don't modify files, just create a patch file.")
        <|> (flag' UndoMode $
                short 'u' <>
                long "undo" <>
                help "Undo a previous command with the same arguments.")
        )
    <*> (optional $ strOption $
            -- TODO: Is this even useful?
            short 'b' <>
            long "backup-suffix" <>
            metavar "<suffix>" <>
            help "Create backup file by appending suffix (like perl -i).")
    <*> (switch $
            short 'F' <>
            long "fixed-strings" <>
            help "Treat <pattern> and <replacement> as literal strings (like grep -F).")
    <*> (optional $ strOption $
            short 'P' <>
            long "patch-file" <>
            metavar "<path>" <>
            help "Set the backup file to create.")
    <*> (switch $
            -- TODO: Is this even useful?
            short 'k' <>
            long "keep-going" <>
            help "Don't stop after encountering errors.")
    <*> (switch $
            short 'i' <>
            long "ignore-case" <>
            help "Ignore case when matching.")

execParseArgs :: IO Plan
execParseArgs = makePlan `liftM` execParser (info (helper <*> parser) idm)

-- Parse arguments into an error message or an execution plan.
parseArgs :: String -> [String] -> Either String Options
parseArgs progName args =
    maybe (Left "parse faied") Right $
        getParseResult $ execParserPure (prefs idm) (info parser idm) args

defaultPlan p r fs =
    let (Right plan) = parseArgs "gsub" (p:r:fs) in plan

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