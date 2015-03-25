module Plan (Plan(..), PlanMode(..), parseArgs, execParseArgs) where

import Utils

import TestUtils hiding (Success)

import Data.Char (isHexDigit)
import qualified Data.List as L
import Data.Maybe (isJust)
import Control.Monad (foldM)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as B
import Options.Applicative (
    (<>), (<$>), (<*>), (<|>), Parser, ParserResult(Success, Failure), execParser, execParserPure,
    flag, flag', help, helper, idm, info, long, many, metavar,
    optional, prefs, pure, short, some, strArgument, strOption, switch)
import Text.Printf (printf)
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), ArgOrder(Permute), OptDescr(Option), getOpt, usageInfo)

type Error = String

data PlanMode
    = RunMode
    | DryRunMode
    | DiffMode
    | UndoMode
    deriving (Eq, Show)

-- An execution plan.
data Plan = Plan {
  patternString :: String,
  replacementString :: String,
  filesToProcess :: [FilePath],
  planMode :: PlanMode,
  backupSuffix :: Maybe String,
  fixedStrings :: Bool,
  specifiedPatchFilePath :: Maybe FilePath,
  keepGoingAfterErrors :: Bool,
  patchFilePath :: FilePath
  } deriving (Eq, Show)

makePlan a b c d e f g h = plan
    where
        plan = Plan a b c d e f g h (defaultPatchFileName plan)

instance Arbitrary Plan where
    arbitrary = makePlan 
        <$> arbitrary `suchThat` ('\n' `notElem`) `suchThat` (not . null)
        <*> arbitrary `suchThat` ('\n' `notElem`)
        <*> arbitrary
        <*> elements [RunMode, DryRunMode, DiffMode, UndoMode]
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

parser :: Parser Plan
parser = makePlan
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
            short 'i' <>
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
            short 'k' <>
            long "keep-going" <>
            help "Don't stop after encountering errors.")

execParseArgs :: IO Plan
execParseArgs = execParser (info (helper <*> parser) idm)

-- Parse arguments into an error message or an execution plan.
parseArgs :: String -> [String] -> Either String Plan
parseArgs progName args =
    case execParserPure (prefs idm) (info parser idm) args of
        Success plan -> Right plan
        _ -> Left "parse failed"


-- Convert a ByteString to a string of hexadecimal digits
toHexString :: B.ByteString -> String
toHexString = concat . map (printf "%02x") . B.unpack

prop_toHexString1 s = length (toHexString (B.pack s)) == 2 * length s
prop_toHexString2 s = all isHexDigit (toHexString (B.pack s))

hashPlan :: Plan -> String
hashPlan plan = 
    toHexString $ SHA1.hash $ B.pack $ L.intercalate "\0" planStrings
    where
        planStrings =
            [ patternString plan
            , replacementString plan
            ] ++ filesToProcess plan

defaultPatchFileName :: Plan -> String
defaultPatchFileName plan = ".gsub_" ++ (hashPlan plan) ++ ".diff"