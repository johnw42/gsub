module Plan (Plan(..), PlanMode(..), parseArgs, execParseArgs) where

import TestUtils hiding (Success)

import Control.Monad (foldM)
import Options.Applicative (
    (<>), (<$>), (<*>), (<|>), Parser, ParserResult(Success, Failure), execParser, execParserPure,
    flag, flag', help, helper, idm, info, long, many, metavar,
    optional, prefs, pure, short, some, strArgument, strOption, switch)
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
  patchFilePath :: Maybe FilePath,
  keepGoingAfterErrors :: Bool
  } deriving (Eq, Show)

instance Arbitrary Plan where
    arbitrary =
        Plan <$> arbitrary `suchThat` ('\n' `notElem`) `suchThat` (not . null)
             <*> arbitrary `suchThat` ('\n' `notElem`)
             <*> arbitrary
             <*> elements [RunMode, DryRunMode, DiffMode, UndoMode]
             <*> arbitrary
             <*> arbitrary
             <*> arbitrary
             <*> arbitrary

parser :: Parser Plan
parser = Plan
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
