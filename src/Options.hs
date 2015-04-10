module Options where

import Utils

import Data.Maybe (isJust, fromJust)
import Options.Applicative ((<>), (<$>), (<*>), (<|>), Parser, ParserResult(Success, Failure), execParser, execParserPure, flag, flag', getParseResult, help, helper, idm, info, internal, long, many, metavar, optional, prefs, pure, short, some, strArgument, strOption, switch)

type Error = String

data PlanMode
    = RunMode
    | DryRunMode
    | DiffMode
    | UndoMode
    deriving (Eq, Show)

-- Command-line options.
data Options = Options {
    patternStringOpt :: String,
    replacementStringOpt :: String,
    filesOpt :: [FilePath],
    planModeOpt :: PlanMode,
    backupSuffixOpt :: Maybe String,
    fixedStringsOpt :: Bool,
    patchFilePathOpt :: Maybe FilePath,
    keepGoingOpt :: Bool,
    ignoreCaseOpt :: Bool,
    undoDirOpt :: Maybe FilePath
    } deriving (Eq, Show)

parser :: Parser Options
parser =
    Options
    <$> (strArgument $
         metavar "<pattern>" <>
         help "Regex or string to replace.")
    <*> (strArgument $
         metavar "<replacement>" <>
         help "Replacement string.")
    <*> (some $ strArgument $
         metavar "<file>..." <>
         help "List of files to process.")
    <*> (pure RunMode <|>
         (flag' DiffMode $
          short 'D' <>
          long "diff" <>
          help "Don't modify files or create a patch file, just show the diff.") <|>
         (flag' DryRunMode $
          short 'N' <>
          long "no-modify" <>
          help "Don't modify files, just create a patch file.") <|>
         (flag' UndoMode $
          short 'u' <>
          long "undo" <>
          help "Undo a previous command with the same arguments."))
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
    <*> (optional $ strOption $
         long "--undo-dir" <>
         internal)
         

execParseArgs :: IO Options
execParseArgs = execParser (info (helper <*> parser) idm)

-- Parse arguments into an error message or an execution plan.
parseArgs :: String -> [String] -> Either String Options
parseArgs progName args =
    maybe (Left "parse faied") Right $
        getParseResult $ execParserPure (prefs idm) (info parser idm) args

defaultOpts p r fs =
    let (Right opts) = parseArgs "gsub" (p:r:fs) in opts
