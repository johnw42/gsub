module Options where

import Types
import Utils

import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Options.Applicative (Parser, ParserResult (Failure, Success), auto,
                            execParser, execParserPure, flag, flag',
                            getParseResult, help, helper, idm, info, internal,
                            long, many, metavar, option, optional, prefs, pure,
                            short, some, strArgument, strOption, switch, value,
                            (<$>), (<*>), (<|>))

-- Command-line options.
data Options
    = Options
      { toReplaceStringOpt   :: String
      , replacementStringOpt :: String
      , filesOpt             :: [FilePath]
      , planModeOpt          :: PlanMode
      , backupSuffixOpt      :: Maybe String
      , replacementModeOpt   :: ReplacementMode
      , patchFilePathOpt     :: FilePath
      , keepGoingOpt         :: Bool
      , caseHandlingOpt      :: CaseHandling
      , contextStringsOpt    :: [String]
      , contextBeforeOpt     :: Int
      , contextAfterOpt      :: Int
      } deriving (Eq, Show)

-- See https://github.com/pcapriotti/optparse-applicative/blob/master/README.md
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
    <*> (pure RunMode
         <|>
         (flag' DryRunMode $
          short 'N' <>
          long "no-modify" <>
          help "Don't modify any files, just show stats.")
         <|>
         (flag' DiffMode $
          short 'D' <>
          long "diff" <>
          help "Don't modify files or create a patch file, just show the diff."))
    <*> (optional $ strOption $
         -- TODO: Is this even useful?
         short 'b' <>
         long "backup-suffix" <>
         metavar "<suffix>" <>
         help "Create backup file by appending suffix (like perl -i).")
    <*> (pure RegexMode
         <|>
         (flag' FixedMode $
          short 'F' <>
          long "fixed-strings" <>
          help "Treat <pattern> and <replacement> as literal strings (like grep -F)."))
    <*> (strOption $
         short 'p' <>
         long "patch-file" <>
         metavar "<path>" <>
         value "gsub#.diff" <>
         help "Set the patch file to create.")
    <*> (switch $
         -- TODO: Is this even useful?
         short 'k' <>
         long "keep-going" <>
         help "Don't stop after encountering errors.")
    <*> (pure ConsiderCase
         <|>
         (flag' IgnoreCase $
          short 'i' <>
          long "ignore-case" <>
          help "Ignore case when matching."))
    <*> (
         (many $ strOption $
          short 'c' <>
          long "context"))
    <*> (pure 0
         <|>
         (option auto $
          short 'B' <>
          long "before-context"))
    <*> (pure 0
         <|>
         (option auto $
          short 'A' <>
          long "after-context"))


execParseArgs :: IO Options
execParseArgs = execParser (info (helper <*> parser) idm)

-- Parse arguments into an error message or an execution plan.
parseArgs :: String -> [String] -> Either String Options
parseArgs progName args =
    maybe (Left "parse faied") Right $
        getParseResult (execParserPure (prefs idm) (info parser idm) args)

defaultOpts p r fs =
    let (Right opts) = parseArgs "gsub" (p:r:fs) in opts
