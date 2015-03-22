module Plan (Plan(..), PlanMode(..), defaultPlan, parseArgs) where

import Control.Monad (foldM)
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), ArgOrder(Permute), OptDescr(Option), getOpt, usageInfo)

type Error = String

-- An execution plan.
data Plan = Plan {
  planMode :: PlanMode,
  filesToProcess :: [FilePath],
  patternString :: String,
  replacementString :: String,
  backupSuffix :: Maybe String,
  fixedStrings :: Bool,
  patchFilePath :: Maybe FilePath,
  keepGoingAfterErrors :: Bool
  } deriving (Eq, Show)

data PlanMode
    = RunMode
    | DryRunMode
    | DiffMode
    | UndoMode
    | HelpMode
    deriving (Eq, Show)

defaultPlan :: String -> String -> [FilePath] -> Plan
defaultPlan pattern replacement files = Plan
    { planMode = RunMode
    , filesToProcess = files
    , patternString = pattern
    , replacementString = replacement
    , backupSuffix = Nothing
    , fixedStrings = False
    , patchFilePath = Nothing
    , keepGoingAfterErrors = False
    }

-- Convert a series of command-line parameters into an execution plan.
makePlan :: String -> String -> [String] -> [Flag] -> Either String Plan
makePlan pattern replacement files flags = foldM applyFlag startingPlan flags
    where
        startingPlan = defaultPlan pattern replacement files
        applyFlag :: Plan -> Flag -> Either String Plan
        applyFlag plan flag = case flag of
            BackupSuffixFlag suffix -> Right plan { backupSuffix = Just suffix }
            FixedStringsFlag -> Right plan { fixedStrings = True }
            PatchFileFlag path -> Right plan { patchFilePath = Just path }
            UndoFlag | runMode -> Right plan { planMode = UndoMode }
            DiffFlag | runMode -> Right plan { planMode = DiffMode }
            NoModifyFlag | runMode -> Right plan { planMode = DryRunMode }
            _ -> Left "can't construct plan"
            where runMode = planMode plan == RunMode

-- Abstract set of possible flags.
data Flag
  = HelpFlag
  | BackupSuffixFlag String
  | FixedStringsFlag
  | PatchFileFlag FilePath
  | UndoFlag
  | DiffFlag
  | NoModifyFlag
  deriving Eq

-- Define the options than can be parsed into flags.
optSpecs :: [OptDescr Flag]
optSpecs =
  [ Option "h" ["help"] (NoArg HelpFlag)
    "Print help message."
  , Option "i" ["backup-suffix"] (ReqArg BackupSuffixFlag "SUFFIX")
    "Create backup file by appending suffix (like perl -i)."
  , Option "F" ["fixed-strings"] (NoArg FixedStringsFlag)
    "Treat <pattern> and <replacement> as literal strings (like grep -F)."
  , Option "P" ["patch-file"] (ReqArg PatchFileFlag "FILE")
    "Set the backup file to create."
  , Option "u" ["undo"] (NoArg UndoFlag)
    "Undo a previous command with the same arguments."
  , Option "N" ["no-modify"] (NoArg NoModifyFlag)
    "Don't modify files, just create a patch file."
  , Option "D" ["diff"] (NoArg DiffFlag)
    "Don't modify files or create a patch file, just show the diff."
  ]

-- Parse arguments into an error message or an execution plan.
parseArgs :: String -> [Error] -> Either String Plan
parseArgs progName args
  | HelpFlag `elem` flags = Left usage
  | not (null errors) = Left $ concat $ map withProgName errors
  | otherwise = case otherArgs of
                  (pattern:replacement:file:files) ->
                      makePlan pattern replacement (file:files) flags
                  _ -> Left $ withProgName "not enough arguments\n"
  where
    (flags, otherArgs, errors) = getOpt Permute optSpecs args
    usage = usageInfo progName optSpecs
    withProgName error = progName ++ ": " ++ error
