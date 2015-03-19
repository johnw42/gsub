{-# LANGUAGE CPP, TemplateHaskell #-}

#ifdef TEST
module Main where
#else
module Main (main) where
#endif

import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import Test.QuickCheck
import Text.Regex.Base
import Text.Regex.PCRE.Light

type Error = String

data FileError = FileError FilePath Error

instance Show FileError where
    show (FileError path error) = path ++ ": " ++ error

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

data PlanMode = RunMode | DryRunMode | DiffMode | UndoMode deriving (Eq, Show)

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

-- Find the first Just in a list.
firstJust :: [Maybe a] -> Maybe a
firstJust = head . (++[Nothing]) . dropWhile isNothing

-- Try to find a reason why a file can't be operated on.
checkFile :: FilePath -> IO (Maybe Error)
checkFile path = liftM firstJust $ sequence $ map ($ path) checks
    where
        makeCheck reason test path = do
            ok <- test path
            return $ if ok then Nothing else Just reason
        checks =
            [ makeCheck "is a directory" $ fmap not . doesDirectoryExist
            , makeCheck "no such file" $ doesFileExist
            , makeCheck "not readable" $ fmap readable . getPermissions
            , makeCheck "not writable" $ fmap writable . getPermissions
            ]

validateFiles :: Plan -> IO [FileError]
validateFiles plan = do
    maybes <- forM (filesToProcess plan) $ \path -> do
        error <- checkFile path
        return $ fmap (FileError path) error
    return $ catMaybes maybes

processFiles :: Plan -> IO [FileError]
processFiles = undefined

realMain = do
  args <- getArgs
  progName <- getProgName
  case parseArgs progName args of
    Left error -> putStr error
    Right plan -> do
      errors <- validateFiles plan
      mapM_ print errors
      when (null errors) $ do
          errors <- processFiles plan
          mapM_ print errors

return []
quickCheckMain = $quickCheckAll

--(require file/sha1)
--
--(module+ test
--  (require rackunit))
--
--(define current-file-name (make-parameter (void)))
--(define pattern-regexp (make-parameter (void)))
--(define warning-count (box 0))
--
--(module+ test
--  (parameterize ([current-command-line-arguments #()])
--    (check-exn
--     exn:fail?
--     parse-command-line)))
--
--;; Debugging helper.  Prints and returns a value.
--(define trace
--  (case-lambda
--    [(value)
--     (trace #f identity value)]
--    [(label-or-format-proc value)
--     (if (string? label-or-format-proc)
--         (trace label-or-format-proc identity value)
--         (trace #f label-or-format-proc value))]
--    [(label format-proc value)
--     (eprintf "~a~v\n"
--              (if (not label) "" (string-append label ": "))
--              (format-proc value))
--     value]))
--
--(define (box-update! the-box update-proc)
--  (let ([value (unbox the-box)])
--    (box-cas! value (update-proc value))))
--
--(define (generate-patch-file-path)
--  (trace "filename" path->string
--   (build-path
--    (find-system-path 'temp-dir)
--    (string-append
--     "gsub."
--     (sha1 (open-input-string (generate-patch-file-spec)))))))
--
--(define (generate-patch-file-spec)
--  (trace "path-spec"
--   (format "~v ~v ~v"
--           (sort
--            (map (compose path->string path->complete-path)
--                 (files-to-process))
--            string<?)
--           (pattern-string)
--           (replacement-string))))
--
--;; (define (init-pattern-and-replacement pattern replacement fixed-strings? reverse?)
--;;   (cond
--;;     [reverse?
--;;      (init-pattern-and-replacement replacement pattern #t #f)]
--
--;;     [fixed-strings?
--;;      (init-pattern-and-replacement (regexp-quote pattern)
--;;                                    (regexp-replace-quote replacement)
--;;                                    #f #f)]
--
--;;     [else
--;;      (pattern-string pattern)
--;;      (replacement-string replacement)]))
--
--
--;; Returns #f if |path| is able to be update, or a warning string if
--;; not.
--(define (file-warning path)
--  (let ([perms (delay (file-or-directory-permissions path))])
--    (cond
--      [(directory-exists? path)
--       "is a directory"]
--      [(not (file-exists? path))
--       "no such file"]
--      [(not (memq 'read (force perms)))
--       "file is not readable"]
--      [(not (memq 'write (force perms)))
--       "file is not writable"]
--      [else #f])))
--
--;; Check all paths in |paths| for errors that will prevent them from
--;; being updated.  Prints a warning for files that can't be updated,
--;; and returns a list of files that are usable.
--(define (check-files paths)
--  (append*
--   (for/list ([path paths])
--     (match (file-warning path)
--       [#f (list path)]
--       [warning (warn-file path warning)]))))
--
--;; Raises an error if execution should terminate because of warnings.
--(define (check-errors)
--  (when (and (positive? (unbox warning-count))
--             (not (keep-going-after-errors?)))
--    (raise-user-error "aborting because of warnings")))
--
--;; (define (call-with-atomic-output-file filename proc)
--;;   (define-values (dir-path last-part must-be-dir?)
--;;     (split-path (path->complete-path filename)))
--;;   (define temp-file-path
--;;     (make-temporary-file
--;;      (string-append (path->string last-part) ".~a")))
--;;   (dynamic-wind
--;;     (lambda ())
--;;     (lambda ()
--;;       (with-handlers ([exn? (lambda (e)
--;;                               (delete-file temp-file-path))])
--;;         (call-with-output-file temp-file-path proc
--;;           #:mode 'text #:exists 'must-truncate)))
--;;     (lambda ()
--;;       (when (file-exists? temp-file-path)
--;;         (rename-file ))))
--;;   )
--
--(define (transform-file-content text)
--  (string-replace text (pattern-regexp) (replacement-string)))
--
--(define (run-diff old-path new-path)
--  (define label (path->string old-path))
--  (match-define-values
--   (proc proc-stdout proc-stdin #f)
--   (subprocess
--    #f
--    #f
--    (current-error-port)
--    (find-executable-path "diff")
--    "-u"
--    "--label" label
--    "--label" label
--    "--"
--    (path->string old-path)
--    (path->string new-path)))
--  (close-output-port proc-stdin)
--  (define diff-data (port->string proc-stdout))
--  (displayln diff-data)
--  (close-input-port proc-stdout))
--
--(define (copy-with-backup from-path to-path)
--  ;; Copies a file, creating backups according to the |backup-suffix|
--  ;; parameter.
--  (apply system*
--         (find-executable-path "cp")
--         (flatten
--          (list
--           "-f"
--           (if (backup-suffix)
--               (list "--suffix" (backup-suffix)
--                     "--backup=numbered")
--               null)
--           "--"
--           (path->string from-path)
--           (path->string to-path)))))
--
--(define (process-one-file path)
--  (define input-data (file->string path))
--  (define temp-file-path (make-temporary-file))
--  (define diff-path (patch-file-path))
--  (dynamic-wind
--    (lambda () #f)
--    (lambda ()
--      (display-to-file (transform-file-content input-data)
--                       temp-file-path #:exists 'truncate)
--      (run-diff path temp-file-path)
--      (when (update-files-in-place?)
--        (unless
--            (apply system*
--                   (find-executable-path "cp")
--                   (flatten
--                    (list
--                     "-f"
--                     (if (backup-suffix)
--                         (list "--suffix" (backup-suffix)
--                               "--backup=numbered")
--                         null)
--                     "--"
--                     (path->string temp-file-path)
--                     (path->string path))))
--          (error "copy failed"))))
--    (lambda ()
--      (delete-file temp-file-path))))
--
--(define (process-files paths)
--  (for ([path paths])
--    (parameterize ([current-file-name path])
--      (process-one-file path))))
--
--;; Prints a warning about a file an increments the warning counter.
--(define (warn-file path message)
--  (eprintf "~a: ~a\n" path message)
--  (box-update! warning-count add1))
--
--(define (replace-in-files paths)
--  (let ([paths (check-files paths)])
--    (check-errors)
--    (process-files paths))
--  (void))
--
--(module+ main
--  (replace-in-files (parse-command-line)))
