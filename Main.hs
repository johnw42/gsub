{-# LANGUAGE CPP, TemplateHaskell #-}

#ifdef TEST
module Main where
#else
module Main (main) where
#endif

import Plan
import Utils

import TestUtils
import qualified FindReplace
import qualified PlanTest

import Control.Applicative ((<$>), (<*>), pure)
import Control.Exception(bracket)
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Char (isHexDigit)
import Data.Maybe
import Data.Traversable (sequenceA)
import System.Directory
import System.Environment
import System.IO
import System.Process (readProcess)
import Text.Printf (printf)

type Error = String

data FileError = FileError FilePath Error

instance Show FileError where
    show (FileError path error) = path ++ ": " ++ error

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
    maybes <- forM (filesToProcess $ options plan) $ \path -> do
        error <- checkFile path
        return $ fmap (FileError path) error
    return $ catMaybes maybes

transformLine :: Plan -> String -> String
transformLine _ "" = ""
transformLine plan line@(c:cs)
    | pat `L.isPrefixOf` line = rep ++ transformLine plan (drop (length pat) line)
    | otherwise = c : transformLine plan cs
    where
        pat = patternString $ options plan
        rep = replacementString $ options plan

transformFileContent :: Plan -> String -> String
transformFileContent plan = unlines . map (transformLine plan) . lines

prop_transformFileContent plan before after =
    not (pattern `L.isInfixOf` (replacement ++ after)) ==>
    not (pattern `L.isInfixOf` (before ++ replacement)) ==>
        replacement `L.isInfixOf` result && not (pattern `L.isInfixOf` result)
    where pattern = patternString $ options plan
          replacement = replacementString $ options plan
          content = before ++ pattern ++ after
          result = transformFileContent plan content

type PatchData = String

runDiff :: FilePath -> FilePath -> IO PatchData
runDiff oldPath newPath = do
    readProcess "diff" diffArgs diffInput
    where
        diffInput = ""
        diffArgs =
            [ "-u"
            , "--label=" ++ oldPath
            , "--label=" ++ oldPath
            , "--", oldPath, newPath
            ]

processSingleFile :: Plan -> FilePath -> IO PatchData
processSingleFile plan path = do
    tempDir <- getTemporaryDirectory
    oldContent <- readFile path
    bracket (openTempFile tempDir "gsub.txt") (hClose . snd) $
        \(tempPath, tempH) -> do
            hPutStr tempH (transformFileContent plan oldContent)
            hFlush tempH
            runDiff path tempPath

processFiles :: Plan -> IO [FileError]
processFiles plan = do
    makePatches >>= (writePatches $ patchFilePath plan)
    return []  -- TODO
    where
        makePatches :: IO [PatchData]
        makePatches = forM (filesToProcess $ options plan) (processSingleFile plan)
        writePatches patchPath patchParts =
            writeFile patchPath (concat patchParts)

-- Convert a ByteString to a string of hexadecimal digits
toHexString :: B.ByteString -> String
toHexString = concat . map (printf "%02x") . B.unpack

prop_toHexString1 s = length (toHexString (B.pack s)) == 2 * length s
prop_toHexString2 s = all isHexDigit (toHexString (B.pack s))

hashOptions :: Options -> String
hashOptions plan = 
    toHexString $ SHA1.hash $ B.pack $ L.intercalate "\0" planStrings
    where
        planStrings =
            [ patternString plan
            , replacementString plan
            ] ++ filesToProcess plan

defaultPatchFileName :: Options -> String
defaultPatchFileName opts = ".gsub_" ++ (hashOptions opts) ++ ".diff"

return []
testIt = $forAllProperties quickCheckResult

testMain :: IO ()
testMain = do
    FindReplace.test
    testIt
    --PlanTest.test
    return ()

main = do
    args <- getArgs
    if null args
    then testMain
    else do
        plan <- execParseArgs
        errors <- validateFiles plan
        mapM_ print errors
        when (null errors) $ do
            errors <- processFiles plan
            mapM_ print errors


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
