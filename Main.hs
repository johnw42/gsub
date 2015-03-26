{-# LANGUAGE CPP, TemplateHaskell #-}

module Main (main) where

import Plan
import Utils

import TestUtils
import qualified FindReplace
import qualified OptionsTest

import Control.Applicative ((<$>), (<*>), pure)
import Control.Exception(bracket)
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.List (isInfixOf, isPrefixOf)
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
    maybes <- forM (filesToProcess plan) $ \path -> do
        error <- checkFile path
        return $ fmap (FileError path) error
    return $ catMaybes maybes

transformLine :: Plan -> String -> String
transformLine _ "" = ""
transformLine plan line@(c:cs)
    | pat `isPrefixOf` line = rep ++ transformLine plan (drop (length pat) line)
    | otherwise = c : transformLine plan cs
    where
        pat = patternString plan
        rep = replacementString plan

transformFileContent :: Plan -> String -> String
transformFileContent plan = unlines . map (transformLine plan) . lines

prop_transformFileContent plan before after =
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
        replacement `isInfixOf` result && not (pattern `isInfixOf` result)
    where pattern = patternString plan
          replacement = replacementString plan
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
        makePatches = forM (filesToProcess plan) (processSingleFile plan)
        writePatches patchPath patchParts =
            writeFile patchPath (concat patchParts)

return []
testIt = $forAllProperties quickCheckResult

testMain :: IO ()
testMain = do
    Plan.test
    FindReplace.test
    testIt
    --OptionsTest.test
    return ()

main = do
    args <- getArgs
    if null args
    then testMain
    else do
        planOrError <- execParseArgsToPlan
        case planOrError of
            Left error -> putStrLn error
            Right plan -> do
                errors <- validateFiles plan
                mapM_ print errors
                when (null errors) $ do
                    errors <- processFiles plan
                    mapM_ print errors

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
