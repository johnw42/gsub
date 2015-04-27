module Gsub where

import DiffDB
import FindReplace
import Options
import Plan

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Either
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Monoid (First(..), mconcat)
import System.Directory
import System.Environment
import System.Exit
import System.IO hiding (stderr, stdout)
import System.Process

type PatchData = String

data FileError = FileError FilePath Error

instance Show FileError where
    show (FileError path error) = path ++ ": " ++ error

-- | Prints errors and if there are any, otherwise executes an action.
exitIfErrors :: Handle -> [FileError] -> IO ()
exitIfErrors stderr errors = do
    unless (null errors) $ do
        mapM_ (hPrint stderr) errors
        exitWith (ExitFailure 2)

-- | Tests whether a file can be operated on.  Adds an error if it
-- can't be.
checkFile :: FilePath -> IO (Maybe FileError)
checkFile path = loop checks
  where
    loop [] = return Nothing
    loop (c:cs) = do
        problem <- c
        maybe (loop cs) (return . Just) problem

    checks =
        [ check "is a directory" $ fmap not . doesDirectoryExist
        , check "no such file" $ doesFileExist
        , check "not readable" $ fmap readable . getPermissions
        , check "not writable" $ fmap writable . getPermissions
        ]

    check problem test = do
        ok <- test path
        if ok
            then return Nothing
            else return (Just (FileError path problem))

-- | Checks that all files in the plan can be operated upon.
validateFiles :: Plan -> IO [FileError]
validateFiles plan =
    fmap catMaybes $ mapM checkFile (filesToProcess plan)

-- | Applies the specified transformation to a line of a file.
transformLine :: Transformation -> FileContent -> FileContent
transformLine (TransformFixed ch needle rep) =
    L8.pack . transformLineFixed ch needle rep . L8.unpack
transformLine (TransformRegex regex rep) =
    transformLineRegex regex rep

-- | Applies the specified transformation to a whole file's content.
transformFileContent :: Plan -> FileContent -> FileContent
transformFileContent plan = L8.unlines . map transform . L8.lines
  where
    transform = transformLine (transformation plan)

-- | Finds the flags that should be passed to @diff@.
diffFlags :: FilePath -> [String]
diffFlags path =
    [ "-u"
    , "--label=" ++ path
    , "--label=" ++ path
    , "--"
    ]
    
-- | Runs the external diff tool over a pair of files.
runDiff :: FilePath -> FilePath -> IO PatchData
runDiff oldPath newPath = do
    (_, diffStdout, diffStderr) <-
        readProcessWithExitCode "diff" diffArgs diffInput
    unless (null diffStderr) $
        error ("diff wrote to stderr:\n" ++ diffStderr)
    return diffStdout
  where
    diffInput = ""
    diffArgs = diffFlags oldPath ++ [oldPath, newPath]

-- | Show the difference between old content and new.
showDiff :: FilePath -> FileContent -> IO String
showDiff path newContent = do
    (_, diffStdout, diffStderr) <-
        readProcessWithExitCode "diff" diffArgs (L8.unpack newContent)
    unless (null diffStderr) $
        error ("diff wrote to stderr:\n" ++ diffStderr)
    return diffStdout
  where
    diffArgs = diffFlags path ++ [path, "-"]

-- | Runs an action with a temporary file, which is deleted afterward.
withTempFile
    :: FilePath                      -- ^ The temp directory to use
    -> String                        -- ^ The temp file template
    -> (FilePath -> Handle -> IO a)  -- ^ The action to run
    -> IO a
withTempFile dir template f =
    bracket open closeAndDelete (uncurry f)
  where
    open = openTempFile dir template
    closeAndDelete (path, handle) = do
        finally
            (hClose handle)
            (removeFile path)

-- | Similar to 'withTempFile', but hardcoded to use the system
-- temporary directory.
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile template f = do
    dir <- getTemporaryDirectory
    withTempFile dir template f

-- | Replace the contents of a file.
updateFileContent :: FilePath -> FilePath -> FileContent -> IO ()
updateFileContent patchPath path newContent =
    withSystemTempFile "gsub.tmp" $ \tempPath tempH -> do
        L8.hPut tempH newContent
        hClose tempH
        patch <- runDiff path tempPath
        appendFile patchPath patch
        copyFile tempPath path

-- | Processes a single file.  Returns (Just path) if the file was
-- modified.
processSingleFile :: Plan
                  -> FilePath
                  -> IO (Maybe String)
processSingleFile plan path = do
    oldContent <- L8.readFile path
    let patchFile = patchFilePath plan
    patchExists <- doesFileExist patchFile
    when patchExists $
        removeFile patchFile
    let newContent = transformFileContent plan oldContent
    if newContent == oldContent
        then return Nothing
        else case planMode plan of
                 RunMode -> do
                     updateFileContent patchPath path newContent
                     return $ Just (path ++ "\n")
                 DryRunMode ->
                     return  $ Just (path ++ "\n")
                 DiffMode -> do
                     Just `liftM` showDiff path newContent
  where
    patchPath = patchFilePath plan

-- | Reverts a change made by a previous run.
revertPatch :: Plan -> IO String
revertPatch plan = do
    patchData <- readFile (patchFilePath plan)
    (_, pStdout, pStderr) <-
        readProcessWithExitCode "patch" ["-R", "-p0"] patchData
    unless (null pStderr) $
        error ("patch wrote to stderr:\n" ++ pStderr)
    return pStdout

-- | Processes all the files in the plan.  Returns a list of files
-- that were modified.
processFiles :: Plan -> IO [String]
processFiles plan =
    case planMode plan of
        UndoMode -> do
            output <- revertPatch plan
            return [output]
        _ ->
            fmap catMaybes $
            forM (filesToProcess plan) (processSingleFile plan)

main :: Handle -> Handle -> IO ()
main stdout stderr = do
    handle printError $ do
        opts <- execParseArgs
        plan <- makePlan opts
        errors <- validateFiles plan
        exitIfErrors stderr errors
        messages <- processFiles plan
        forM_ messages $ hPutStr stdout
  where
    printError (ErrorCall msg) = do
        name <- getProgName
        hPutStrLn stderr (name ++ ": " ++ msg)
        exitWith (ExitFailure 1)
