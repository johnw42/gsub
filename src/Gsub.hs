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

data AppState
    = AppState
      { fileErrors :: IORef [FileError]
      , touchedFiles :: IORef [FilePath]
      }

addTouchedFile :: AppState -> FilePath -> IO ()
addTouchedFile app path =
    modifyIORef (touchedFiles app) (path :)

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
runDiff :: Handle -> FilePath -> FilePath -> IO PatchData
runDiff stderr oldPath newPath = do
    (_, diffStdout, diffStderr) <-
        readProcessWithExitCode "diff" diffArgs diffInput
    hPutStr stderr diffStderr
    return diffStdout
  where
    diffInput = ""
    diffArgs = diffFlags oldPath ++ [oldPath, newPath]

-- | Show the difference between old content and new.
showDiff :: Handle -> Handle -> FilePath -> FileContent -> IO ()
showDiff stdout stderr path newContent = do
    (_, diffStdout, diffStderr) <-
        readProcessWithExitCode "diff" diffArgs (L8.unpack newContent)
    hPutStr stderr diffStderr
    hPutStr stdout diffStdout
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
updateFileContent :: Handle -> FilePath -> FilePath -> FileContent -> IO ()
updateFileContent stderr patchPath path newContent =
    withSystemTempFile "gsub.tmp" $ \tempPath tempH -> do
        L8.hPut tempH newContent
        hClose tempH
        patch <- runDiff stderr path tempPath
        appendFile patchPath patch
        copyFile tempPath path

-- | Processes a single file.  Returns (Just path) if the file was
-- modified.
processSingleFile :: Handle
                  -> Handle
                  -> Plan
                  -> FilePath
                  -> IO (Maybe FilePath)
processSingleFile stdout stderr plan path = do
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
                     updateFileContent stderr patchPath path newContent
                     return (Just path)
                 DryRunMode ->
                     return (Just path)
                 DiffMode -> do
                     showDiff stdout stderr path newContent
                     return Nothing
  where
    patchPath = patchFilePath plan

-- | Reverts a change made by a previous run.
revertPatch :: Handle -> Handle -> Plan -> IO ()
revertPatch stdout stderr plan = do
    patchData <- readFile (patchFilePath plan)
    (_, pStdout, pStderr) <-
        readProcessWithExitCode "patch" ["-R", "-p0"] patchData
    hPutStr stderr pStderr
    hPutStr stdout pStdout

-- | Processes all the files in the plan.  Returns a list of files
-- that were modified.
processFiles :: Handle -> Handle -> Plan -> IO [FilePath]
processFiles stdout stderr plan =
    case planMode plan of
        UndoMode -> do
            revertPatch stdout stderr plan
            return []
        _ ->
            fmap catMaybes $
            forM (filesToProcess plan)
            (processSingleFile stdout stderr plan)

main :: Handle -> Handle -> IO ()
main stdout stderr = do
    handle printError $ do
        opts <- execParseArgs
        plan <- makePlan opts
        app <- AppState <$> newIORef [] <*> newIORef []
        errors <- validateFiles plan
        exitIfErrors stderr errors
        touchedFiles <- processFiles stdout stderr plan
        forM_ touchedFiles $ hPutStrLn stdout
  where
    printError (ErrorCall msg) = do
        name <- getProgName
        hPutStrLn stderr (name ++ ": " ++ msg)
        exitWith (ExitFailure 1)
