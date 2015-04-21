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
import System.IO
import System.Process (readProcess)

type PatchData = String

data FileError = FileError FilePath Error

instance Show FileError where
    show (FileError path error) = path ++ ": " ++ error

data AppState = AppState {
    fileErrors :: IORef [FileError],
    touchedFiles :: IORef [FilePath]
    }

addTouchedFile :: AppState -> FilePath -> IO ()
addTouchedFile app path =
    modifyIORef (touchedFiles app) (path :)

addFileError :: AppState -> FilePath -> Error -> IO ()
addFileError app p e =
    modifyIORef (fileErrors app) (FileError p e :)

-- | Prints errors and if there are any, otherwise executes an action.
exitIfErrors :: AppState -> IO ()
exitIfErrors app = do
    errors <- readIORef (fileErrors app)
    unless (null errors) $ do
        mapM_ print (reverse errors)
        exitWith (ExitFailure 1)

-- | Tests whether a file can be operated on.  Adds an error if it
-- can't be.
checkFile :: AppState -> FilePath -> IO ()
checkFile app path = do
    problem <- loop checks
    when (isJust problem) $
        addFileError app path (fromJust problem)
    
  where
    loop [] = return Nothing
    loop (c:cs) = do
        problem <- c path
        case problem of
            Nothing -> loop cs
            _ -> return problem

    checks = [
        check "is a directory" $ fmap not . doesDirectoryExist,
        check "no such file" $ doesFileExist,
        check "not readable" $ fmap readable . getPermissions,
        check "not writable" $ fmap writable . getPermissions
        ]

    check problem test path = do
        ok <- test path
        if ok
            then return Nothing
            else return (Just problem)

-- | Checks that all files in the plan can be operated upon.
validateFiles :: AppState -> Plan -> IO ()
validateFiles app plan =
    mapM_ (checkFile app) (filesToProcess plan)

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
    transform = transformLine $ transformation plan

-- | Finds the flags that should be passed to @diff@.
diffFlags :: FilePath -> [String]
diffFlags path = [
    "-u",
    "--label=" ++ path,
    "--label=" ++ path,
    "--"
    ]
    
-- | Runs the external diff tool over a pair of files.
runDiff :: FilePath -> FilePath -> IO PatchData
runDiff oldPath newPath = do
    readProcess "diff" diffArgs diffInput
  where
    diffInput = ""
    diffArgs = diffFlags oldPath ++ [oldPath, newPath]

-- | Show the difference between old content and new.
showDiff :: FilePath -> FileContent -> IO ()
showDiff path newContent = do
    patch <- readProcess "diff" diffArgs (L8.unpack newContent)
    putStr patch
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

-- | Processes a single file.
processSingleFile :: Plan -> FilePath -> IO ()
processSingleFile plan path = do
    oldContent <- L8.readFile path
    let newContent = transformFileContent plan oldContent
    unless (newContent == oldContent) $
        case planMode plan of
            RunMode -> do
                putStrLn path
                updateFileContent patchPath path newContent
            DryRunMode ->
                putStrLn path
  where
    patchPath = patchFilePath plan

-- | Processes all the files in the plan.
processFiles :: Plan -> IO ()
processFiles plan = do
    forM_ (filesToProcess plan) (processSingleFile plan)

main :: IO ()
main = do
    handle printError $ do
        opts <- execParseArgs
        plan <- makePlan opts
        app <- AppState <$> newIORef [] <*> newIORef []
        validateFiles app plan
        exitIfErrors app
        processFiles plan
        exitIfErrors app
  where
    printError (ErrorCall e) = do
        name <- getProgName
        putStrLn (name ++ ": " ++ e)
        exitWith (ExitFailure 2)
