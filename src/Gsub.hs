module Gsub where

import DiffDB
import FindReplace
import Options
import Plan

import Control.Applicative
import Control.Exception
import Control.Monad (foldM, forM, liftM, unless, when)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Either
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Monoid (First(..), mconcat)
import System.Directory
import System.Exit
import System.IO
import System.Process (readProcess)

import qualified Text.Regex.PCRE.Heavy as Heavy

type FileContent = L8.ByteString
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

-- | Transforms a line using fixed strings.
transformLineFixed
    :: CaseHandling
    -> String  -- ^ Pattern string.
    -> String  -- ^ Replacement string.
    -> String  -- ^ String to replace in.
    -> String
transformLineFixed ch needle rep line = loop line
  where
    loop "" = ""
    loop cs@(c:cs')
        | withCase needle `isPrefixOf` withCase cs =
              rep ++ loop (drop (length needle) cs)
        | otherwise = c : loop cs'
    withCase = case ch of
        -- Use of uppercase to significant because
        -- toLower (toUpper '\181') == '\956' !
        IgnoreCase -> map toUpper
        ConsiderCase -> id

-- | Transforms a line using regex replacement.
transformLineRegex :: Heavy.Regex -> Replacement -> FileContent -> FileContent
transformLineRegex regex rep line =
    Heavy.gsub regex (expand rep) line

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
    
-- | Runs the external diff tool over a pair of files.
runDiff :: FilePath -> FilePath -> IO PatchData
runDiff oldPath newPath = do
    readProcess "diff" diffArgs diffInput
  where
    diffInput = ""
    diffArgs = [
        "-u",
        "--label=" ++ oldPath,
        "--label=" ++ oldPath,
        "--", oldPath, newPath
        ]

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

-- | Processes a single file.
processSingleFile :: Plan -> FilePath -> IO PatchData
processSingleFile plan path = do
    diffPath <- getDiffPath (patchFilePath plan)
    oldContent <- L8.readFile path
    let newContent = transformFileContent plan oldContent
    withSystemTempFile "gsub.tmp" $
        \tempPath tempH -> do
            L8.hPut tempH newContent
            hClose tempH
            patch <- runDiff path tempPath
            unless (null patch) $
                copyFile tempPath path
            return patch

-- | Processes all the files in the plan.
processFiles :: Plan -> IO ()
processFiles plan = do
    makePatches >>= writePatches (patchFilePath plan)
  where
    makePatches = forM (filesToProcess plan) (processSingleFile plan)
    writePatches patchPath patchParts =
        writeFile patchPath (concat patchParts)

main :: IO ()
main = do
    opts <- execParseArgs
    plan <- makePlan opts
    app <- AppState <$> newIORef [] <*> newIORef []
    validateFiles app plan
    exitIfErrors app
    processFiles plan
    exitIfErrors app
