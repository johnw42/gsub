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
import Data.List (isPrefixOf, mapAccumL)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Monoid (First(..), mconcat)
import System.Directory
import System.Environment
import System.Exit
import System.IO
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
-- Returns a pair of the modified content and the number of lines
-- changed.
transformFileContent :: Plan -> FileContent -> (Int, FileContent)
transformFileContent plan content = (numChanges, L8.unlines lines')
  where
    lines = L8.lines content
    (numChanges, lines') = mapAccumL step 0 lines
    step accum line = (accum', line')
      where
        line' = transform line
        accum'
            | line' == line = accum
            | otherwise     = 1 + accum
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

-- | Processes a single file.  Returns the number of lines changed in
-- this file.
processSingleFile :: Plan -> FilePath -> IO Int
processSingleFile plan path = do
    oldContent <- L8.readFile path
    let (numChanges, newContent) = transformFileContent plan oldContent
    when (numChanges > 0) $
        case planMode plan of
        RunMode -> do
            updateFileContent
                (fromJust patchPath) path newContent
            printChanges numChanges
        DryRunMode ->
            printChanges numChanges
        DiffMode -> do
            diff <- showDiff path newContent
            putStr diff
    return numChanges
  where
    patchPath = patchFilePath plan
    printChanges n =
        putStrLn (path ++ ": " ++ showLineCount n ++ " changed")

-- Make a phrase showing a count of things.
showCount :: String -- the thing (singular)
          -> String -- the things (plural)
          -> Int    -- the number of things
          -> String
showCount sing plural n =
    show n ++ " " ++ (if n == 1 then sing else plural)

showLineCount = showCount "line" "lines"

showFileCount = showCount "file" "files"

-- A tuple of the count of changed files and the count of changed
-- lines in those files.
type ChangeCounts = (Int,Int)

-- | Processes all the files in the plan.  Returns the number of files
-- changed and the number of lines changed.
processFiles :: Plan -> IO ChangeCounts
processFiles plan = do
    lineCounts <- forM (filesToProcess plan) (processSingleFile plan)
    let filesChanged = length $ filter (>0) lineCounts
        linesChanged = sum lineCounts
    return (filesChanged, linesChanged)

-- Prints a summary of what the program did.
printSummary :: Plan -> ChangeCounts -> IO ()
printSummary plan (filesChanged, linesChanged) =
    case planMode plan of
    RunMode -> do
        putStrLn ("Changed " ++
                  showLineCount linesChanged ++ " in " ++
                  showFileCount filesChanged)
        putStrLn ("Diff saved in " ++
                  fromJust (patchFilePath plan))
    DryRunMode -> do
        putStrLn ("Would have changed " ++
                  showLineCount linesChanged ++ " in " ++
                  showFileCount filesChanged)
        putStrLn ("Diff would have been saved in " ++
                  fromJust (patchFilePath plan))
    DiffMode -> return ()

main :: IO ()
main = do
    handle printError $ do
        opts <- execParseArgs
        plan <- makePlan opts
        errors <- validateFiles plan
        exitIfErrors stderr errors
        changes <- processFiles plan
        printSummary plan changes
  where
    printError (ErrorCall msg) = do
        name <- getProgName
        hPutStrLn stderr (name ++ ": " ++ msg)
        exitWith (ExitFailure 1)
