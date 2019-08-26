 module Gsub where

import DiffDB
import FindReplace
import Options
import Plan
import Types

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Either
import           Data.IORef
import           Data.List (init, isPrefixOf, mapAccumL, tails)
import           Data.Maybe (catMaybes, fromJust, isJust)
import           Data.Monoid (First (..), mconcat)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import qualified Text.Regex.PCRE.Heavy as RE
import qualified Text.Regex.PCRE.Light as RE

type PatchData = String

data FileError = FileError FilePath Error

data LineWithContext = LWC
                       LineContent   -- ^ The line itself.
                       [LineContent] -- ^ Lines before this line.
                       [LineContent] -- ^ Lines after this line.

instance Show FileError where
    show (FileError path error) = path ++ ": " ++ error

-- | Prints errors if there are any, otherwise executes an action.
exitIfErrors :: [FileError] -> IO ()
exitIfErrors errors = do
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
        , check "open in emacs" $ fmap not . testIsOpenInEmacs
        ]

    check problem test = do
        ok <- test path
        if ok
            then return Nothing
            else return (Just (FileError path problem))

-- Test whether a file appears to be open in emacs.
testIsOpenInEmacs :: FilePath -> IO Bool
testIsOpenInEmacs p = do
    doesFileExist $ "tmp" ++ map replaceSlash p
  where
    replaceSlash '/' = '!'
    replaceSlash c   = c


-- | Checks that all files in the plan can be operated upon.
validateFiles :: Plan -> IO [FileError]
validateFiles plan =
    fmap catMaybes $ mapM checkFile (filesToProcess plan)

maybeTransformLine :: Plan -> LineWithContext -> LineContent
maybeTransformLine plan lwc@(LWC line linesBefore linesAfter) =
    if contextOk
        then transformLine trans line
        else line
  where
    trans = transformation plan
    pats = contextPatterns plan
    pats' = notContextPatterns plan
    relevantContext =
        [line]
        ++ take (contextBefore plan) linesBefore
        ++ take (contextAfter plan) linesAfter
    contextOk =
        (null pats || any (\pat -> any (RE.=~ pat) relevantContext) pats) &&
        (null pats' || not (any (\pat -> not $ any (RE.=~ pat) relevantContext) pats'))

-- | Applies the specified transformation to a line of a file.
transformLine :: Transformation -> LineContent -> LineContent
transformLine (TransformFixed ch needle rep) =
    B8.pack . transformLineFixed ch needle rep . B8.unpack
transformLine (TransformRegex regex rep) =
    transformLineRegex regex rep

-- Converts a file's content to a series of lines in a way that preserves the
-- presence or absence of a trailing newline.
fileContentToLines :: FileContent -> [LineContent]
fileContentToLines = map L8.toStrict . L8.split '\n'

-- Inverse of 'fileContentToLines'.
linesToFileContent :: [LineContent] -> FileContent
linesToFileContent = L8.intercalate (L8.pack "\n") . map L8.fromStrict

-- Attach context to each line.
addLineContext :: [LineContent] -> [LineWithContext]
addLineContext lines = snd $ mapAccumL step [] $ init $ tails lines
  where
    step before (line:after) = (line:before, LWC line before after)

-- | Applies the specified transformation to a whole file's content.
-- Returns a pair of the modified content and the number of lines
-- changed.
transformFileContent :: Plan -> FileContent -> (Int, FileContent)
transformFileContent plan content = (numChanges, linesToFileContent lines')
  where
    lwcs = addLineContext $ fileContentToLines content
    (numChanges, lines') = mapAccumL step 0 lwcs
    step accum lwc@(LWC line _ _) = (accum', line')
      where
        line' = maybeTransformLine plan lwc
        accum'
            | line' == line = accum
            | otherwise     = 1 + accum

-- | Finds the flags that should be passed to @diff@.
diffFlags :: FilePath -> [String]
diffFlags path =
    [ "-u"
    , "--label=" ++ path
    , "--label=" ++ path
    , "--"
    ]

-- | Runs the external diff tool over a pair of files.  Returns the
-- output of diff.
getDiffOutput :: FilePath -> FilePath -> IO PatchData
getDiffOutput oldPath newPath = do
    (_, diffStdout, diffStderr) <-
        readProcessWithExitCode "diff" diffArgs diffInput
    unless (null diffStderr) $
        error ("diff wrote to stderr:\n" ++ diffStderr)
    return diffStdout
  where
    diffInput = ""
    diffArgs = diffFlags oldPath ++ [oldPath, newPath]

-- Runs diff to show the differences between the old and new content
-- to stdout.
runDiff :: FilePath -> FileContent -> IO ()
runDiff path newContent = do
    (_, diffStdout, diffStderr) <-
        readProcessWithExitCode "diff" diffArgs (L8.unpack newContent)
    unless (null diffStderr) $
        error ("diff wrote to stderr:\n" ++ diffStderr)
    putStr diffStdout
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
        patch <- getDiffOutput path tempPath
        appendFile patchPath patch
        copyFile tempPath path

-- | Processes a single file.  Returns the number of lines changed in
-- this file.
processSingleFile :: Plan -> FilePath -> IO Int
processSingleFile plan path = do
    oldContent <- L8.readFile path
    let (numChanges, newContent) =
            transformFileContent plan oldContent
    when (numChanges > 0) $
        case planMode plan of
        RunMode -> do
            updateFileContent
                (fromJust patchPath) path newContent
            printChanges numChanges
        DryRunMode ->
            printChanges numChanges
        DiffMode -> do
            runDiff path newContent
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
        when (filesChanged > 0) $
            putStrLn ("Diff saved in " ++
                      fromJust (patchFilePath plan))
    DryRunMode -> do
        putStrLn ("Would have changed " ++
                  showLineCount linesChanged ++ " in " ++
                  showFileCount filesChanged)
        when (filesChanged > 0) $
            putStrLn ("Diff would have been saved in " ++
                      fromJust (patchFilePath plan))
    DiffMode -> return ()

main :: IO ()
main = do
    handle printError $ do
        opts <- execParseArgs
        plan <- makePlan opts
        errors <- validateFiles plan
        exitIfErrors errors
        changes <- processFiles plan
        printSummary plan changes
  where
    printError (ErrorCall msg) = do
        name <- getProgName
        hPutStrLn stderr (name ++ ": " ++ msg)
        exitWith (ExitFailure 1)
