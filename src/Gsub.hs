module Gsub where

import DiffDB
import FindReplace
import Options
import Plan

import Control.Applicative ((<*>))
import Control.Exception
import Control.Monad (foldM, forM, liftM, unless, when)
import Control.Monad.IO.Class
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Monoid (First(..), mconcat)
import System.Directory
import System.IO
import System.Process (readProcess)

import qualified Text.Regex.PCRE.Heavy as Heavy

data FileError = FileError FilePath Error

instance Show FileError where
    show (FileError path error) = path ++ ": " ++ error

data AppState = AppState {
    appErrors :: [FileError]
    }

type App a = StateT AppState IO a

addAppError :: FilePath -> Error -> App ()
addAppError p e = do
    app <- get
    put app { appErrors = FileError p e : appErrors app }

-- | Prints errors and if there are any, otherwise executes an action.
unlessErrors :: App () -> App ()
unlessErrors action = do
    app <- get
    let errors = appErrors app
    if null errors
        then mapM_ (liftIO . print) (reverse errors)
        else action

-- | Tests whether a file can be operated on.  Adds an error if it
-- can't be.
checkFile :: FilePath -> App ()
checkFile path = do
    problem <- liftIO $ loop checks
    when (isJust problem) $
        addAppError path (fromJust problem)
    
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
validateFiles :: Plan -> App ()
validateFiles plan =
    mapM_ checkFile (filesToProcess plan)

-- | Applies the specified transformation to a line of a file.
transformLine :: Transformation -> String -> Either Error String
transformLine t@(TransformFixed ch needle rep) line =
    Right (loop line)
  where
    loop "" = ""
    loop cs@(c:cs')
        | withCase needle `isPrefixOf` withCase cs =
              rep ++ loop (drop (length needle) cs)
        | otherwise = c : loop cs'
    withCase = case ch of
        IgnoreCase -> map toLower
        ConsiderCase -> id
transformLine (TransformRegex regex rep) line =
    foldr eachMatch (Right line) matchRanges
  where
    matchRanges = Heavy.scanRanges regex line
    eachMatch _ (Left e) = Left e
    eachMatch ((i,j),subs) (Right s) =
        case expand groups rep of
            Left e -> Left e
            Right expansion ->
                Right (prefix ++ expansion ++ suffix)
      where
        prefix = take i s
        suffix = drop j s
        groups = map rangeToText groupRanges
        groupRanges = (i,j):subs
        rangeToText (i,j) = drop i (take j line)

-- | Applies the specified transformation to a whole file's content.
transformFileContent :: Plan -> String -> Either Error String
transformFileContent plan text = do
    let ls = lines text
    tls <- forM ls (transformLine $ transformation plan)
    return $ unlines tls
    
type PatchData = String

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
    liftIO $ bracket open closeAndDelete (uncurry f)
  where
    open = liftIO $ openTempFile dir template
    closeAndDelete (path, handle) = do
        finally
            (liftIO $ hClose handle)
            (liftIO $ removeFile path)

-- | Similar to 'withTempFile', but hardcoded to use the system
-- temporary directory.
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile template f = do
    dir <- getTemporaryDirectory
    withTempFile dir template f

-- | Processes a single file.
processSingleFile :: Plan -> FilePath -> App PatchData
processSingleFile plan path = do
    diffPath <- liftIO $ getDiffPath (patchFilePath plan)
    oldContent <- liftIO $ readFile path
    case transformFileContent plan oldContent of
        Left e -> do
            addAppError path e
            return ""
        Right newContent ->
            liftIO $ withSystemTempFile "gsub.tmp" $
            \tempPath tempH -> do
                hPutStr tempH newContent
                hClose tempH
                patch <- runDiff path tempPath
                unless (null patch) $ do
                    copyFile tempPath path
                return patch

-- | Processes all the files in the plan.
processFiles :: Plan -> App ()
processFiles plan = do
    makePatches >>= writePatches (patchFilePath plan)
  where
    makePatches :: App [PatchData]
    makePatches = forM (filesToProcess plan) (processSingleFile plan)
    writePatches patchPath patchParts =
        liftIO $ writeFile patchPath (concat patchParts)

appMain :: App ()
appMain = do
    planOrError <- liftIO execParseArgsToPlan
    case planOrError of
        Left error -> liftIO $ putStrLn error
        Right plan -> do
            validateFiles plan
            unlessErrors $ do
                processFiles plan
                unlessErrors $ return ()

main = evalStateT appMain initAppState
  where
    initAppState = AppState []
