{-# LANGUAGE OverloadedStrings #-}

module Gsub where

import DiffDB
import Options
import Plan

import Control.Exception
import Control.Monad (foldM, forM, liftM, unless, when)
import Control.Monad.IO.Class
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Monoid (First(..), mconcat)
import System.Directory
import System.IO
import System.Process (readProcess)

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

-- | Prints errors and returns True if there were no errors.
printAppErrors :: App Bool
printAppErrors = do
    app <- get
    mapM_ (liftIO . print) (reverse $ appErrors app)
    return $ null $ appErrors app

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
transformLine :: Transformation -> String -> String
transformLine t@(TransformFixed needle rep) line = loop line
  where
    loop "" = ""
    loop cs@(c:cs')
        | needle `isPrefixOf` cs =
              rep ++ loop (drop (length needle) cs)
        | otherwise = c : loop cs'
transformLine (TransformRegex regex rep) line = undefined

-- | Applies the specified transformation to a whole file's content.
transformFileContent :: Plan -> String -> String
transformFileContent plan =
    unlines . map (transformLine $ transformation plan) . lines

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
processSingleFile plan path = liftIO $ do
    diffPath <- getDiffPath (patchFilePath plan)
    oldContent <- readFile path
    withSystemTempFile "gsub.tmp" $
        \tempPath tempH -> do
            hPutStr tempH (transformFileContent plan oldContent)
            hFlush tempH
            runDiff path tempPath

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
            ok <- printAppErrors
            when ok $ do
                processFiles plan
                printAppErrors
                return ()

main = evalStateT appMain initAppState
  where
    initAppState = AppState []
