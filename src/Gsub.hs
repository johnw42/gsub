module Gsub where

import Options
import Plan

import Control.Exception (bracket)
import Control.Monad (foldM, forM, liftM, unless, when)
import Control.Monad.IO.Class
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Monoid (First(..), mconcat)
import System.Directory (doesDirectoryExist, doesFileExist, getPermissions, getTemporaryDirectory, readable, writable)
import System.Environment (getArgs)
import System.IO (hClose, hFlush, hPutStr, openTempFile)
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

-- Print errors and return True if there were no errors.
printAppErrors :: App Bool
printAppErrors = do
    app <- get
    mapM_ (liftIO . print) (reverse $ appErrors app)
    return $ null $ appErrors app

-- Tests whether a file can be operated on.  Adds an error if it can't be.
checkFile :: FilePath -> App Bool
checkFile path = loop checks
  where
    loop :: [FilePath -> App Bool] -> App Bool
    loop [] = return True
    loop (c:cs) = do
        ok <- c path
        if not ok then return False else loop cs

    checks :: [FilePath -> App Bool]
    checks = [
        makeCheck "is a directory" $ fmap not . doesDirectoryExist,
        makeCheck "no such file" $ doesFileExist,
        makeCheck "not readable" $ fmap readable . getPermissions,
        makeCheck "not writable" $ fmap writable . getPermissions
        ]

    makeCheck reason test path = do
        ok <- liftIO $ test path
        unless ok $
            addAppError path reason
        return ok

validateFiles :: Plan -> App ()
validateFiles plan =
    mapM_ checkFile (filesToProcess plan)

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

type PatchData = String

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

processSingleFile :: Plan -> FilePath -> App PatchData
processSingleFile plan path = do
    tempDir <- liftIO getTemporaryDirectory
    oldContent <- liftIO $ readFile path
    liftIO $ bracket (openTempFile tempDir "gsub.txt") (hClose . snd) $
        \(tempPath, tempH) -> do
            hPutStr tempH (transformFileContent plan oldContent)
            hFlush tempH
            runDiff path tempPath

processFiles :: Plan -> App ()
processFiles plan = do
    makePatches >>= writePatches (patchFilePath plan)
  where
    makePatches :: App [PatchData]
    makePatches = forM (filesToProcess plan) (processSingleFile plan)
    writePatches patchPath patchParts =
        liftIO $ writeFile patchPath (concat patchParts)

-- Find the first Just in a list.
firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . mconcat . map First

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
