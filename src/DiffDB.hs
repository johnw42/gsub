module DiffDB where

import System.Directory
import System.FilePath

getDiffPath :: String -> IO FilePath
getDiffPath s = do
    dir <- getTemporaryDirectory
    return $ dir </> ("gsub-" ++ s ++ ".diff")
