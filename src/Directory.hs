module Directory where

import System.Directory

doesPathExist :: FilePath -> IO Bool
doesPathExist p = do
    isFile <- doesFileExist p
    if isFile
        then return True
        else doesDirectoryExist p
