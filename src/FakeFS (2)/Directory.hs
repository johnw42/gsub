module FakeFS.Directory
    ( doesFileExist
    , doesDirectoryExist
    , getPermissions
    , Real.Permissions(..)
    ) where

import qualified System.Directory as Real

doesFileExist :: FilePath -> IO Bool
doesFileExist _ = return False

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist _ = return False

getPermissions :: FilePath -> IO Real.Permissions
getPermissions _ = return Real.emptyPermissions