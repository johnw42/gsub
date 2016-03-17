{-# LANGUAGE TemplateHaskell #-}
module Utils where

import Data.Maybe (catMaybes, listToMaybe)

-- Find the first Just in a list.
firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes
