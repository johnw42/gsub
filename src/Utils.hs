{-# LANGUAGE TemplateHaskell #-}
module Utils where

import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Monoid (First(..), mconcat)

-- Find the first Just in a list.
firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . mconcat . map First
