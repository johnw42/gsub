module Types
       ( FileContent
       , LineContent
       , Error
       , PlanMode(..)
       , ReplacementMode(..)
       , CaseHandling(..)
       ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

type FileContent = L8.ByteString
type LineContent = B8.ByteString

type Error = String

-- | Overall program mode.
data PlanMode
    = RunMode
    | DryRunMode
    | DiffMode
    deriving (Eq, Show)

-- | How to treat the match pattern.
data ReplacementMode
    = RegexMode
    | FixedMode
    deriving (Eq, Show)

-- | How letter case should be handled.
data CaseHandling
    = ConsiderCase
    | IgnoreCase
    | SmartCase
    deriving (Eq, Show)
