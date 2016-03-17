module Types
( FileContent
, LineContent
) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

type FileContent = L8.ByteString
type LineContent = B8.ByteString
