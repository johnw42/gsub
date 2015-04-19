module FindReplace where

import Control.Exception
import Control.Monad (liftM, liftM2)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.List
import Numeric (readDec)
import qualified Text.Regex.PCRE.Heavy as Heavy

type FileContent = L8.ByteString

data CaseHandling = IgnoreCase | ConsiderCase deriving Show

data ReplacementPart = LiteralPart String | GroupPart Int deriving (Show, Eq)
data Replacement = Rep
    { repParts :: [ReplacementPart]
    , repMaxGroup :: Int
    }

makeReplacement :: [ReplacementPart] -> Replacement
makeReplacement parts = Rep parts maxGroup
  where
    maxGroup = maximum $ 0 : map partGroupNum parts
    partGroupNum (GroupPart n) = n
    partGroupNum _ = 0

literalReplacement :: String -> Replacement
literalReplacement "" = makeReplacement []
literalReplacement s = makeReplacement [LiteralPart s]

-- Parse a replacement string into a data structure, with the following
-- escape sequences:
--   \n -> Match group n, where n >= 0
--   \& -> The empty string.
--   \\ -> A literal backslash.
parseReplacement :: String -> Either String Replacement
parseReplacement = liftM makeReplacement . parseReplacement'

parseReplacement' :: String -> Either String [ReplacementPart]
parseReplacement' s = loop 0 s
  where
    loop _ "" = Right []
    loop offset "\\" =
        Left ("unterminated escape sequence at offset " ++ show offset)
    loop offset ('\\':'&':cs) = loop (offset+2) cs
    loop offset ('\\':'\\':cs) = consLiteral "\\" (loop (offset+2) cs)
    loop offset ('\\':c:cs) = case readDec (c:cs) of
        ((n, cs'):_) ->
            let offset' = offset + length cs - length cs'
            in cons (GroupPart n) (loop offset' cs')
        _ -> Left ("invalid escape sequence at offset " ++ show offset)
    loop offset (c:cs) = consLiteral [c] (loop (offset+1) cs)
    cons part parts = liftM2 (:) (Right $ part) parts
    consLiteral s parts =
        liftM mergeLiterals $ cons (LiteralPart s) parts
    
-- Normalize a replacement sequence by combining adjacent LiteralParts.
mergeLiterals :: [ReplacementPart] -> [ReplacementPart]
mergeLiterals = loop
  where
    loop [] = []
    loop (LiteralPart "" : parts) = loop parts
    loop (LiteralPart a : LiteralPart b : parts) =
        loop (LiteralPart (a ++ b) : parts)
    loop (part : parts) = part : loop parts

-- Substitute values into a replacement.
expand :: Replacement -> [String] -> String
expand (Rep parts maxG) groups =
    assert (not $ null $ drop maxG groups) $
    expand' parts groups

expand' :: [ReplacementPart] -> [String] -> String
expand' parts groups = concatMap expandGroup parts
  where
    expandGroup (LiteralPart s) = s
    expandGroup (GroupPart n) = groups !! n

-- | Transforms a line using fixed strings.
transformLineFixed
    :: CaseHandling
    -> String  -- ^ Pattern string.
    -> String  -- ^ Replacement string.
    -> String  -- ^ String to replace in.
    -> String
transformLineFixed ch needle rep line = loop line
  where
    loop "" = ""
    loop cs@(c:cs')
        | withCase needle `isPrefixOf` withCase cs =
              rep ++ loop (drop (length needle) cs)
        | otherwise = c : loop cs'
    withCase = case ch of
        -- Use of uppercase to significant because
        -- toLower (toUpper '\181') == '\956' !
        IgnoreCase -> map toUpper
        ConsiderCase -> id

-- | Transforms a line using regex replacement.
transformLineRegex :: Heavy.Regex -> Replacement -> FileContent -> FileContent
transformLineRegex regex rep line =
    Heavy.gsub regex (expand rep) line
