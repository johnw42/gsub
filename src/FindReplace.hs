module FindReplace where

import Control.Monad (liftM)
import Data.Char (isDigit)
import Numeric (readDec)

data ReplacementPart = LiteralPart String | GroupPart Int deriving (Show, Eq)
type Replacement = [ReplacementPart]

literalReplacement :: String -> Replacement
literalReplacement s = [LiteralPart s]

-- Parse a replacement string into a data structure, with the following
-- escape sequences:
--   \n -> Match group n, where n >= 0
--   \& -> The empty string.
--   \c -> The literal character c, where c is any character except a digit or
--
-- As a special case, a single \ at the end of a string is dropped.
parseReplacement :: String -> Replacement
parseReplacement "" = []
parseReplacement "\\" = []  -- Yuck!
parseReplacement ('\\':'&':cs) = parseReplacement cs
parseReplacement ('\\':'\\':cs) =
    mergeLiterals $ LiteralPart "\\" : parseReplacement cs
parseReplacement ('\\':c:cs)
    | isDigit c = case readDec (c:cs) of
        [(n, cs')] -> GroupPart n : parseReplacement cs'
        x -> error $ "parsed " ++ show (c:cs) ++ " as " ++ show x
    | otherwise = mergeLiterals $ (LiteralPart "\\") : parseReplacement (c:cs)
parseReplacement (c:cs) = mergeLiterals $ LiteralPart [c] : parseReplacement cs

-- Normalize a replacement sequence by combining adjacent LiteralParts
-- and merging adjacent LiteralParts.
mergeLiterals :: Replacement -> Replacement
mergeLiterals [] = []
mergeLiterals (LiteralPart a : LiteralPart b : parts) = mergeLiterals (LiteralPart (a ++ b) : parts)
mergeLiterals (LiteralPart "" : parts) = mergeLiterals parts
mergeLiterals (part : parts) = part : mergeLiterals parts

-- Substitute values into a replacement.
expand :: [String] -> Replacement -> Either String String
expand groups parts = liftM concat $ sequence $ map expandGroup parts
  where
    expandGroup (LiteralPart s) = Right s
    expandGroup (GroupPart n) = case drop n groups of
      (g:_) -> Right g
      _ -> Left $ "no such group: " ++ show n
