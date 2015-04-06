module FindReplace where

import Data.Char (isDigit)
import Data.List (inits, tails)
import Data.Text.ICU
import Numeric (readDec)

data ReplacementPart = LiteralPart String | GroupPart Int deriving (Show, Eq)
type Replacement = [ReplacementPart]

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

mergeLiterals :: Replacement -> Replacement
mergeLiterals [] = []
mergeLiterals (LiteralPart a : LiteralPart b : parts) = mergeLiterals (LiteralPart (a ++ b) : parts)
mergeLiterals (LiteralPart "" : parts) = mergeLiterals parts
mergeLiterals (part : parts) = part : mergeLiterals parts
