module FindReplace where

import Control.Monad (liftM, liftM2)
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
--   \\ -> A literal backslash.
parseReplacement :: String -> Either String Replacement
parseReplacement s = loop 0 s
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
