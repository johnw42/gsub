module FindReplace where

import Control.Exception
import Control.Monad (liftM, liftM2)
import Data.Char (isDigit)
import Numeric (readDec)

data ReplacementPart = LiteralPart String | GroupPart Int deriving (Show, Eq)
data Replacement = Rep
    { repParts :: [ReplacementPart]
    , repMaxGroup :: Int
    }

makeReplacement :: [ReplacementPart] -> Replacement
makeReplacement parts = Rep parts maxGroup
  where
    maxGroup = maximum $ map partGroupNum parts
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
expand :: [String] -> Replacement -> String
expand groups (Rep parts maxG) =
    assert (not $ null $ drop maxG groups) $
    expand' groups parts

expand' :: [String] -> [ReplacementPart] -> String
expand' groups parts = concatMap expandGroup parts
  where
    expandGroup (LiteralPart s) = s
    expandGroup (GroupPart n) = groups !! n
