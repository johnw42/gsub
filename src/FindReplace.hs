{-# LANGUAGE TypeSynonymInstances #-}
module FindReplace (parseReplacement, test) where

import Data.Char (isDigit)
import Data.Text.ICU
import TestUtils


data ReplacementPart = LiteralPart Char | GroupPart Int deriving Eq
type Replacement = [ReplacementPart]

parseReplacement :: String -> Replacement

parseReplacement "" = []

parseReplacement ('\\':'&':cs) = parseReplacement cs

parseReplacement ('\\':'\\':cs) =
    (LiteralPart '\\') : parseReplacement cs

parseReplacement ('\\':cs) =
    case reads cs of
        [(num, rest)] -> (GroupPart num) : parseReplacement rest
        _ -> parseReplacement ("\\\\" ++ cs)

parseReplacement (c:cs) = (LiteralPart c) : parseReplacement cs

-- Unit Tests

instance Arbitrary ReplacementPart where
    arbitrary = do
        c <- arbitrary
        (Positive (Small n)) <- arbitrary
        elements [LiteralPart c, GroupPart n, LiteralPart '\\']

instance Show Replacement where
    show [] = ""
    show (part:parts) = case part of
        LiteralPart '\\' -> "\\\\" ++ showParts
        LiteralPart c -> c : showParts
        GroupPart n -> "\\" ++ show n ++ maybeAmp ++ showParts where
            maybeAmp = case parts of
                (LiteralPart digit):parts' | isDigit digit -> "\\&"
                _ -> ""
        where
            showParts = show parts

instance Arbitrary Replacement where
    arbitrary = listOf arbitrary

prop_parseReplacement r =
    parseReplacement (show r) == r

return []
test = $forAllProperties quickCheckProp