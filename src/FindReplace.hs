module FindReplace (parseReplacement, test) where

import Data.Char (isDigit)
import Data.Text.ICU
import TestUtils

data ReplacementPart = LiteralPart Char | GroupPart Int deriving Eq
newtype Replacement = Replacement { parts :: [ReplacementPart] } deriving Eq

parseReplacement :: String -> Replacement

parseReplacement "" = Replacement []

parseReplacement ('\\':'&':cs) = parseReplacement cs

parseReplacement ('\\':'\\':cs) =
    Replacement $ (LiteralPart '\\') : parts (parseReplacement cs)

parseReplacement ('\\':cs) =
    case reads cs of
        [(num, rest)] -> Replacement $ (GroupPart num) : parts (parseReplacement rest)
        _ -> parseReplacement ("\\\\" ++ cs)

parseReplacement (c:cs) = Replacement $ (LiteralPart c) : parts (parseReplacement cs)

-- Unit Tests

instance Arbitrary ReplacementPart where
    arbitrary = do
        c <- arbitrary
        (Positive (Small n)) <- arbitrary
        elements [LiteralPart c, GroupPart n, LiteralPart '\\']

instance Show Replacement where
    show (Replacement []) = ""
    show (Replacement (part:parts)) = case part of
        LiteralPart '\\' -> "\\\\" ++ showParts
        LiteralPart c -> c : showParts
        GroupPart n -> "\\" ++ show n ++ maybeAmp ++ showParts where
            maybeAmp = case parts of
                (LiteralPart digit):parts' | isDigit digit -> "\\&"
                _ -> ""
        where
            showParts = show (Replacement parts)

instance Arbitrary Replacement where
    arbitrary = do
        parts <- listOf arbitrary
        return $ Replacement parts

prop_parseReplacement r =
    parseReplacement (show r) == r

return []
test = $forAllProperties quickCheckProp