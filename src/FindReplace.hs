module FindReplace (parseReplacement, test) where

import Data.Char (isDigit)
import Data.List (inits, tails)
import Data.Text.ICU
import TestUtils


data ReplacementPart = LiteralPart String | GroupPart Int deriving (Show, Eq)
type Replacement = [ReplacementPart]

parseReplacement :: String -> Replacement

parseReplacement "" = []

parseReplacement ('\\':'&':cs) = parseReplacement cs

parseReplacement ('\\':'\\':cs) =
    mergeLiterals $ LiteralPart "\\" : parseReplacement cs

parseReplacement ('\\':cs) =
    case reads cs of
        [(num, rest)] -> GroupPart num : parseReplacement rest
        _ -> parseReplacement ("\\\\" ++ cs)

parseReplacement (c:cs) = mergeLiterals $ LiteralPart [c] : parseReplacement cs

mergeLiterals :: Replacement -> Replacement
mergeLiterals [] = []
mergeLiterals (LiteralPart a : LiteralPart b : parts) = mergeLiterals (LiteralPart (a ++ b) : parts)
mergeLiterals (LiteralPart "" : parts) = mergeLiterals parts
mergeLiterals (part : parts) = part : mergeLiterals parts

prop_mergeLiterals1 pa pb =
    counterexample (show merged) $
    merged == case parts of
        [LiteralPart "", other] -> [other]
        [other, LiteralPart ""] -> [other]
        [LiteralPart a, LiteralPart b] -> [LiteralPart (a ++ b)]
        _ -> parts
    where
        parts = [pa, pb]
        merged = mergeLiterals parts

prop_mergeLiterals2 parts =
    all check (tails $ mergeLiterals parts)
    where
        check [] = True
        check [_] = True
        check (a:b:_) = not (isLiteral a && isLiteral b)
        isLiteral (LiteralPart _) = True
        isLiteral _ = False

-- Unit Tests

instance Arbitrary ReplacementPart where
    arbitrary = do
        s <- arbitrary
        (Positive (Small n)) <- arbitrary
        elements [LiteralPart s, GroupPart n, LiteralPart "\\"]

showReplacement = concatMap showPart . tails
    where
        showPart [] = ""
        showPart (LiteralPart s : _) = showLiteral s
        showPart (GroupPart n : after) = "\\" ++ show n ++ maybeAmp after
        maybeAmp (LiteralPart (digit : _) : _)
            | isDigit digit = "\\&"
        maybeAmp _ = ""
        showLiteral = concatMap showLiteralChar
        showLiteralChar '\\' = "\\\\"
        showLiteralChar c = [c]

instance Arbitrary Replacement where
    arbitrary = listOf arbitrary

prop_parseReplacement r =
    within 100000 $
    let r' = mergeLiterals r
        shown = showReplacement r'
        parsed = parseReplacement shown
    in counterexample ("r'     " ++ show r') $
       counterexample ("shown  " ++ show shown) $
       counterexample ("parsed " ++ show parsed) $
       parsed == r'

return []
test = $forAllProperties quickCheckProp