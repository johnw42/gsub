{-# LANGUAGE TemplateHaskell #-}
module FindReplace (parseReplacement, test) where

import Data.Char (isDigit)
import Data.List (inits, tails)
import Data.Text.ICU
import Numeric (readDec)
import TestUtils


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

prop_mergeLiterals1 pa pb =
    counterexample (show merged) $
    merged == case parts of
        [LiteralPart "", LiteralPart ""] -> []
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
        check [LiteralPart ""] = False
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
    shrink (LiteralPart s) = map LiteralPart (shrink s)
    shrink (GroupPart n) = map GroupPart [n-1, n-2 .. 0]

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

prop_parseReplacement r =
    let r' = mergeLiterals r
        shown = showReplacement r'
        parsed = parseReplacement shown
    in counterexample ("r'     " ++ show r') $
       counterexample ("shown  " ++ show shown) $
       counterexample ("parsed " ++ show parsed) $
       parsed == r'

prop_parseReplacement0 =
    apEq parseReplacement "\\0.1" [GroupPart 0,LiteralPart ".1"]

return []
test = do
--    quickCheckWithResult stdArgs { maxSuccess = 10000 }
--        $ forAll arbitrary prop_parseReplacement
    $forAllProperties quickCheckResult
