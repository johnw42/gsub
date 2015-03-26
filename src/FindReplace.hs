module FindReplace (Replacement, literalReplacement, parseReplacement, test) where

import TestUtils

import Control.Monad (liftM)
import Data.Char (isDigit)
import Data.List (inits, tails)
import Numeric (readDec)

-- A regexp replacement string.
data ReplacementPart = LiteralPart String | GroupPart Int deriving (Show, Eq)
type Replacement = [ReplacementPart]

literalReplacement :: String -> Replacement
literalReplacement s = [LiteralPart s]

-- Parse a replacement string into a data structure, with the following
-- escape sequences:
--   \n -> Match group n, where n >= 0
--   \& -> The empty string.
--   \c -> The literal character c, where c is any character except a digit or &
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
    | otherwise = (LiteralPart "\\") : parseReplacement (c:cs)
parseReplacement (c:cs) = mergeLiterals $ LiteralPart [c] : parseReplacement cs

-- Substitute values into a replacement.
expand :: [String] -> Replacement -> Either String String
expand groups parts = liftM concat $ sequence $ map expandGroup parts
    where
        expandGroup (LiteralPart s) = Right s
        expandGroup (GroupPart n) = case drop n groups of
            (g:_) -> Right g
            _ -> Left $ "no such group: " ++ show n

prop_expand before after (NonNegative (Small n))  (Positive (Small k)) =
    (label "The empty replacement works.")
    (once $ expand testGroups [] ==? Right "") .&&.
    (label "Valid groups are replaced.")
    (expand testGroups replacement ==? Right expected) .&&.
    (label "Invalid group numbers trigger and error.")
    (expand (take n testGroups) replacement ==? Left ("no such group: " ++ show n'))
    where
        n' = n + k
        expected = before ++ (testGroups !! n') ++ after
        replacement = [LiteralPart before, GroupPart n', LiteralPart after]
        testGroups = ["<group" ++ (show n) ++ ">" | n <- [0..]]

-- Normalize a replacement sequence by combining adjacent LiteralParts
-- and merging adjacent LiteralParts.
mergeLiterals :: Replacement -> Replacement
mergeLiterals [] = []
mergeLiterals (LiteralPart a : LiteralPart b : parts) = mergeLiterals (LiteralPart (a ++ b) : parts)
mergeLiterals (LiteralPart "" : parts) = mergeLiterals parts
mergeLiterals (part : parts) = part : mergeLiterals parts

prop_mergeLiterals1 pa pb =
    merged ==? case parts of
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

-- Test support:

instance Arbitrary ReplacementPart where
    arbitrary = sized $ \size -> do
        s <- arbitrary
        n <- choose (0, size)
        elements [LiteralPart s, GroupPart n, LiteralPart "\\"]
    shrink (LiteralPart s) = map LiteralPart (shrink s)
    shrink (GroupPart n) = map GroupPart [n-1, n-2 .. 0]

-- Convert a replacement sequence into a parseable representation.
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
    once $ apEq parseReplacement "\\0.1" [GroupPart 0,LiteralPart ".1"]

return []
test = do
--    quickCheckWithResult stdArgs { maxSuccess = 10000 }
--        $ forAll arbitrary prop_parseReplacement
    $forAllProperties quickCheckResult