module FindReplaceTest (tests) where

import FindReplace

import TestUtils

import Data.Char (isDigit)
import Data.List (tails)

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

prop_mergeLiterals1 pa pb =
    printTestCase (show merged) $
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

instance Arbitrary ReplacementPart where
    arbitrary = do
        s <- arbitrary
        n <- choose (1, 20)
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
    in printTestCase ("r'     " ++ show r') $
       printTestCase ("shown  " ++ show shown) $
       printTestCase ("parsed " ++ show parsed) $
       parsed == r'

prop_parseReplacement0 =
    apEq parseReplacement "\\0.1" [GroupPart 0,LiteralPart ".1"]

tests = testGroup "FindReplace" [
  testProperty "mergeLiterals1" prop_mergeLiterals1,
  testProperty "mergeLiterals2" prop_mergeLiterals2,
  testProperty "parseReplacement" prop_parseReplacement,
  testProperty "parseReplacement0" prop_parseReplacement0
  ]
