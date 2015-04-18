module FindReplaceTest (tests) where

import FindReplace
import TestUtils ((==?), apEq)

import Data.Char (isDigit)
import Data.List (tails)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

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

prop_expand before after =
  (forAll (sized $ \size -> choose (0, size)) $ \n ->
    (forAll (sized $ \size -> choose (1, size)) $ \k ->
      inner n k))
  where
    testGroups = ["<group" ++ (show n) ++ ">" | n <- [0..]]
    inner n k =
        (label "The empty replacement works.")
        (once $ expand' [] testGroups ==? "") .&&.
        (label "Valid groups are replaced.")
        (expand' replacement testGroups ==? expected)
      where
        n' = n + k
        expected = before ++ (testGroups !! n') ++ after
        replacement = [LiteralPart before, GroupPart n', LiteralPart after]


instance Arbitrary ReplacementPart where
    arbitrary = sized $ \size -> do
        s <- arbitrary
        n <- choose (1, size)
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
        parsed = parseReplacement' shown
    in printTestCase ("r'     " ++ show r') $
       printTestCase ("shown  " ++ show shown) $
       printTestCase ("parsed " ++ show parsed) $
       parsed == Right r'

case_parseReplacement0 =
    parseReplacement' "\\0.1" @?= Right [GroupPart 0, LiteralPart ".1"]

tests = testGroup "FindReplace" [
  testProperty "mergeLiterals1" prop_mergeLiterals1,
  testProperty "mergeLiterals2" prop_mergeLiterals2,
  testProperty "expand" prop_expand,
  testProperty "parseReplacement" prop_parseReplacement,
  testCase "parseReplacement0" case_parseReplacement0
  ]
