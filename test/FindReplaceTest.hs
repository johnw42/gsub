module FindReplaceTest (tests) where

import FindReplace
import TestUtils

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.List
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

instance Arbitrary CaseHandling where
    arbitrary = elements [IgnoreCase, ConsiderCase]

prop_mergeLiterals1 :: ReplacementPart
                    -> ReplacementPart
                    -> Property
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

prop_mergeLiterals2 :: [ReplacementPart] -> Bool
prop_mergeLiterals2 parts =
    all check (tails (mergeLiterals parts))
  where
    check (LiteralPart "" : _) = False
    check (LiteralPart _ : LiteralPart _ : _) = False
    check _ = True

prop_expand :: String -> String -> Property
prop_expand before after =
    forAll (sized $ \size -> choose (0, size)) $ \n ->
    forAll (sized $ \size -> choose (1, size)) $ \k ->
    inner n k
  where
    testGroups = ["<group" ++ (show n) ++ ">" | n <- [0..]]
    inner n k =
        label "The empty replacement works."
        (once $ expand (Rep []) testGroups ==? "") .&&.
        label "Valid groups are replaced."
        (expand replacement testGroups ==? expected)
      where
        n' = n + k
        expected = before ++ (testGroups !! n') ++ after
        replacement = Rep [LiteralPart before, GroupPart n', LiteralPart after]


instance Arbitrary ReplacementPart where
    arbitrary = sized $ \size -> do
        s <- arbitrary
        n <- choose (1, size)
        elements [LiteralPart s, GroupPart n, LiteralPart "\\"]
    shrink (LiteralPart s) = map LiteralPart (shrink s)
    shrink (GroupPart n) = map GroupPart [n-1, n-2 .. 0]

-- Convert a replacement sequence into a parseable representation.
showReplacement :: [ReplacementPart] -> String
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

prop_parseReplacement :: [ReplacementPart] -> Property
prop_parseReplacement r =
    let r' = mergeLiterals r
        shown = showReplacement r'
        parsed = parseReplacement shown
    in printTestCase ("r'     " ++ show r') $
       printTestCase ("shown  " ++ show shown) $
       printTestCase ("parsed " ++ show parsed) $
       parsed == Right (Rep r')

case_parseReplacement0 =
    parseReplacement "\\0.1" @?= Right (Rep [GroupPart 0, LiteralPart ".1"])

prop_transformLineFixed1 :: CaseHandling
                         -> String
                         -> String
                         -> String
                         -> String
                         -> Property
prop_transformLineFixed1 ch pattern replacement before after =
    not (pattern `isInfixOf` (replacement ++ after)) ==>
    not (pattern `isInfixOf` (before ++ replacement)) ==>
    printTestCase (show result) $
        replacement `isInfixOf` result &&
        not (pattern `isInfixOf` result)
  where
    content = before ++ pattern ++ after
    result = transformLineFixed ch pattern replacement content

prop_transformLineFixed2 :: String
                         -> String
                         -> String
                         -> String
                         -> Property
prop_transformLineFixed2 pattern replacement before after =
    not (u pattern `isInfixOf` (u $ replacement ++ after)) ==>
    not (u pattern `isInfixOf` (u $ before ++ replacement)) ==>
    printTestCase (show result) $
        replacement `isInfixOf` result &&
        not (pattern `isInfixOf` result)
  where
    content = before ++ pattern ++ after
    u = map toUpper
    result = transformLineFixed IgnoreCase (u pattern) replacement content

prop_transformLineRegex :: AlphaString
                        -> AlphaString
                        -> String
                        -> String
                        -> Property
prop_transformLineRegex (Alpha patStr) (Alpha repStr) before after =
    not (patStr `isInfixOf` (repStr ++ after)) ==>
    not (patStr `isInfixOf` (before ++ repStr)) ==>
    printTestCase (show result) $
    repStr `isInfixOf` result &&
    not (patStr `isInfixOf` result)
  where
    content = before ++ patStr ++ after
    result = B8.unpack $ transformLineRegex regex rep $ B8.pack content
    Right regex = compileRegex False patStr
    rep = literalReplacement repStr

tests =
    testGroup "FindReplace"
    [ testProperty "mergeLiterals1" prop_mergeLiterals1
    , testProperty "mergeLiterals2" prop_mergeLiterals2
    , testProperty "expand" prop_expand
    , testProperty "parseReplacement" prop_parseReplacement
    , testCase "parseReplacement0" case_parseReplacement0
    , testProperty "transformLineFixed1" prop_transformLineFixed1
    , testProperty "transformLineFixed2" prop_transformLineFixed2
    , testProperty "transformLineRegex" prop_transformLineRegex
    ]
