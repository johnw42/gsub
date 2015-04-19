module FindReplace where

import Control.Applicative
import Control.Exception
import Control.Monad (liftM, liftM2)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.List
import Numeric (readDec)
import qualified Text.Regex.PCRE.Heavy as Heavy
import qualified Text.Regex.PCRE.Light as Light

type FileContent = L8.ByteString

-- | How letter case should be handled.
data CaseHandling
    = IgnoreCase
    | ConsiderCase
    | SmartCase
    deriving Show

-- | Ways to transform a replacement string.
data ReplacementCase
    = ReplaceUpper
    | ReplaceTitle
    | ReplaceOriginal

-- | Data structure representing how to replace a regex match.
data Replacement = Rep
    { repParts :: [ReplacementPart]
    , repMaxGroup :: Int
    }
data ReplacementPart
    = LiteralPart String  -- ^ Insert a literal value.
    | GroupPart Int       -- ^ Insert the text of a match group.
    deriving (Show, Eq)

-- | Converts a string to the given case.
stringToCase :: ReplacementCase -> String -> String
stringToCase ReplaceOriginal s = s
stringToCase ReplaceUpper s = map toUpper s
stringToCase ReplaceTitle (c:cs) = toUpper c : cs
stringToCase ReplaceTitle s = s

-- | Finds the desired replacement case for a given match when using
-- 'SmartCase'.
replacementCase :: String -> ReplacementCase
replacementCase (c1:c2:_)
    | isUpper c1 && isUpper c2 = ReplaceUpper
replacementCase (c:_)
    | isUpper c = ReplaceTitle
replacementCase _ = ReplaceOriginal

makeReplacement :: [ReplacementPart] -> Replacement
makeReplacement parts = Rep parts maxGroup
  where
    maxGroup = maximum $ 0 : map partGroupNum parts
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
expand :: Replacement -> [String] -> String
expand (Rep parts maxG) groups =
    assert (not $ null $ drop maxG groups) $
    expand' parts groups

expand' :: [ReplacementPart] -> [String] -> String
expand' parts groups = concatMap expandGroup parts
  where
    expandGroup (LiteralPart s) = s
    expandGroup (GroupPart n) = groups !! n

-- | Compiles a regex.
compileRegex :: Bool -> String -> Either String Heavy.Regex
compileRegex ignoreCase pat = Heavy.compileM (B8.pack pat) pcreOpts
  where
    pcreOpts = if ignoreCase
               then [Light.caseless]
               else []

-- | Transforms a regex pattern to match using 'SmartCase' semantics.
smartCaseRegex :: String -> Either String String
smartCaseRegex "" = Right ""
smartCaseRegex ('[':_) = Left "character class"
smartCaseRegex ('\\':c:cs)
    | isSafeEscape c = liftM (('\\' : [c]) ++) (smartCaseRegex cs)
    | otherwise = Left ("escape sequence starting with \\" ++ [c])
  where
    isSafeEscape c = c `elem` "aAbBdDefhHKnNrRsStvVwWzZG"
smartCaseRegex ('(':'?':cs)
    | isSafeMagic cs = liftM ("(?" ++) (smartCaseRegex cs)
    | otherwise = Left "magic parens"
  where
    isSafeMagic s = any (`isPrefixOf` s) safeMagicSeqs
    safeMagicSeqs = [":", "|", ">", "#", "=", "!", "<=", "<!"]
smartCaseRegex (c:cs)
    | isLower c =
          liftM (('[' : c : toUpper c : ']' : []) ++) (smartCaseRegex cs)
    | otherwise = liftM (c:) (smartCaseRegex cs)

-- | Transforms a line using fixed strings.
transformLineFixed
    :: CaseHandling
    -> String  -- ^ Pattern string.
    -> String  -- ^ Replacement string.
    -> String  -- ^ String to replace in.
    -> String
transformLineFixed ch needle rep line = loop line
  where
    loop "" = ""
    loop cs@(c:cs')
        | needle `isCasePrefixOf` cs =
              rep ++ loop (drop (length needle) cs)
        | otherwise = c : loop cs'
    isCasePrefixOf l r = case ch of
        ConsiderCase -> l `isPrefixOf` r
        IgnoreCase -> map toUpper l `isPrefixOf` map toUpper r
        SmartCase -> l `isSmartPrefixOf` r
    "" `isSmartPrefixOf` "" = True
    (c:_) `isSmartPrefixOf` "" = False
    (l:ls) `isSmartPrefixOf` (r:rs) =
        l `charMatches` r && ls `isSmartPrefixOf` rs
    charMatches l r
        | isLower l = toUpper l == toUpper r
        | otherwise = l == r

-- | Transforms a line using regex replacement.
transformLineRegex :: Heavy.Regex -> Replacement -> FileContent -> FileContent
transformLineRegex regex rep line =
    Heavy.gsub regex (expand rep) line
