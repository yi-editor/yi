--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

-- | String manipulation utilities
--

module Yi.String (isBlank,
                  chomp,
                  split,
                  capitalize,
                  capitalizeFirst,
                  dropSpace,
                  fillText,
                  modifyLines
                 ) where

import Data.List (isSuffixOf, isPrefixOf, intercalate)
import Data.Char (toUpper, toLower, isSpace, isAlphaNum)

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : map toLower cs

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (c:cs) 
    | isAlphaNum c = toUpper c : map toLower cs
    | otherwise = c : capitalizeFirst cs


-- | Remove any trailing strings matching /irs/ (input record separator)
-- from input string. Like perl's chomp(1).
--
chomp :: String -> String -> String
chomp irs st
    | irs `isSuffixOf` st
    = let st' = reverse $ drop (length irs) (reverse st) in chomp irs st'
    | otherwise = st
{-# INLINE chomp #-}

--
-- | Split a list into pieces that were held together by glue.  Example:
--
-- > split ", " "one, two, three" ===> ["one","two","three"]
--
split :: Eq a => [a] -- ^ Glue that holds pieces together
      -> [a]         -- ^ List to break into pieces
      -> [[a]]       -- ^ Result: list of pieces

split glue xs = split' xs
    where
    split' [] = []
    split' xs' = piece : split' (dropGlue rest)
        where (piece, rest) = breakOnGlue glue xs'
    dropGlue = drop (length glue)
{-# INLINE split #-}

--
-- | Break off the first piece of a list held together by glue,
--   leaving the glue attached to the remainder of the list.  Example:
--   Like break, but works with a [a] match.
--
-- > breakOnGlue ", " "one, two, three" ===> ("one", ", two, three")
--
breakOnGlue :: (Eq a) => [a] -- ^ Glue that holds pieces together
            -> [a]           -- ^ List from which to break off a piece
            -> ([a],[a])     -- ^ Result: (first piece, glue ++ rest of list)
breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest')
        where (piece, rest') = breakOnGlue glue xs
{-# INLINE breakOnGlue #-}


-- | Trim spaces at beginning /and/ end
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f


isBlank :: String -> Bool
isBlank = all isSpace

fillText margin = map (unwords . reverse) . fill margin [] . words
   where fill _ acc [] = [acc]
         fill n acc (w:ws) 
           | n + length w >= 80 = acc : fill (length w) [w] ws
           | otherwise = fill (n + 1 + length w) (w:acc) ws
         


-- | A helper function for creating functions suitable for
-- 'modifySelectionB' and 'modifyRegionB'.
-- To be used when the desired function should map across
-- the lines of a region.
modifyLines :: (String -> String) -> String -> String
modifyLines transform input
  -- Note the simple definition "unlines (map transform $ lines input)"
  -- only works if there is a newline character at the end of the input
  -- Because 'lines' eats up the newline character but 'unlines' adds
  -- one.
  | last input == '\n' = unlines newLines
  -- For other inputs if we use 'unlines' then a new line is inserted
  -- at the end, so instead of 'unlines' we use 'intercalate.
  | otherwise          = intercalate "\n" newLines
  where
  newLines = map transform $ lines input
