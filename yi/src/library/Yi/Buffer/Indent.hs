-- Handles indentation in the keymaps. Includes:
--  * (TODO) Auto-indentation to the previous lines indentation
--  * Tab-expansion
--  * Shifting of the indentation for a region of text

module Yi.Buffer.Indent
    ( autoIndentB
    , cycleIndentsB
    , indentAsNextB
    , indentAsPreviousB
    , indentOfB
    , indentOfCurrentPosB
    , indentSettingsB
    , indentToB
    , newlineAndIndentB
    , shiftIndentOfRegionB
    , tabB
    ) where

import Control.Applicative
import Yi.Buffer.Basic
import Yi.Buffer.Misc
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Data.Char
import Data.List (sort, nub)
import Yi.String

{- |
  Return either a \t or the number of spaces specified by tabSize in the
  IndentSettings. Note that if you actually want to insert a tab character
  (for example when editing makefiles) then you should use: @insertB '\t'@.
-}
tabB :: BufferM String
tabB = do
  indentSettings <- indentSettingsB
  return $ if expandTabs indentSettings
    then replicate (tabSize indentSettings) ' '
    else "\t"

{-|
  Retrieve the current indentation settings for the buffer.
-}
indentSettingsB :: BufferM IndentSettings
indentSettingsB = withModeB $ return . modeIndentSettings


{-|
  A specialisation of 'autoIndentHelperB'.
  This is the most basic and the user is encouraged to
  specialise 'autoIndentHelperB' on their own.
-}
autoIndentB :: IndentBehaviour -> BufferM ()
autoIndentB = autoIndentHelperB fetchPreviousIndentsB indentsOfString
  where
  -- Returns the indentation hints considering the given
  -- string as the line above the current one.
  -- The hints added are:
  --     The indent of the given string
  --     The indent of the given string plus two
  --     The offset of the last open bracket if any in the line.
  indentsOfString :: String -> BufferM [Int]
  indentsOfString input =
    do indent       <- indentOfB input
       bracketHints <- lastOpenBracketHint input
       indentSettings <- indentSettingsB
       return $ indent : (indent + shiftWidth indentSettings) : bracketHints

{-|
  This takes two arguments the first is a function to
  obtain indentation hints from lines above the current one.
  The second is a function to obtain a set of indentation hints
  from the previous line. Both of these are in the 'BufferM'
  monad although the second seems like it is unnecessary.
  However we must take into account the length of tabs which come
  from the the tab settings and hence we must be in the 'BufferM'
  monad.

  To get the straightforward behaviour of the indents of all previous
  lines until one of them has zero indent call this with:
  @autoIndentHelperB fetchPreviousIndentsB (fmap (: []) indentOfB)@
  However commonly we wish to have something more interesting for
  the second argument, in particular we commonly wish to have the
  last opening bracket of the previous line as well as its indent.
-}
autoIndentHelperB :: BufferM [ Int ]
                  -- ^ Action to fetch hints from previous lines
                 -> (String -> BufferM [ Int ])
                 -- ^ Action to calculate hints from previous line
                 -> IndentBehaviour
                 -- ^ Sets the indent behaviour,
                 -- see 'Yi.Buffer.IndentBehaviour' for a description
                 -> BufferM ()
autoIndentHelperB getUpwards getPrevious indentBehave =
  do upwardHints   <- savingExcursionB getUpwards
     previousLine  <- getNextLineB Backward
     previousHints <- getPrevious previousLine
     let allHints = upwardHints ++ previousHints
     cycleIndentsB indentBehave allHints

-- | Cycles through the indentation hints. It does this without
-- requiring to set/get any state. We just look at the current
-- indentation of the current line and moving to the largest
-- indent that is
cycleIndentsB :: IndentBehaviour -> [Int] -> BufferM ()
cycleIndentsB _ [] = return ()
cycleIndentsB indentBehave indents =
    do currentLine   <- readLnB
       currentIndent <- indentOfB currentLine
       indentToB $ chooseIndent currentIndent (sort $ nub indents)
  where
  -- Is the function to choose the indent from the given current
  -- indent to the given list of indentation hints.
  chooseIndent :: Int -> [ Int ] -> Int
  chooseIndent =
    case indentBehave of
      IncreaseCycle -> chooseIncreaseCycle
      DecreaseCycle -> chooseDecreaseCycle
      IncreaseOnly  -> chooseIncreaseOnly
      DecreaseOnly  -> chooseDecreaseOnly



  -- Choose the indentation hint which is one more than the current
  -- indentation hint unless the current is the largest or larger than
  -- all the indentation hints in which case choose the smallest
  -- (which will often be zero)
  chooseIncreaseCycle :: Int -> [ Int ] -> Int
  chooseIncreaseCycle currentIndent hints =
    -- Similarly to 'chooseDecreasing' if 'above' is null then
    -- we will go to the first of below which will be the smallest
    -- indentation hint, if above is not null then we are moving to
    -- the indentation hint which is one above the current.
    head (above ++ below)
    where
    (below, above) = span (<= currentIndent) hints

  -- Note that these functions which follow generally assume that
  -- the list of hints which have been given is already sorted
  -- and that the list is non-empty

  -- Choose the indentation hint one less than the current indentation
  -- unless the current indentation is the smallest (usually zero)
  -- in which case choose the largest indentation hint.
  chooseDecreaseCycle :: Int -> [ Int ] -> Int
  chooseDecreaseCycle currentIndent hints =
    -- So in particular if 'below' is null then we will
    -- go to the largest indentation, if below is not null
    -- we go to the largest indentation which is *not* higher
    -- than the current one.
    last (above ++ below)
    where
    (below, above) = span (< currentIndent) hints

  chooseIncreaseOnly :: Int -> [ Int ] -> Int
  chooseIncreaseOnly currentIndent hints =
    head $ filter (> currentIndent) hints ++ [ currentIndent ]

  chooseDecreaseOnly :: Int -> [ Int ] -> Int
  chooseDecreaseOnly currentIndent hints =
    last $ currentIndent : filter (< currentIndent) hints

{-|
  A function generally useful as the first argument to
  'autoIndentHelperB'. This searches the lines above
  the current line for the indentations of each line
  until we get to a line which has no indentation
  *and* is not empty. Indicating that we have reached
  the outer scope.
-}
fetchPreviousIndentsB :: BufferM [Int]
fetchPreviousIndentsB =
     -- Move up one line,
  do moveOffset <- lineMoveRel (-1)
     line       <- readLnB
     indent     <- indentOfB line
     -- So if we didn't manage to move upwards
     -- or the current offset was zero *and* the line
     -- was non-blank then we return just the current
     -- indent (it might be the first line but indented some.)
     if moveOffset == 0 ||
        ( indent == 0 &&
          any (not . isSpace) line )
        then return [ indent ]
        else (indent :) <$> fetchPreviousIndentsB


{-| An application of 'autoIndentHelperB' which adds more
    indentation hints using the given keywords.
    The offsets of the first set of keywords are used as hints.
    For the second set of keywords it is not the offsets of the
    keywords themselves but the offset of the first non-white
    characters after the keywords.

    In addition to the keyword hints we also do the same as the
    default ('autoIndentB') which is to use any non-closed
    opening brackets as hints.
-}
autoIndentWithKeywordsB :: [ String ]   -- ^ Keywords to act as hints
                        -> [ String ]   -- ^ Keywords to act as offset hints
                        -> IndentBehaviour
                        -> BufferM ()
autoIndentWithKeywordsB firstKeywords secondKeywords =
  autoIndentHelperB fetchPreviousIndentsB getPreviousLineHints
  where
  getPreviousLineHints :: String -> BufferM [ Int ]
  getPreviousLineHints input =
    do indent       <- indentOfB input
       bracketHints <- lastOpenBracketHint input
       keyHintsOne  <- keywordHints firstKeywords input
       keyHintsTwo  <- keywordAfterHints secondKeywords input
       return $ indent : (indent + 2) : ( bracketHints ++
                                          keyHintsOne  ++
                                          keyHintsTwo )

-- | Returns the position of the last opening bracket on the
-- line which is not closed on the same line.
-- Note that if we have unmatched parentheses such as "( ]"
-- then we may not get the correct answer, but in that case
-- then arguably we don't really care if we get the correct
-- answer (at least if we get it wrong the user may notice
-- their error).
-- We return a list here as it's a convenient way of returning
-- no hint in the case of there being no non-closed bracket
-- and normally such a hint will be part of a list of hints
-- anyway.
-- NOTE: this could be easily modified to return the indentations
-- of *all* the non-closed opening brackets. But I think this is
-- not what you generally want.
-- TODO: we also do not care whether or not the bracket is within
-- a string or escaped. If someone feels up to caring about that
-- by all means please fix this.
lastOpenBracketHint :: String -> BufferM [ Int ]
lastOpenBracketHint input =
  case getOpen 0 $ reverse input of
    Nothing -> return []
    Just s  -> (: []) <$> spacingOfB s
  where
  -- We get the last open bracket by counting through
  -- the reversed line, when we see a closed bracket we
  -- add one to the count. When we see an opening bracket
  -- decrease the count. If we see an opening bracket when the
  -- count is 0 we return the remaining (reversed) string
  -- as the part of the line which preceds the last opening bracket.
  -- This can then be turned into an indentation by calling 'spacingOfB'
  -- on it so that tabs are counted as tab length.
  -- NOTE: that this will work even if tab occur in the middle of the line
  getOpen :: Int -> String -> Maybe String
  -- We of course return nothing, there is no bracket to give a hint.
  getOpen _ []               = Nothing
  getOpen i (c : rest)
      -- If it is opening and we have no closing to match
      -- then we return the rest of the line
    | isOpening c && i == 0 = Just rest
      -- If i is not zero then we have matched one of the
      -- closing parentheses and we can decrease the nesting count.
    | isOpening c           = getOpen (i - 1) rest
      -- If the character is a closing bracket then we must increase
      -- the nesting count
    | isClosing c           = getOpen (i + 1) rest
      -- If it is just a normal character forget about it and move on.
    | otherwise             = getOpen i rest

  isOpening :: Char -> Bool
  isOpening '(' = True
  isOpening '[' = True
  isOpening '{' = True
  isOpening _   = False

  isClosing :: Char -> Bool
  isClosing ')' = True
  isClosing ']' = True
  isClosing '}' = True
  isClosing _   = False


-- | Returns the offsets of all the given keywords
-- within the given string. This is potentially useful
-- as providing indentation hints.
keywordHints :: [ String ] -> String -> BufferM [ Int ]
keywordHints keywords =
  getHints 0
  where
  -- Calculate the indentation hints of keywords from the
  -- given string. The first argument is the current offset.
  -- NOTE: that we have to take into account how long tab characters
  -- are according to the indentation settings.
  getHints :: Int -> String -> BufferM [ Int ]
  getHints _i []                     = return []
  getHints i input
    -- If there are no non-white characters left return zero hints.
    | null rest                      = return []
    -- Check if there are white space characters at the front and if
    -- so then calculate the ident of it and carry on.
    | not $ null white               = do spaceSize <- spacingOfB white
                                          getHints (i + spaceSize) rest
    -- If there are no white space characters check if we are looking
    -- at a keyword and if so add it as a hint
    | initNonWhite `elem` keywords = (i :) <$> whiteRestHints
    -- Finally we just continue with the tail.
    | otherwise                      = whiteRestHints
    where
    -- Separate into the leading non-white characters and the rest
    (initNonWhite, whiteRest) = break isSpace input
    -- Separate into the leading white space characters and the rest
    (white, rest)             = span isSpace input
    -- Get the hints from everything after any leading non-white space.
    -- This should only be used if there is no white space at the start.
    whiteRestHints            = getHints (i + length initNonWhite) whiteRest


-- | Returns the offsets of anything that isn't white space 'after'
-- a keyword on the given line.
-- This is essentially then the same as 'keywordHints' except that
-- for each keyword on the input rather than return the offset at
-- the start of the keyword we return the offset of the first non-white
-- character after the keyword.
keywordAfterHints :: [ String ] -> String -> BufferM [ Int ]
keywordAfterHints keywords =
  getHints 0
  where
  -- Calculate the indentation hints of keywords from the
  -- given string. The first argument is the current offset.
  -- NOTE: that we have to take into account how long tab characters
  -- are according to the indentation settings.
  getHints :: Int -> String -> BufferM [ Int ]
  getHints _i []                  = return []
  getHints i input
    -- If there is any preceding white space then just take the length
    -- of it (according to the indentation settings and proceed.
    | not $ null indentation      = do indent <- spacingOfB indentation
                                       getHints (i + indent) nonWhite
    -- If there is a keyword at the current position and
    -- the keyword isn't the last thing on the line.
    | key `elem` keywords
      && not (null afterwhite)    =  do indent    <- spacingOfB white
                                        let hint  =  i + length key + indent
                                        tailHints <- getHints hint afterwhite
                                        return $ hint : tailHints
    -- we don't have a hint and we can re-try for the rest of the line
    | otherwise                   = afterKeyHints
    where
    -- Split the input into the preceding white space and the rest
    (indentation, nonWhite) = span isSpace input

    -- The keyword and what is after the keyword
    -- this is only used if 'indentation' is null so we needn't worry that
    -- we are taking from the input rather than 'nonWhite'
    (key, afterkey)     = break isSpace input
    -- The white space and what is after the white space
    (white, afterwhite) = span  isSpace afterkey

    -- Get the hints from everything after any leading non-white space.
    -- This should only be used if there is no white space at the start.
    afterKeyHints       = getHints (i + length key) afterkey


{-|
  Returns the indentation of a given string. Note that this depends
  on the current indentation settings.
-}
indentOfB :: String -> BufferM Int
indentOfB = spacingOfB . takeWhile isSpace

{-| Returns the length of a given string taking into account the
    white space and the indentation settings.
-}
spacingOfB :: String -> BufferM Int
spacingOfB text =
  do indentSettings <- indentSettingsB
     let spacingOfChar :: Char -> Int
         spacingOfChar '\t' = tabSize indentSettings
         spacingOfChar _    = 1
     return (sum $ fmap spacingOfChar text)

{-| Indents the current line to the given indentation level.
    In addition moves the point according to where it was on the
    line originally. If we were somewhere within the indentation
    (ie at the start of the line or on an empty line) then we want
    to just go to the end of the (new) indentation.
    However if we are currently pointing somewhere within the text
    of the line then we wish to remain pointing to the same character.
-}
indentToB :: Int -> BufferM ()
indentToB level = do
    indentSettings <- indentSettingsB
    modifyRegionClever (rePadString indentSettings level) =<< regionOfB Line

-- | Indent as much as the previous line
indentAsPreviousB :: BufferM ()
indentAsPreviousB = indentAsNeighborLineB Backward

-- | Indent as much as the previous line
indentAsNextB :: BufferM ()
indentAsNextB = indentAsNeighborLineB Forward

indentAsNeighborLineB :: Direction -> BufferM ()
indentAsNeighborLineB dir =
  do otherLine   <- getNextNonBlankLineB dir
     otherIndent <- indentOfB otherLine
     indentToB otherIndent

-- | Insert a newline at point and indent the new line as the previous one.
newlineAndIndentB :: BufferM ()
newlineAndIndentB = newlineB >> indentAsPreviousB

-- | Set the padding of the string to newCount, filling in tabs if
-- expandTabs is set in the buffers IndentSettings
rePadString :: IndentSettings -> Int -> String -> String
rePadString indentSettings newCount input
    | newCount <= 0 = rest
    | expandTabs indentSettings = replicate newCount ' ' ++ rest
    | otherwise = tabs ++ spaces ++ rest
    where (_indents,rest) = span isSpace input
          tabs   = replicate (newCount `div` tabSize indentSettings) '\t'
          spaces = replicate (newCount `mod` tabSize indentSettings) ' '


-- | shifts right (or left if num is negative) num times, filling in tabs if
-- expandTabs is set in the buffers IndentSettings
indentString :: IndentSettings -> Int -> String -> String
indentString indentSettings numOfShifts input = rePadString indentSettings newCount input
    where (indents,_) = span isSpace input
          countSpace '\t' = tabSize indentSettings
          countSpace _ = 1 -- we'll assume nothing but tabs and spaces
          newCount = sum (fmap countSpace indents) + (shiftWidth indentSettings * numOfShifts)

-- | Increases the indentation on the region by the given amount of shiftWidth
shiftIndentOfRegionB :: Int -> Region -> BufferM ()
shiftIndentOfRegionB shiftCount region = do
    indentSettings <- indentSettingsB
    modifyRegionB (mapLines (indentString indentSettings shiftCount `unless` null)) region
    moveTo $ regionStart region
    firstNonSpaceB
  where (f `unless` c) x = if c x then x else f x

deleteIndentOfRegion :: Region -> BufferM ()
deleteIndentOfRegion = modifyRegionB (mapLines $ dropWhile isSpace)

-- | Return the number of spaces at the beginning of the line, up to the point.
indentOfCurrentPosB :: BufferM Int
indentOfCurrentPosB = do
    p <- pointB
    moveToSol
    sol <- pointB
    moveTo p
    let region = mkRegion p sol
    readRegionB region >>=  spacingOfB
