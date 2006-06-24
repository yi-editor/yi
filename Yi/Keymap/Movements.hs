{-
  This file represents some common movements that maybe used by
  all of the individual key maps, rather than re-implementing such
  movements again and again for separate keymaps.
-}

module Yi.Keymap.Movements ( moveRightWord
			   , movementCommand )
where

{- Standard library modules imported -}
import Data.Char

{- Local, or Yi, imported modules -}
import Yi.Editor hiding     ( keymap )
import Yi.Yi hiding         ( keymap, meta, string )
-- import Yi.Window
-- import Yi.Buffer

{-
  For moving around with the control key, this speeds up movement
  and I find it somewhat useful to be honest.

  As for [-moveRightWord-] I'm not sure if it is fast to continually
  perform a [-readE-] to get the current character, and then [-rightE-]
  to move on character to the right if the character is an alphaNum.
  Okay new tactic, find the first non-whitespace character and then
  move until we find the next white space character, I think this works
  quite well for programming symbols and such as well.
  It isn't brilliant but it's not bad, so probably it should, if the
  current character is alpha-num then skip to the first non alpha-num,
  if the current character *isn't* alpha-num, then skip to the first
  alpha-num.
  Other suggestions more than welcome.

  @todo{the other directions}
-}
-- | Move right by one word, semantics here are questionable.
moveRightWord :: Action
moveRightWord = 
    do restChars      <- readRestOfLnE
       getMovement restChars
    where skipTo :: (Char -> Bool) -> Int -> String -> Int
	  skipTo _ x [] = x
	  skipTo f x (c : rest)
	      | f c == False = skipTo f (x + 1) rest
	      | otherwise    = x

          -- The first states if we are at the end of
	  -- a line then just move to the start of the
	  -- next line, if we are at the last but one, then
	  -- move to the end of the line (maybe should move to the next line)
          getMovement :: String -> Action
	  getMovement []         = rightE
	  getMovement [_]        = rightE
	  getMovement (c1 : c2 : rest)
	      | isAlphaNum c1 = skipAlphaNum 1 $ c2 : rest
	      | isSpace c1 &&
		isAlphaNum c2 = skipAlphaNum 2 rest
	      | isSpace c1    = skipSpace 2 rest
	      | otherwise     = skipToAlphaNum 1 $ c2 : rest

          skipAlphaNum :: Int -> String -> Action
	  skipAlphaNum x s = rightOrEolE $ skipTo (not . isAlphaNum) x s

          skipSpace :: Int -> String -> Action
	  skipSpace x s = rightOrEolE $ skipTo (not . isSpace) x s

          skipToAlphaNum :: Int -> String -> Action
	  skipToAlphaNum x s = rightOrEolE $ skipTo isAlphaNum x s


{-
  This is the old code for [-getMovement-].
  [- nextNonWhite :: Int -> String -> (Int, String)
     nextNonWhite x [] = (x, [])
     nextNonWhite x (c : rest)
         | not $ isAlphaNum c = nextNonWhite (x + 1) rest
	 | otherwise          = (x, c : rest)

     wordLength :: Int -> String -> Int
     wordLength x []            = x
     wordLength x (firstChar : rest)
	  | isAlphaNum firstChar = wordLength (x + 1) rest
	  | otherwise            = x

     getMovement :: String -> Int
     getMovement s = 
	  wordLength skipped restS
	  where (skipped, restS) = nextNonWhite 0 s
  -]
-}


{-
  Writing to the mode line information about the current buffer, in particular
  the current line and column numbers.
-}
-- | Performs the given action and then updates the message with
-- | the current (possibly updated) position.
movementCommand :: Action -> Action
movementCommand act = 
    do act
       bufInfo <- bufInfoE
       msgE $ showBufInfo bufInfo
    where 
    showBufInfo :: BufferFileInfo -> String
    showBufInfo bufInfo =
	concat [ show $ bufInfoFileName bufInfo
	       , " Line "
	       , show $ bufInfoLineNo bufInfo
	       , " Col "
	       , show $ bufInfoColNo bufInfo
	       , " ["
	       , bufInfoPercent bufInfo
	       , "]"
	       ]