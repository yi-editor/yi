{-
  \section{Module Yi.Keymap.Movements}
  This file represents some common movements that maybe used by
  all of the individual key maps, rather than re-implementing such
  movements again and again for separate keymaps.
-}

module Yi.Keymap.Movements ( moveRightWord
			   , movementCommand 
			   , newlineAndIndent
			   , writeFileandMessage
			   )
where

{- Standard library modules imported -}
import Data.Char

{- Local, or Yi, imported modules -}
import Yi.Editor hiding     ( keymap )
import Yi.Yi hiding         ( keymap, meta, string )
-- import Yi.Window
-- import Yi.Buffer


{-
  \subsection{File Commands}
  This sections contains commands for reading and writing
  the buffers to disk.
-}
writeFileandMessage :: Action
writeFileandMessage =
    do bufInfo <- bufInfoE
       fwriteE
       msgE $ concat [ "file: "
		     , show $ bufInfoFileName bufInfo
		     , " written to disk"
		     ]


{-
  \subsection{Movement commands}
  In this section we define commands for quickly moving the cursor
  to where we want to be.

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
	       , isModified
	       ]
	where isModified
		  | bufInfoModified bufInfo = "(Modified)"
		  | otherwise               = ""



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
  \subsection{Simple Indentation}
  Here we will define a simple indentation algorithm. The basic idea is that
  when we want to indent we indent the same amount as the previous line
  and then the user can move it to the left or the right however they like.
  This is obviously very primitive and for most languages not ideal, however it
  has two very good properties, it's very predictable, and works with pretty
  much any language, even something like tex. It is also has the desirable
  property that it is easy to implement.

  We start by simply looking at the previous line, we should also check
  if it was an empty line and if so check the line above and so on.

  As a side issue note here that we cannot place the new line character
  in the string given to [-indentNE-] (see the definition of [-indentNE-]
  in [-Yi.Core.hs-]) so we combine two actions, this means that the
  undo command will not undo these as one action; two questions arise
  from this: is there a way around this? and do we want to get around it
  or is the splitting behaviour desirable?
-}
newlineAndIndent :: Action
newlineAndIndent = do lineChars <- readLnE
		      let spaces = indentationChars lineChars
		      insertE '\n'
		      insertNE spaces


{-
 We should also write an indent current line, which is a bit more
 than the above, since the point may not be current at the start of
 the line so we cannot simply insert at number of spaces.
-}


{-
  \subsection{Utility functions}
  This section contains small utility functions used by the definitions
  above for common calculations such as finding the next non-whitespace
  character.
-}

-- | Returns the indentation characters of the given string,
-- that is return the white space at the start of the string, generally
-- the given string will be a line.
indentationChars :: String -> String
indentationChars [] = []
indentationChars ( first : rest )
    | isSpace first = first : (indentationChars rest)
    | otherwise     = []
