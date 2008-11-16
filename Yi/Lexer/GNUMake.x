-- -*- haskell -*- 
-- Lexer for Makefiles with consideration of GNU extensions 
-- This is based off the syntax as described in the GNU Make manual:
-- http://www.gnu.org/software/make/manual/make.html
--
{
{-# OPTIONS -w  #-}
module Yi.Lexer.GNUMake
  ( initState, alexScanToken ) 
where
import Yi.Lexer.Alex
import Yi.Style
  ( Style             ( .. )
  , StyleName
  )
import qualified Yi.Style as Style
}

make :-

<0>
{
    -- All lines that start with a \t are passed to the shell.
    -- This includes # characters that might be in the shell code! Those indicate comments *only* if
    -- the shell interpretting the code would consider it a comment. Wack huh?
    -- See 3.1
    ^\t.*
        { c Style.makeFileAction }
    \#
        { m (\_ -> InComment) Style.commentStyle }
    \n
        { c Style.defaultStyle }
    .
        { c Style.defaultStyle }
}

<comment>
{
    -- Comments can be continued to the next line with a trailing slash.
    -- See 3.1
    \\[.\n]
        { c Style.commentStyle }
    \n
        { m (\_ -> TopLevel) Style.defaultStyle }
    .
        { c Style.commentStyle }
}

{
data HlState = 
      TopLevel 
    | InComment

stateToInit TopLevel = 0
stateToInit InComment = comment

initState :: HlState
initState = TopLevel

type Token = StyleName
#include "alex.hsinc"
}

