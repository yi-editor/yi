-- -*- haskell -*- 
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
    \\
        { m (\_ -> InCommentSlashEscape) Style.commentStyle }
    \n
        { m (\_ -> TopLevel) Style.defaultStyle }
    .
        { c Style.commentStyle }
}

<inCommentSlashEscape>
{
    \n
        { m (\_ -> InComment) Style.commentStyle }
    .
        { m (\_ -> InComment) Style.commentStyle }
}

{
data HlState = 
      TopLevel 
    | InComment
    | InCommentSlashEscape

stateToInit TopLevel = 0
stateToInit InComment = comment
stateToInit InCommentSlashEscape = inCommentSlashEscape

initState :: HlState
initState = TopLevel

type Token = StyleName
#include "alex.hsinc"
}

