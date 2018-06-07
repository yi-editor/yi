-- -*- haskell -*-
-- Lexer for Markdown-file
-- This is based off the syntax as described in the github manual:
-- https://guides.github.com/features/mastering-markdown/
-- Maintainer: Junji Hashimoto
{
{-# OPTIONS -w  #-}
module Yi.Lexer.Markdown ( lexer ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
  ( Style             ( .. )
  , StyleName
  )
import qualified Yi.Style as Style
}

$varChar = $printable # [\: \# \= \ \{ \} \( \)]


$space = [\ ]

markdown :-

<0>
{
    ^\`\`\` { m (const InComment) Style.commentStyle }
    ^\#+ { c Style.keywordStyle }
    ^$space*[\+\-\*] { c Style.keywordStyle }
    ^$space*[0-9]+\. { c Style.keywordStyle }
    \!\[[^\]]*\]\([^\)]*\) { c Style.quoteStyle }
    \[[^\]]*\]\([^\)]*\) { c Style.quoteStyle }
    \[[^\]]*\]\[[^\]]*\] { c Style.quoteStyle }
    \*[^\*]*\* { c Style.stringStyle }
    \_[^\_]*\_ { c Style.stringStyle }
    \*\*[^\*]*\*\* { c Style.stringStyle }
    \_\_[^\_]*\_\_ { c Style.stringStyle }
    \n
        { c Style.defaultStyle }
    .
        { c Style.defaultStyle }
}

<comment>
{
    ^\`\`\` { m (const TopLevel) Style.commentStyle }
    \n
        { c Style.commentStyle }
    .
        { c Style.commentStyle }
}

{
data HlState =
      TopLevel
    | InComment
  deriving Show
stateToInit TopLevel = 0
stateToInit InComment = comment

initState :: HlState
initState = TopLevel

type Token = StyleName

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
