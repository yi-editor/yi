-- -*- haskell -*-
-- Lexer for YAML-file
-- This is based off the syntax as described in the github manual:
-- http://yaml.org/spec/1.2/spec.html
-- Maintainer: Junji Hashimoto
{
{-# OPTIONS -w  #-}
module Yi.Lexer.YAML ( lexer ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
  ( Style             ( .. )
  , StyleName
  )
import qualified Yi.Style as Style
}

$space = [\ ]

yaml :-

<0>
{
    ^\%.* { c Style.importStyle }
    ^\-\-\- { c Style.importStyle }
    ^$space*\#.* { c Style.commentStyle }
    ^$space*[\-] { c Style.keywordStyle }
    ^$space*[^\:]+\: { c Style.keywordStyle }
    \n
        { c Style.defaultStyle }
    .
        { c Style.defaultStyle }
}

{
data HlState =
      TopLevel
  deriving Show
stateToInit TopLevel = 0

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
