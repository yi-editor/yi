-- -*- haskell -*-

-- Lexer for Whitespace
-- (C) Copyright 2009 Deniz Dogan (mad credz)

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}

module Yi.Lexer.Whitespace ( lexer ) where

import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style (StyleName, withBg, red, green, commentStyle)

}

@space = " "
@tab = \t
@nl = [\r\n]

:-

<0> {
@space+  { c spaceStyle }
@tab+    { c tabStyle }
@nl      ;
.        { c commentStyle }
}

{

spaceStyle :: StyleName
spaceStyle = const $ withBg red

tabStyle :: StyleName
tabStyle = const $ withBg green

type Token = StyleName
type HlState = Int

stateToInit x = 0

initState :: HlState
initState = 0

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"

}
