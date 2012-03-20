-- Lexer for Whitespace
-- (C) Copyright 2009 Deniz Dogan (mad credz)

{

{-# OPTIONS -w  #-}
{-# LANGUAGE BangPatterns  #-}

module Yi.Lexer.Whitespace ( initState, alexScanToken ) where

import Yi.Lexer.Alex
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

#include "common.hsinc"

}
