-- -*- haskell -*- 
--
-- Simple syntax highlighting for Latex source files
--
-- This is not intended to be a lexical analyser for
-- latex, merely good enough to provide some syntax
-- highlighting for latex source files.
--

{
{-# OPTIONS -w  #-}
module Yi.Syntax.Latex ( highlighter ) where

import qualified Data.ByteString.Char8
import qualified Yi.Syntax
import Yi.Style

}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid = newcommand|begin|end

@reservedop =
        ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

haskell :-

<0> $white+                                     { c defaultStyle } -- whitespace


<0> {
  "%"\-*[^\n]*                                  { c commentStyle }

 $special                                       { c defaultStyle }

 @reservedid                                    { c keywordStyle }

 \\ @varid                                      { c upperIdStyle }

 @reservedop                                    { c operatorStyle }
 @varsym                                        { c operatorStyle }
 @consym                                        { c upperIdStyle }

 @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal                          { c operatorStyle }

 @decimal \. @decimal @exponent?
  | @decimal @exponent                          { c defaultStyle }

 .                                              { c defaultStyle }
}

{

commentStyle :: Style
commentStyle = lineCommentStyle

lineCommentStyle :: Style
lineCommentStyle = purpleA

keywordStyle :: Style
keywordStyle = darkredA

operatorStyle :: Style
operatorStyle = brownA

upperIdStyle :: Style
upperIdStyle = darkgreenA

stringStyle :: Style
stringStyle = darkcyanA



blackA       = Style black defaultbg
greyA        = Style grey defaultbg
darkredA     = Style darkred defaultbg
redA         = Style red defaultbg
darkgreenA   = Style darkgreen defaultbg
greenA       = Style green defaultbg
brownA       = Style brown defaultbg
yellowA      = Style yellow defaultbg
darkblueA    = Style darkblue defaultbg
blueA        = Style blue defaultbg
purpleA      = Style purple defaultbg
magentaA     = Style magenta defaultbg
darkcyanA    = Style darkcyan defaultbg
cyanA        = Style cyan defaultbg
whiteA       = Style white defaultbg
brightwhiteA = Style brightwhite defaultbg


type HlState = Int

{- See Haskell.x which uses this to say whether we are in a
   comment (perhaps a nested comment) or not.
-}
stateToInit x = 0

initState = 0

#include "alex.hsinc"
}
