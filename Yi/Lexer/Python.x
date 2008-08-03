-- -*- haskell -*- 
--
--  A poorly-written Python lexer
--
--  This is one of the first lexers I've ever written, so this could probably be
--  rewritten much, much better.

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Python ( initState, alexScanToken ) where

import Yi.Lexer.Alex
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

$strprefix = [urUR]

@reservedid = 
    and
  | as
  | assert
  | break
  | class
  | continue
  | def
  | del
  | elif
  | else
  | except
  | exec
  | finally
  | for
  | from
  | global
  | if
  | import
  | in
  | is
  | lambda
  | not
  | or
  | pass
  | print
  | raise
  | return
  | try
  | while
  | with
  | yield

@reservedop = 
  "!=" | "==" | "<<" | ">>" | "-" | "~" | "+" | "/" | "*" | "%" | "=" | "<" | ">" | "&" | "^" | "|" | "."

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol+
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

<0> {
 $white+                                        { c defaultStyle }

 "#"[^\n]*                                      { c commentStyle }

 $special                                       { c defaultStyle }

 @reservedid                                    { c keywordStyle }
 @varid                                         { c defaultStyle }
 @conid                                         { c upperIdStyle }

 @reservedop                                    { c operatorStyle }
 @varsym                                        { c operatorStyle }

 @decimal 
  | 0 @octal
  | 0[xX] @hexadecimal                          { c defaultStyle }

 @decimal \. @decimal @exponent?
  | @decimal @exponent                          { c defaultStyle }

 $strprefix* \" @string* \"
   | $strprefix* \' @string* \'
   | $strprefix* \" \" \" @string* \" \" \"
   | $strprefix* \' \' \' @string* \' \' \'     { c stringStyle }
 .                                              { c operatorStyle }
}


{

type HlState = Int
type Token = StyleName

stateToInit x = 0

initState :: HlState
initState = 0

#include "alex.hsinc"
}
