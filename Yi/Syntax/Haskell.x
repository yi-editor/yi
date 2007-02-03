--
-- Lexical syntax for illiterate Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--

{
{-# OPTIONS -w  #-}
module Yi.Syntax.Haskell ( highlighter ) where

import qualified Data.ByteString.Char8
import qualified Yi.Syntax
import Yi.Vty

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

@reservedid = 
        as|case|class|data|default|deriving|do|else|hiding|if|
        import|in|infix|infixl|infixr|instance|let|module|newtype|
        of|qualified|then|type|where|forall|mdo|foreign|export|dynamic|
        safe|threadsafe|unsafe|stdcall|ccall|dotnet

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

<0> $white+                                     { c attr } -- whitespace

<nestcomm> {
  "{-"                                          { m (subtract 1) attr{fg = red} }
  "-}"                                          { m (+1) attr{fg = red} }
  .                                             { c attr{fg = red} }
}

<0> {
  "--"\-* $symbol $symchar*                     { c attr }
  "--"\-*[^\n]*                                 { c attr{fg = red} }

 "{-"                                           { m (subtract 1) attr{fg = red} }

 $special                                       { c attr }

 @reservedid                                    { c attr{fg=blue, bold=True} }
 @varid                                         { c attr }
 @conid                                         { c attr{fg=green} }

 @reservedop                                    { c attr{fg=yellow} }
 @varsym                                        { c attr{fg=yellow} }
 @consym                                        { c attr{fg=green} }

 @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal                          { c attr }

 @decimal \. @decimal @exponent?
  | @decimal @exponent                          { c attr }

 \' ($graphic # [\'\\] | " " | @escape) \'      { c attr{fg=green} }
 \" @string* \"                                 { c attr{fg=green} }
 .                                              { c attr{fg=red, bold=True} }
}

{

type HlState = Int

stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState = 0

#include "alex.hsinc"
}
