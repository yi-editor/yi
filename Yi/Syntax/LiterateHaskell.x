-- -*- haskell -*-
--
-- Lexical syntax for literate Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--
-- Adapted to literate Haskell 98 by Nicolas Pouillard
--

{
{-# OPTIONS -w  #-}
module Yi.Syntax.LiterateHaskell ( highlighter ) where

import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Yi.FingerString
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

<0> $white+                                     { c defaultStyle } -- whitespace

<nestcomm> {
  "{-"                                          { m Comment commentStyle }
  "-}"                                          { m unComment commentStyle }
  $white+                                       { c defaultStyle } -- whitespace
  .                                             { c commentStyle }
}

<0> {
  "\begin{code}"                                { m (const CodeBlock) keywordStyle }
  ">"                                           { m (const CodeLine) operatorStyle }
  $white+                                       { c defaultStyle } -- whitespace
  .                                             { c latexStyle }
}

<codeBlock> {
  "\end{code}"                                  { m (const LaTeX) keywordStyle }

  $white+                                       { c defaultStyle } -- whitespace

  "--"\-* $symbol $symchar*                     { c defaultStyle }
  "--"\-*[^\n]*                                 { c commentStyle }

  "{-"                                          { m Comment commentStyle }

  $special                                      { c defaultStyle }

  @reservedid                                   { c keywordStyle }
  @varid                                        { c defaultStyle }
  @conid                                        { c upperIdStyle }

  @reservedop                                   { c operatorStyle }
  @varsym                                       { c operatorStyle }
  @consym                                       { c upperIdStyle }

  @decimal
   | 0[oO] @octal
   | 0[xX] @hexadecimal                         { c defaultStyle }

  @decimal \. @decimal @exponent?
   | @decimal @exponent                         { c defaultStyle }

  \' ($graphic # [\'\\] | " " | @escape) \'     { c stringStyle }
  \" @string* \"                                { c stringStyle }
  .                                             { c operatorStyle }
}

<codeLine> {
  [\t\n\r\f\v]+                                 { m (const LaTeX) keywordStyle }

  [\ \t]+                                       { c defaultStyle } -- whitespace

  "--"\-* $symbol $symchar*                     { c defaultStyle }
  "--"\-*[^\n]*                                 { c commentStyle }

  "{-"                                          { m Comment commentStyle }

  $special                                      { c defaultStyle }

  @reservedid                                   { c keywordStyle }
  @varid                                        { c defaultStyle }
  @conid                                        { c upperIdStyle }

  @reservedop                                   { c operatorStyle }
  @varsym                                       { c operatorStyle }
  @consym                                       { c upperIdStyle }

  @decimal
   | 0[oO] @octal
   | 0[xX] @hexadecimal                         { c defaultStyle }

  @decimal \. @decimal @exponent?
   | @decimal @exponent                         { c defaultStyle }

  \' ($graphic # [\'\\] | " " | @escape) \'     { c stringStyle }
  \" @string* \"                                { c stringStyle }
  .                                             { c operatorStyle }
}

{

data HlState = CodeBlock
             | CodeLine
             | Comment { unComment :: HlState }
             | LaTeX
  deriving (Eq)

stateToInit (Comment _) = nestcomm
stateToInit CodeBlock   = codeBlock
stateToInit CodeLine    = codeLine
stateToInit LaTeX       = 0

initState = LaTeX

latexStyle = commentStyle

#include "alex.hsinc"

}
