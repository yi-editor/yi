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
module Yi.Lexer.LiterateHaskell ( initState, alexScanToken ) where
import Yi.Lexer.Alex
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
  ^ "\begin{code}"                              { m (const CodeBlock) keywordStyle }
  ^ ">"                                         { m (const CodeLine) operatorStyle }
  $white+                                       { c defaultStyle } -- whitespace
  .                                             { c latexStyle }
}

<codeBlock> {
  "\end{code}"                                  { m (const LaTeX) keywordStyle }

  $white+                                       { c defaultStyle } -- whitespace

-- The first rule matches operators that begin with --, eg --++-- is a valid
-- operator and *not* a comment. 
-- Note that we have to dissallow '-' as a symbol char for the first one
-- of these because we may have -------- which would stilljust be the 
-- start of a comment.
  "--"\-* [$symbol # \-] $symchar*              { c defaultStyle }
-- The next rule allows for the start of a comment basically
-- it is -- followed by anything which isn't a symbol character
-- (OR more '-'s). So for example "-----:" is still the start of a comment.
  "--"~[$symbol # \-][^$nl]*                    { c commentStyle }
-- Finally because the above rule had to add in a non symbol character 
-- it's also possible that we have just finishing a line,
-- people sometimes do this for example  when breaking up paragraphs
-- in a long comment.
  "--"$nl                                       { c commentStyle }

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

-- Same three rules for line comments as above (see above for explanation).
  "--"\-* [$symbol # \-] $symchar*              { c defaultStyle }
  "--"~[$symbol # \-][^$nl]*                    { c commentStyle }
  "--"$nl                                       { c commentStyle }

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
type Token = StyleName
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
