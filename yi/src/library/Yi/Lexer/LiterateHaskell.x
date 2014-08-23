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
module Yi.Lexer.LiterateHaskell ( initState, alexScanToken, HlState ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Lexer.Haskell hiding (initState, alexScanToken, HlState)
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
        as|case|class|data|default|else|hiding|if|
        import|in|infix|infixl|infixr|instance|newtype|
        qualified|then|type|family|foreign|export|dynamic|
        safe|threadsafe|unsafe|stdcall|ccall|dotnet

@layoutReservedId =
    of|let|do|mdo


@reservedop =
        ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
@qual   = (@conid ".")*
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

<0> $white+                                     ;

<nestcomm> {
  "{-"                                          { m CommentBlock (Comment Open) }
  "-}"                                          { m unComment (Comment Close) }
  $white+                                       ; -- whitespace
  [^\-\{]+                                      { c $ Comment Text } -- rule to generate comments larger than 1 char
  .                                             { c $ Comment Text }
}

<0> {
  ^ "\begin{code}"                              { m (const CodeBlock) $ Reserved Other }
  ^ ">"                                         { ms (const CodeLine) Operator }
  $white+                                       ; -- whitespace
  .                                             { c $ Comment Text {-LaTeX-} }
}

<codeBlock> {
  "\end{code}"                                  { m (const LaTeXBlock) $ Reserved Other }

  $white+                                       ; -- whitespace

-- The first rule matches operators that begin with --, eg --++-- is a valid
-- operator and *not* a comment.
-- Note that we have to dissallow '-' as a symbol char for the first one
-- of these because we may have -------- which would stilljust be the
-- start of a comment.
  "--"\-* [$symbol # \-] $symchar*              { cs Operator }
-- The next rule allows for the start of a comment basically
-- it is -- followed by anything which isn't a symbol character
-- (OR more '-'s). So for example "-----:" is still the start of a comment.
  "--"~[$symbol # \-][^$nl]*                    { c $ Comment Line }
-- Finally because the above rule had to add in a non symbol character
-- it's also possible that we have just finishing a line,
-- people sometimes do this for example  when breaking up paragraphs
-- in a long comment.
  "--"$nl                                       { c $ Comment Line }

  "{-"                                          { m CommentBlock $ Comment Open }

  ^"#".*                                        { c $ CppDirective }
  $special                                      { cs $ \(c:_) -> Special c }
  "deriving"                                    { c (Reserved Deriving) }
  "forall"                                      { c (Reserved Forall) }
  @reservedid                                   { c (Reserved Other) }
  "module"                                      { c (Reserved Module) }
  "where"                                       { c (Reserved Where) }
  @layoutReservedId                             { c (Reserved OtherLayout) }
  `@qual @varid`                                { cs $ Operator . init . tail }
  `@qual @conid`                                { cs $ ConsOperator . init . tail }
  @qual @varid                                  { c VarIdent }
  @qual @conid                                  { c ConsIdent }

  "|"                                           { c (ReservedOp Pipe) }
  "="                                           { c (ReservedOp Equal) }
  \\                                            { c (ReservedOp BackSlash) }
  "<-"                                          { c (ReservedOp LeftArrow) }
  "->"                                          { c (ReservedOp RightArrow) }
  "=>"                                          { c (ReservedOp DoubleRightArrow) }
  ".."                                          { c (ReservedOp DoubleDot) }
  "@"                                           { c (ReservedOp Arobase) }
  "~"                                           { c (ReservedOp Tilda) }
  "=>"                                          { c (ReservedOp DoubleRightArrow) }
  "::"                                          { c (ReservedOp DoubleColon) }
  @qual @varsym                                 { cs Operator }
  @qual @consym                                 { cs ConsOperator }

  @decimal
    | 0[oO] @octal
    | 0[xX] @hexadecimal                        { c Number }

  @decimal \. @decimal @exponent?
    | @decimal @exponent                        { c Number }

  \' ($graphic # [\'\\] | " " | @escape) \'     { c CharTok }
  \" @string* \"                                { c StringTok }
  .                                             { c Unrecognized }
}

<codeLine> {
  [\t\n\r\f\v]+                                 { m (const LaTeXBlock) $ Reserved Other }

  [\ \t]+                                       ; -- whitespace

-- Same three rules for line comments as above (see above for explanation).
  "--"\-* [$symbol # \-] $symchar*              { cs Operator }
  "--"~[$symbol # \-][^$nl]*                    { c $ Comment Line }
  "--"$nl                                       { c $ Comment Line }

  "{-"                                          { m CommentBlock $ Comment Open }

  ^"#".*                                        { c $ CppDirective }
  $special                                      { cs $ \(c:_) -> Special c }
  "deriving"                                    { c (Reserved Deriving) }
  "forall"                                      { c (Reserved Forall) }
  @reservedid                                   { c (Reserved Other) }
  "module"                                      { c (Reserved Module) }
  "where"                                       { c (Reserved Where) }
  @layoutReservedId                             { c (Reserved OtherLayout) }
  `@qual @varid`                                { cs $ Operator . init . tail }
  `@qual @conid`                                { cs $ ConsOperator . init . tail }
  @qual @varid                                  { c VarIdent }
  @qual @conid                                  { c ConsIdent }

  "|"                                           { c (ReservedOp Pipe) }
  "="                                           { c (ReservedOp Equal) }
  \\                                            { c (ReservedOp BackSlash) }
  "<-"                                          { c (ReservedOp LeftArrow) }
  "->"                                          { c (ReservedOp RightArrow) }
  "=>"                                          { c (ReservedOp DoubleRightArrow) }
  ".."                                          { c (ReservedOp DoubleDot) }
  "@"                                           { c (ReservedOp Arobase) }
  "~"                                           { c (ReservedOp Tilda) }
  "=>"                                          { c (ReservedOp DoubleRightArrow) }
  "::"                                          { c (ReservedOp DoubleColon) }
  @qual @varsym                                 { cs Operator }
  @qual @consym                                 { cs ConsOperator }

  @decimal
    | 0[oO] @octal
    | 0[xX] @hexadecimal                        { c Number }

  @decimal \. @decimal @exponent?
    | @decimal @exponent                        { c Number }

  \' ($graphic # [\'\\] | " " | @escape) \'     { c CharTok }
  \" @string* \"                                { c StringTok }
  .                                             { c Unrecognized }
}

{

data HlState = CodeBlock
             | CodeLine
             | CommentBlock { unComment :: HlState }
             | LaTeXBlock
  deriving (Eq, Show)

stateToInit (CommentBlock _) = nestcomm
stateToInit CodeBlock        = codeBlock
stateToInit CodeLine         = codeLine
stateToInit LaTeXBlock       = 0

initState = LaTeXBlock

#include "common.hsinc"
}
