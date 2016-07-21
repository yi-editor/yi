-- -*- haskell -*-
--  Simple lexer for c

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-} -- Alex generate warnings-ridden code.
module Yi.Lexer.C ( lexer ) where

{- Standard Library Modules Imported -}
import Yi.Lexer.Alex hiding (tokenToStyle)

{- External Library Modules Imported -}

{- Local Modules Imported -}
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
    auto
  | break
  | case
  | char
  | const
  | continue
  | default
  | do
  | double
  | else
  | enum
  | extern
  | float
  | for
  | goto
  | if
  | int
  | long
  | register
  | return
  | short
  | signed
  | static
  | struct
  | switch
  | typedef
  | union
  | unsigned
  | void
  | volatile
  | while

@cppid =
    "#define"
  | "#defined"
  | "#if"
  | "#ifdef"
  | "#ifndef"
  | "#elif"
  | "#else"
  | "#endif"
  | "#include"

-- From this list, but only the C ones: http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B
@reservedop =
  "+"  | "++"  | "+=" | "-"   | "--" | "-=" | "*"      | "*=" | "/"  | "/=" | "%"  | "%=" |
  "<"  | "<="  | ">"  | ">="  | "!=" | "==" |
  "!"  | "&&"  | "||" |
  "<<" | "<<=" | ">>" | ">>=" | "~"  | "&"  | "&="     | "|"  | "|=" | "^"  | "^=" |
  "="  | "->"  | "."  | ","   | "?"  | ":"  | "sizeof"

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

c :-

<0> $white+                                     { c defaultStyle } -- whitespace

<nestcomm> {
  -- We could do nested comments like this
  -- "/*"                                       { m (subtract 1) blockCommentStyle }
  "*/"                                          { m (+1) blockCommentStyle }
  $white+                                       ; -- Whitespace
  .                                             { c blockCommentStyle }
}

<0> {
  "//"[^\n]*                                    { c commentStyle }
  "/*".*"*/"                                    { c blockCommentStyle }

 "/*" @reservedop*                              { m (subtract 1) blockCommentStyle }

 $special                                       { c defaultStyle }

 @reservedid                                    { c keywordStyle }
 @cppid                                         { c preprocessorStyle }
 @varid                                         { c defaultStyle }
 @conid                                         { c typeStyle }

 @reservedop                                    { c operatorStyle }
 @varsym                                        { c operatorStyle }
 @consym                                        { c typeStyle }

 @decimal
  | 0[oO] @octal
  | 0[xX] @hexadecimal                          { c defaultStyle }

 @decimal \. @decimal @exponent?
  | @decimal @exponent                          { c defaultStyle }

 \' ($graphic # [\'\\] | " " | @escape) \'      { c stringStyle }
 \" @string* \"                                 { c stringStyle }
 .                                              { c operatorStyle }
}


{

type HlState = Int
type Token = StyleName

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

stateToInit :: HlState -> Int
stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState :: HlState
initState = 0

#include "common.hsinc"
}
