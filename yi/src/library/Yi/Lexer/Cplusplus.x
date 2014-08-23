-- -*- haskell -*-
--  Simple lexer for c/c++ files

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.Cplusplus ( lexer ) where
{- Standard Library Modules Imported -}
import Yi.Lexer.Alex hiding (tokenToStyle)
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Yi.Syntax
import Yi.Style

{- End of Module Imports -}

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
  asm
  |break
  |case
  |continue
  |default
  |do
  |else
  |enum
  |for
  |fortran
  |goto
  |if
  |return
  |sizeof
  |struct
  |switch
  |typedef
  |union
  |while
  |_Bool
  |_Complex
  |_Imaginary
  |bool
  |char
  |double
  |float
  |int
  |long
  |short
  |signed
  |size_t
  |unsigned
  |void
  |auto
  |const
  |extern
  |inline
  |register
  |restrict
  |static
  |volatile
  |NULL
  |MAX
  |MIN
  |TRUE
  |FALSE
  |__LINE__
  |__DATA__
  |__FILE__
  |__func__
  |__TIME__
  |__STDC__
  |and
  |and_eq
  |bitand
  |bitor
  |catch
  |compl
  |const_cast
  |delete
  |dynamic_cast
  |false
  |for
  |friend
  |new
  |not
  |not_eq
  |operator
  |or
  |or_eq
  |private
  |protected
  |public
  |reinterpret_cast
  |static_cast
  |this
  |throw
  |true
  |try
  |typeid
  |using
  |xor
  |xor_eq
  |class
  |namespace
  |typename
  |template
  |virtual
  |bool
  |explicit
  |export
  |inline
  |mutable
  |wchar_t


@reservedop =
  "->" | "*" | "+" | "-" | "%" | \\ | "||" | "&&" | "?" | ":"

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
  -- We could do nested comments like this
  -- "/*"                                       { m (subtract 1) blockCommentStyle }
  "*/"                                          { m (+1) blockCommentStyle }
  $white+                                       { c defaultStyle } -- whitespace
  .                                             { c blockCommentStyle }
}

<0> {
  "//"[^\n]*                                    { c commentStyle }

 "/*"                                           { m (subtract 1) blockCommentStyle }

 $special                                       { c defaultStyle }

 @reservedid                                    { c keywordStyle }
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

stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState :: HlState
initState = 0

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
