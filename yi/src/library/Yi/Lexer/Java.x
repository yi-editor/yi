-- -*- haskell -*-
--  Simple lexer for c

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-} -- Alex generate warnings-ridden code.
module Yi.Lexer.Java ( lexer ) where

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

@keywordid =
  abstract
  | assert
  | break
  | catch
  | class
  | case
  | const
  | continue
  | default
  | except
  | extends
  | else
  | false
  | finally
  | goto
  | final
  | for
  | if
  | implements
  | import
  | instanceof
  | interface
  | long
  | native
  | new
  | null
  | package
  | private
  | protected
  | public
  | return
  | static
  | switch
  | unsigned
  | volatile
  | while
  | super
  | switch
  | synchronized
  | this
  | throw
  | throws
  | true
  | transient
  | try
  | void
  | volatile
  | while

@builtinTypes =
  char
  | byte
  | boolean
  | double
  | enum
  | float
  | int
  | long
  | short
  | void
  | String
  | Integer
  | Float
  | Double
  | Long

@reservedop =
  "+"  | "++"  | "+=" | "-"   | "--" | "-=" | "*"      | "*=" | "/"  | "/=" | "%"  | "%=" |
  "<"  | "<="  | ">"  | ">="  | "!=" | "==" |
  "!"  | "&&"  | "||" |
  "<<" | "<<=" | ">>" | ">>=" | "~"  | "&"  | "&="     | "|"  | "|=" | "^"  | "^=" |
  "="  | "->"  | "."  | ","   | "?"  | ":"

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

java :-

<0> $white+                                     { c defaultStyle } -- whitespace

<nestcomm> {
  "*/"                                          { m (+1) blockCommentStyle }
  $white+                                       ; -- Whitespace
  .                                             { c blockCommentStyle }
}

<0> {
  "//"[^\n]*                                    { c commentStyle }

 "/*" @reservedop*                              { m (subtract 1) blockCommentStyle }

 $special                                       { c defaultStyle }

 @keywordid                                     { c keywordStyle }
 @builtinTypes                                  { c typeStyle }
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

stateToInit :: HlState -> Int
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
