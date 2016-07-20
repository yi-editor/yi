-- -*- haskell -*-
--
-- Lexical syntax for JSON
--

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.JSON ( lexer, Token(..)) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}\:]

$ascdigit     = 0-9
$unidigit     = [] -- TODO
$digit        = [$ascdigit $unidigit]

$large        = [A-Z \xc0-\xd6 \xd8-\xde]
$small        = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha        = [$small $large]

$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol    = [] -- TODO
$symbol       = [$ascsymbol $unisymbol] # [$special \_]

$graphic      = [$small $large $symbol $digit $special \"\']

$nonzerodigit = 1-9
$octit        = 0-7
$hexit        = [0-9 A-F a-f]
$idchar       = [$alpha $digit]
$symchar      = [$symbol]

@digits = $nonzerodigit $digit*
@octits = "0"  $octit
@hexits = "0x" $hexit

@integer     = @digits | @octits | @hexits
@longinteger = @integer
@number      = @integer | @longinteger

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL

$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @number)
@string = $graphic # [\"\\] | " " | @escape

json :-
<0> {
    $white+                  { c defaultStyle }
    null                     { c keywordStyle }
    \" @string* \"           { c stringStyle }
    @number
      | @number \. @number?  { c numberStyle }
    .                        { c defaultStyle }
}

{

type HlState = Int
type Token = StyleName

stateToInit :: HlState -> Int
stateToInit x = 0

initState :: HlState
initState = 0

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
