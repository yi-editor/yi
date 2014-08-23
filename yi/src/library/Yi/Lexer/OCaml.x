-- -*- haskell -*-
--
-- Lexical syntax for OCaml
--
-- (c) Nicolas Pouillard 2008
--

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.OCaml ( lexer, Token(..) ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}] -- OCAML CHECK

$digit     = 0-9

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~] -- OCAML CHECK
$symbol    = $ascsymbol # [$special \_\:\"\'] -- OCAML CHECK

$large     = [A-Z \xc0-\xd6 \xd8-\xde] -- OCAML CHECK
$small     = [a-z \xdf-\xf6 \xf8-\xff \_] -- OCAML CHECK
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\'] -- OCAML CHECK

$bit       = 0-1
$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:] -- OCAML CHECK
$nl        = [\n\r]

@revisedReservedId =
        value|declare

@reservedid =
        and|as|asr|assert|begin|class|constraint|do|done|downto|else|end|
        exception|external|as|assert|false|for|fun|function|if|in|include|
        inherit|initializer|land|lor|lxor|lsl|lsr|lazy|let|match|method|mod|
        module|mutable|new|object|of|open|or|parser|private|raise|rec|sig|
        struct|then|to|true|try|type|val|virtual|when|while|with|
        @revisedReservedId

@reservedop =
        "=" | ":=" | "?" | \\ | "|" | "<-" | "->" | "~" -- OCAML CHECK

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \:\: $symchar*

@binary      = $bit+
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

@number = @decimal
        | 0[bB] @binary
        | 0[oO] @octal
        | 0[xX] @hexadecimal
        | @decimal \. @decimal @exponent?
        | @decimal @exponent
@numberPostfix = [lLn]?

-- OCAML CHECK
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

ocaml :-

<0> $white+                                     ;

<nestcomm> {
  "(*"                                          { m (subtract 1) Comment }
  "*)"                                          { m (+1) Comment }
  $white+                                       ; -- whitespace
  .                                             { c Comment }
}

<0> {

 "(*"                                           { m (subtract 1) Comment }

 $special                                       { \str st -> (st, Special (snd $ head str)) }

 @reservedid                                    { c Reserved }
 @varid                                         { c VarIdent }
 @conid                                         { c ConsIdent }

 @reservedop                                    { c Operator }
 @varsym                                        { c Operator }
 @consym                                        { c ConsOperator }

 @number @numberPostfix                         { c Number }

 \' ($graphic # [\'\\] | " " | @escape) \'      { c CharTok }
 \" @string* \"                                 { c StringTok }
 .                                              { c Operator }
}

{

type HlState = Int

data Token = Number | CharTok | StringTok | VarIdent | ConsIdent
           | IndentReserved | Reserved | ReservedOp | Special Char
           | ConsOperator | Operator
           | Comment
             deriving (Eq, Show)

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = tokenToStyle
  , _styleLexer = commonLexer alexScanToken initState
  }

tokenToStyle :: Token -> StyleName
tokenToStyle tok = case tok of
  Number       -> defaultStyle
  CharTok      -> stringStyle
  StringTok    -> stringStyle
  VarIdent     -> defaultStyle
  ConsIdent    -> typeStyle
  ReservedOp   -> operatorStyle
  Reserved     -> keywordStyle
  IndentReserved -> keywordStyle
  Special _    -> defaultStyle
  ConsOperator -> typeStyle
  Operator     -> operatorStyle
  Comment      -> commentStyle


stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState :: HlState
initState = 0

#include "common.hsinc"
}
