-- -*- haskell -*-
--
-- Lexical syntax for OCaml
--
-- (c) Nicolas Pouillard 2008
--

{
{-# OPTIONS -w  #-}
module Yi.Lexer.OCaml ( initState, alexScanToken, tokenToStyle, Token(..) ) where
import Yi.Lexer.Alex
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
        and|as|assert|class|constraint|else|exception|external|fun|function|
        if|in|inherit|initializer|land|lazy|let|match|method|mutable|module|
        new|of|parser|private|raise|rec|try|type|val|virtual|when|while|with|
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

tokenToStyle :: Token -> Style
tokenToStyle tok = case tok of
  Number       -> defaultStyle
  CharTok      -> stringStyle
  StringTok    -> stringStyle
  VarIdent     -> defaultStyle
  ConsIdent    -> upperIdStyle
  ReservedOp   -> operatorStyle
  Reserved     -> keywordStyle
  IndentReserved -> keywordStyle
  Special _    -> defaultStyle
  ConsOperator -> upperIdStyle
  Operator     -> operatorStyle
  Comment      -> commentStyle


stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState :: HlState
initState = 0

#include "alex.hsinc"
}
