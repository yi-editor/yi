-- -*- haskell -*- 
--
-- Lexical syntax for illiterate Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--

{
{-# OPTIONS -w  #-}
module Yi.Syntax.Haskell ( initState, alexScanToken, tokenToStyle, Token(..) ) where
import Yi.Syntax.Alex
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
        as|case|class|data|default|deriving|else|hiding|if|
        import|in|infix|infixl|infixr|instance|module|newtype|
        qualified|then|type|forall|foreign|export|dynamic|
        safe|threadsafe|unsafe|stdcall|ccall|dotnet

@indentReservedId =
    of|where|let|do|mdo

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

<0> $white+                                     ;

<nestcomm> {
  "{-"                                          { m (subtract 1) Comment }
  "-}"                                          { m (+1) Comment }
  $white+                                       ; -- whitespace
  .                                             { c Comment }
}

<0> {
-- The first rule matches operators that begin with --, eg --++-- is a valid
-- operator and *not* a comment. 
-- Note that we have to dissallow '-' as a symbol char for the first one
-- of these because we may have -------- which would stilljust be the 
-- start of a comment.
  "--"\-* [$symbol # \-] $symchar*              { c Operator }
-- The next rule allows for the start of a comment basically
-- it is -- followed by anything which isn't a symbol character
-- (OR more '-'s). So for example "-----:" is still the start of a comment.
  "--"~[$symbol # \-][^$nl]*                    { c Comment }
-- Finally because the above rule had to add in a non symbol character 
-- it's also possible that we have just finishing a line,
-- people sometimes do this for example  when breaking up paragraphs
-- in a long comment.
  "--"$nl                                       { c Comment }

 "{-"                                           { m (subtract 1) Comment }

 $special                                       { \str st -> (st, Special (headLB str)) }

 @reservedid                                    { c Reserved }
 @indentReservedId                              { c IndentReserved }
 @varid                                         { c VarIdent }
 @conid                                         { c ConsIdent }

 @reservedop                                    { c Operator }
 @varsym                                        { c Operator }
 @consym                                        { c ConsOperator }

 @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal                          { c Number }

 @decimal \. @decimal @exponent?
  | @decimal @exponent                          { c Number }

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
