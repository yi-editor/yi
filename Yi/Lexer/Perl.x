-- -*- haskell -*- 
-- Simple lexer for Perl source files.
-- This started as a copy of the C++ lexer so some bits and pieces don't make sense for Perl.

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Perl ( initState, alexScanToken ) where
{- Standard Library Modules Imported -}
import Yi.Lexer.Alex
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Yi.Syntax
import Yi.Style
{- End of Module Imports -}

}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\{\}]

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
$idchar    = [$alpha $ascdigit]
$nl        = [\n\r]

@reservedId = 
    if
    | while
    | do
    | then
    | last
    | next
    | redo
    | continue
    | goto
    | foreach
    | unless
    | until
    | elsif
    | else
    | sub
    | package
    | use
    | require
    | our
    | my

@seperator = $whitechar+ | $special
@interpVarSeperator = [^$idchar] | $nl

@reservedop = 
  "->" | "*" | "+" | "-" | "%" | \\ | "||" | "&&" | "?" | ":" | "=>" | "or" | "and"

@varTypeOp =
    "@"
    | "$"
    | "%"

@varPackageSpec = $idchar+ "::" 
@varid  = @varTypeOp+ @varPackageSpec* $idchar+

@specialVarToken = 
    "_" | ARG
    | "." | INPUT_LINE_NUMBER | NR
    | "/" | INPUT_RECORD_SEPARATOR | RS
    | "?" | CHILD_ERROR
    | ENV

@specialVarId = @varTypeOp @specialVarToken

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

@nonInterpolatingString  = $graphic | " " 
@interpolatingString  = $graphic # [\"\\] | " " | @escape | @gap

@heredocId = [$large '_']+

perlHighlighterRules :-

<0> 
{
    $white+                                        { c defaultStyle } -- whitespace

    "#"[^\n]*                                      { c commentStyle }

    @seperator @reservedId / @seperator            { c keywordStyle }
    ^ @reservedId / @seperator                     { c keywordStyle }
    @varid / @seperator                            { c [Foreground purple] }
    @specialVarId / @seperator                     { c [Foreground cyan] }

    @reservedop                                    { c operatorStyle }

    @decimal 
    | 0[oO] @octal
    | 0[xX] @hexadecimal                           { c defaultStyle }

    @decimal \. @decimal @exponent?
    | @decimal @exponent                           { c defaultStyle }

    \"
    { 
        m (\_ -> HlInInterpString) operatorStyle 
    }
    "<<" @heredocId
    { 
        \str _ -> (HlInHeredoc (drop 2 $ fmap snd str), operatorStyle)
    }
    "`" @interpolatingString* "`"                  { c stringStyle }
    \' 
    { 
        m (\_ -> HlInString) operatorStyle 
    }
    "qw(" @nonInterpolatingString* ")"             { c stringStyle }
    .                                              { c defaultStyle }
}

<interpString>
{
    [^\\]^\"
        {
            m (\_ -> HlInCode) operatorStyle
        }
    $white+ { c defaultStyle }
    @varid / @interpVarSeperator
        { 
            c [Foreground purple] 
        }
    @specialVarId / @interpVarSeperator                     
        { 
            c [Foreground cyan] 
        }
    .   { c stringStyle }
}

<heredoc>
{
    ^(@heredocId\n)/
        {
            \state preInput _ _ ->
                case state of
                    HlInHeredoc tag ->
                        let inputText = take (length tag) $ alexCollectChar preInput
                        in if (inputText == tag) then True else False
                    _ -> False
        }
        {
            m (\_ -> HlInCode) operatorStyle
        }
    $white+ { c defaultStyle }
    @varid / @interpVarSeperator
        { 
            c [Foreground purple] 
        }
    @specialVarId / @interpVarSeperator                     
        { 
            c [Foreground cyan] 
        }
    .   { c stringStyle }
}

<string>
{
    \'
        {
            m (\_ -> HlInCode) operatorStyle
        }
    $white+ { c defaultStyle }
    .   { c stringStyle }
}

{

data HlState = 
    HlInCode
    | HlInInterpString
    | HlInString
    | HlInHeredoc String

type Token = Style

stateToInit HlInCode = 0
stateToInit HlInInterpString = interpString
stateToInit HlInString = string
stateToInit (HlInHeredoc _) = heredoc

initState :: HlState
initState = HlInCode

#include "alex.hsinc"
}
