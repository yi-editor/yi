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
    | redo
    | for
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
    | defined
    | undef
    | exists
    | die

@seperator = $whitechar+ | $special
@interpVarSeperator = [^$idchar] | $nl

@reservedop = 
  "->" | "*" | "+" | "-" | "%" | \\ | "||" | "&&" | "?" | ":" | "=>" 
  | "or" | "and" | "ne" | "eq"
  | "=~" | "!~"

-- Standard variables
-- TODO: Handle casts of the form @varTypeOp{@varid}
@varTypeOp =
    "@"
    | "$"
    | "%"

@varPackageSpec = $idchar+ "::" 
@varid  = @varTypeOp+ @varPackageSpec* $idchar+

-- TODO: A lot! There is a whole list of special global variables.
@specialVarToken = 
    "_" | ARG
    | "." | INPUT_LINE_NUMBER | NR
    | "/" | INPUT_RECORD_SEPARATOR | RS
    | "?" | CHILD_ERROR
    | ENV

@specialVarId = @varTypeOp @specialVarToken

-- Standard classes
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

-- string components
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&\`]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\

@nonInterpolatingString  = $graphic # [\'] | " " 

-- Heredoc
@heredocId = [$large '_']+

-- Perldoc
-- perldoc starts at a "line that begins with an equal sign and a word"
-- (man perlsyn)
@perlDocStartWord = "=" [^$whitechar]+

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

    -- Chunks that are handled as interpolating strings.
    \"
    { 
        m (\_ -> HlInInterpString "\"" ) operatorStyle 
    }
    "`" 
    {
        m (\_ -> HlInInterpString "`" ) operatorStyle
    }

    -- Heredocs are kinda like interpolating strings...
    "<<" @heredocId
    { 
        \str _ -> (HlInHeredoc (drop 2 $ fmap snd str), operatorStyle)
    }

    -- Chunks that are handles as non-interpolating strings.
    \' 
    { 
        m (\_ -> HlInString) operatorStyle 
    }
    "qw(" @nonInterpolatingString* ")"             { c stringStyle }

    -- perldoc starts at a "line that begins with an equal sign and a word"
    -- (man perlsyn)
    ^ @perlDocStartWord
        {
            m (\_ -> HlInPerldoc) commentStyle
        }

    -- Everything else is unstyled.
    .                                              { c defaultStyle }
}

<interpString>
{
    @escape { c defaultStyle }
    $white+ { c defaultStyle }
    @varid / @interpVarSeperator
        { 
            c [Foreground purple] 
        }
    @specialVarId / @interpVarSeperator                     
        { 
            c [Foreground cyan] 
        }
    ./
        {
            \state preInput _ _ ->
                case state of
                    HlInInterpString end_tag ->
                        let inputText = take (length end_tag) $ alexCollectChar preInput
                        in if (inputText == end_tag) then True else False
                    _ -> False
        }
        {
            m (\_ -> HlInCode) operatorStyle
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

<perldoc>
{
    ^ "=cut" $ 
        { 
            m (\_ -> HlInCode) commentStyle 
        }
    $white+ { c defaultStyle }
    . { c commentStyle }
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
    | HlInInterpString String
    | HlInString
    | HlInHeredoc String
    | HlInPerldoc

type Token = Style

stateToInit HlInCode = 0
stateToInit (HlInInterpString _) = interpString
stateToInit HlInString = string
stateToInit (HlInHeredoc _) = heredoc
stateToInit HlInPerldoc = perldoc

initState :: HlState
initState = HlInCode

#include "alex.hsinc"
}
