-- -*- haskell -*-
--
--  A Ruby lexer
--
--  This is based on the Python lexer.

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.Ruby ( lexer ) where

import Yi.Lexer.Alex hiding (tokenToStyle)
import qualified Yi.Syntax
import Yi.Style

}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}\:]

$ascdigit     = 0-9
$unidigit     = [] -- TODO
$digit        = [$ascdigit $unidigit]

$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol    = [] -- TODO
$symbol       = [$ascsymbol $unisymbol] # [$special \_]

$large        = [A-Z \xc0-\xd6 \xd8-\xde]
$small        = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha        = [$small $large]

$graphic      = [$small $large $symbol $digit $special \"\']

$nonzerodigit = 1-9
$octit        = 0-7
$hexit        = [0-9 A-F a-f]
$idchar       = [$alpha $digit]
$symchar      = [$symbol]
$nl           = [\n\r]

$longintegersuffix = [lL]

@builtinValues =
    true
  | false
  | nil
  | self

@builtinVars =
    deferr
  | defout
  | stderr
  | stdin
  | stdout
  | DEBUG
  | FILENAME
  | KCODE
  | LOADED_FEATURES
  | LOAD_PATH
  | PROGRAM_NAME
  | SAFE
  | VERBOSE

-- @builtinConstants =

@importst =
    import
  | include
  | require
  | require_relative

@reservedid =
    @builtinVars
  | abort
  | and
  | alias
  | assert
  | begin
  | break
  | case
  | class
  | continue
  | def
  | do
  | elsif
  | else
  | end
  | ensure
  | except
  | exec
  | exit
  | for
  | fork
  | from
  | gets
  | global
  | if
  | in
  | lambda
  | loop
  | next
  | not
  | or
  | pass
  | print
  | puts
  | raise
  | redo
  | rescue
  | retry
  | return
  | super
  | trap
  | try
  | undef
  | unless
  | until
  | when
  | while
  | with
  | yield

@compop =
  "<=" | ">=" | "==" | "<" | ">" | "<>"

@infarithop =
  "+" | "-" | "*" | "/" | "//" | "%" | "&" | "|" | "^" | ">>" | "<<" | "**"

-- This is separated so the infix operators above can be used with the augmented assignment form"
@prefarithop = "~"

@assignop = @infarithop? "="

@reservedop =
  @compop | @prefarithop | @assignop

@varid  = $alpha $idchar*
@varsym = $symbol+

@digits = $nonzerodigit $digit*
@octits = "0"  $octit
@hexits = "0x" $hexit

@integer     = @digits | @octits | @hexits
@longinteger = @integer $longintegersuffix
@exponent    = [eE] [\-\+] @integer
@number      = @integer | @longinteger
@predicates  = $small* \?

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @number)
@gap     = \\ $whitechar+ \\

@shortstring = $graphic # [\"\'\\] | " " | @escape | @gap
@longstring  = @shortstring | $nl

main :-

<0> {
 $white+                                        { c defaultStyle }

 "#"[^\n]*                                      { c commentStyle }
 $special                                       { c defaultStyle }
 @importst                                      { c importStyle  }

 @reservedid                                    { c keywordStyle }
 @builtinValues                                 { c typeStyle }
 @predicates                                    { c operatorStyle }

 -- classes and modules
 [A-Z] $alpha*                                  { c builtinStyle }
 @varid                                         { c variableStyle }
 @reservedop                                    { c operatorStyle }

 @number @exponent?
   | @number \. @number? @exponent?             { c numberStyle }

 -- symbols and raw strings
 :[$small]+                                     { c stringStyle }
 \%q\{ @longstring* \}
 | \' @shortstring* \'                          { c stringStyle }

 -- interpolated strings
 \%\{ @longstring* \}
 | \%\/ @longstring* \/
 | \%Q\{ @longstring* \}
 | \" @shortstring* \"                          { c stringStyle }

 .                                              { c operatorStyle }
}


{

type HlState = Int
type Token = StyleName

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
