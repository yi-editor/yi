-- -*- haskell -*-
--
--  A poorly-written Python lexer
--
--  This is one of the first lexers I've ever written, so this could probably be
--  rewritten much, much better.

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.Python ( lexer ) where

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

$strprefix         = [urUR]
$longintegersuffix = [lL]

@builtins =
    False
  | None
  | True

@importst = import

@reservedid =
    @builtins
  | and
  | as
  | assert
  | break
  | class
  | continue
  | def
  | del
  | elif
  | else
  | except
  | exec
  | finally
  | for
  | from
  | global
  | if
  | in
  | is
  | lambda
  | not
  | or
  | pass
  | print
  | raise
  | return
  | try
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

$cntrl   = [$large \@\[\\\]\^\_]

main :-

<0> {
 $white+                                        { c defaultStyle }

 "#"[^\n]*                                      { c commentStyle }

 $special                                       { c defaultStyle }

 @importst                                      { c importStyle  }

 @reservedid                                    { c keywordStyle }
 @varid                                         { c defaultStyle }

 @reservedop                                    { c operatorStyle }
-- @varsym                                        { c operatorStyle }

 @number @exponent?
   | @number \. @number? @exponent?             { c numberStyle }

 $strprefix* \"                                 { m (const DoubleQuotes) stringStyle }
 $strprefix* \'                                 { m (const SingleQuotes) stringStyle }
 $strprefix* \" \" \"                           { m (const DoubleDoc) stringStyle }
 $strprefix* \' \' \'                           { m (const SingleDoc) stringStyle }

 .                                              { c operatorStyle }
}

<doublequotes> {
  \"                                            { m (const Base) stringStyle }
  .                                             { c stringStyle }
}

<singlequotes> {
  \'                                            { m (const Base) stringStyle }
  .                                             { c stringStyle }
}

<doubledoc> {
  \" \" \"                                      { m (const Base) stringStyle }
  .                                             { c stringStyle }
}

<singledoc> {
  \' \' \'                                      { m (const Base) stringStyle }
  .                                             { c stringStyle }
}

{
data HlState = Base | DoubleQuotes | SingleQuotes | DoubleDoc | SingleDoc
    deriving (Eq, Show)

type Token = StyleName

stateToInit Base = 0
stateToInit DoubleQuotes = doublequotes
stateToInit SingleQuotes = singlequotes
stateToInit DoubleDoc = doubledoc
stateToInit SingleDoc = singledoc

initState :: HlState
initState = Base

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
