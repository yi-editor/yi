-- -*- haskell -*-
--
-- Lexical syntax for Cabal files
--
-- History:
--   Adapted from the Haskell lexical syntax by Allan Clark
--   Adapted to follow more closely the Cabal tool by Nicolas Pouillard
--

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Cabal ( lexer ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
  ( Style             ( .. )
  , StyleName
  , defaultStyle
  , commentStyle
  , blockCommentStyle
  , keywordStyle
  , operatorStyle
  , typeStyle
  , stringStyle
  , numberStyle
  )
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
   GPL
  |LGPL
  |BSD3
  |BSD4
  |PublicDomain
  |AllRightsReserved
  |OtherLicense
  |if
  |[Ff]lag
  |else
  |[Oo][Ss]
  |[Aa]rch
  |[Tt]rue
  |[Ff]alse
  |[Ii]mpl

@fieldid =
   [Aa]uthor
  |[Bb]ug\-[Rr]eports
  |[Bb]uild\-[Dd]epends
  |[Bb]uild\-[Tt]ype
  |[Bb]uild\-[Tt]ools
  |[Bb]uildable
  |[Cc]\-[Ss]ources
  |[Cc][Cc]\-[Oo]ptions
  |[Cc]abal\-[Vv]ersion
  |[Cc]ategory
  |[Cc]opyright
  |[Dd]ata\-[Dd]ir
  |[Dd]ata\-[Ff]iles
  |[Dd]efault
  |[Dd]escription
  |[Ee]xecutable
  |[Ee]xposed
  |[Ee]xposed\-[Mm]odules
  |[Ee]xtensions
  |[Ee]xtra\-[Ll]ibraries
  |[Ee]xtra\-[Ll]ib\-[Dd]irs
  |[Ee]xtra\-[Ss]ource\-[Ff]iles
  |[Ee]xtra\-[Tt]mp\-[Ff]iles
  |[Ff]rameworks
  |[Gg][Hh][Cc]\-[Oo]ptions
  |[Gg][Hh][Cc]\-[Pp]rof\-[Oo]ptions
  |[Gg][Hh][Cc]\-[Ss]hared\-[Oo]ptions
  |[Hh][Uu][Gg][Ss]\-[Oo]ptions
  |[Nn][Hh][Cc]98\-[Oo]ptions
  |[Hh]omepage
  |[Hh][Ss]\-[Ss]ource\-[Dd]irs
  |[Ii]nclude\-[Dd]irs
  |[Ii]ncludes
  |[Ii]nstall\-[Ii]ncludes
  |[Ll]icense
  |[Ll]icense\-[Ff]ile
  |[Ll][Dd]\-[Oo]ptions
  |[Mm]ain\-[Ii]s
  |[Mm]aintainer
  |[Nn]ame
  |[Oo]ther\-[Mm]odules
  |[Pp]ackage\-[Uu][Rr][Ll]
  |[Pp]kgconfig\-[Dd]epends
  |[Ss]tability
  |[Ss]ynopsis
  |[Tt]ested\-[Ww]ith
  |[Vv]ersion

@sourcerepofieldid =
   [Tt]ype
  |[Ll]ocation
  |[Mm]odule
  |[Bb]ranch
  |[Tt]ag
  |[Ss]ubdir

@reservedop =
        ">" | ">=" | "<" | "<="

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

main :-

<0> {
 [\ \t]+                                        { c defaultStyle }
 $nl+                                           { c defaultStyle }

 ^ [\ \t]* "--" [^$nl]* $                       { c commentStyle }

 $special                                       { c defaultStyle }

 @reservedid                                    { c keywordStyle }
 @varid                                         { c defaultStyle }
 @conid                                         { c defaultStyle }

 @fieldid ":"                                   { c typeStyle }
 @sourcerepofieldid ":"                         { c typeStyle }

 @reservedop                                    { c operatorStyle }
 @varsym                                        { c operatorStyle }
 @consym                                        { c defaultStyle  }

 @decimal
  | 0[oO] @octal
  | 0[xX] @hexadecimal                          { c defaultStyle }

 @decimal \. @decimal                           { c defaultStyle }

 \' ($graphic # [\'\\] | " " | @escape) \'      { c stringStyle }
 \" @string* \"                                 { c stringStyle }
 .                                              { c operatorStyle }
}

{
type HlState = ()
type Token = StyleName

stateToInit () = 0

initState :: HlState
initState = ()

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
