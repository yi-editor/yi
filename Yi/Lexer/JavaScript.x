-- -*- haskell -*-
--
-- Simple lexer for JavaScript 1.7
-- (C) Copyright 2009 Deniz Dogan
--
-- Note that this mode is for JavaScript 1.7, i.e. Mozilla, not ECMAScript, nor
-- JScript, but it does take much of its information from the ECMAScript
-- specification.
--
-- Things about JavaScript deserve to be mentioned:
-- * JavaScript explicitly forbids nested comments, so we don't have to care
--   about the "level" of nesting in multiline comments
--
-- Sources for reserved words:
-- https://developer.mozilla.org/En/Core_JavaScript_1.5_Reference/Reserved_Words
-- https://developer.mozilla.org/en/New_in_JavaScript_1.6
-- https://developer.mozilla.org/en/New_in_JavaScript_1.7
--
-- Sources for the general syntax:
-- http://en.wikipedia.org/wiki/JavaScript_syntax
-- http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf

{

{-# OPTIONS -w  #-}

module Yi.Lexer.JavaScript ( initState,
                             alexScanToken,
                             tokenToStyle ) where

import Yi.Lexer.Alex
import qualified Yi.Syntax
import Yi.Style

}

$whitechar  = [\ \t\n\r\f\v]

@builtin    = true | false | null
@reservedid = break | case | catch | continue | default | delete | do | else
            | finally | for | function | if | in | instanceof | new | return
            | switch | this | throw | try | typeof | var | void | while | with
            | @builtin

@ops = "+"   | "-"   | "*"   | "/"   | "%"   | "++"  | "--"  | "="   | "+="
     | "-="  | "*="  | "/="  | "%="  | "=="  | "!="  | ">"   | ">="  | "<"
     | "<="  | "===" | "!==" | "&&"  |"||"   | "!"   | "&"   | "|"   | "^"
     | "<<"  | ">>"  | ">>>" | "~"

$small     = [a-z]
$large     = [A-Z]
$special   = [\(\)\,\;\[\]\`\{\}\.\:]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\_]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \:\"\']

$graphic   = [$small $large $symbol $digit $special \:\"\']

@number  = $digit+
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | HT
         | LF  | VT  | FF  | CR  | SO  | SI  | DLE | DC1 | DC2 | DC3 | DC4 | NAK
         | SYN | ETB | CAN | EM  | SUB | ESC | FS  | GS  | RS  | US  | SP  | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @number)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap
@varid   = [a-z0-9\_\$] [a-zA-Z0-9\_\$]* -- TODO: Unicode
@const   = [A-Z0-9\_\$] [a-zA-Z0-9\_\$]* -- Constants and constructors

haskell :-

<0> {

$whitechar+            ;
$special               { cs $ Special }

@number                { cs $ Number }
@ops                   { cs $ Operator }
@reservedid            { cs $ Res }
@varid                 { cs $ Variable }
@const                 { cs $ Const }

\" @string* \"         { cs $ Str }
\' @string* \'         { cs $ Str }
"//".*                 { c  $ Comment Line }
"/*"                   { m (subtract 1) $ Comment Start }

.                      { cs $ Unknown }

}

<nestcomm> {

"*/"    { m (+1) $ Comment End }
.       { c $ Comment Text }
$whitechar+ ;

}


{

data CommentType = Line | Start | End | Text

tokenToStyle (Comment Line) = commentStyle
tokenToStyle (Comment _)    = blockCommentStyle
tokenToStyle (Const _)      = typeStyle
tokenToStyle (Number _)     = numberStyle
tokenToStyle (Operator _)   = defaultStyle
tokenToStyle (Res _)        = keywordStyle
tokenToStyle (Special _)    = defaultStyle
tokenToStyle (Str _)        = stringStyle
tokenToStyle (Unknown _)    = errorStyle
tokenToStyle (Variable _)   = defaultStyle

type HlState = Int
data Token = Unknown !String
           | Res !String
           | Str !String
           | Operator !String
           | Special !String
           | Number !String
           | Variable !String
           | Comment !CommentType
           | Const !String

stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState :: HlState
initState = 0

#include "alex.hsinc"
}
