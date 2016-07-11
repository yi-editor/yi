-- Lexical syntax for clojure.
--

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.Clojure ( lexer ) where

import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style

}

-- generic (copied from Java.x)

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\:\[\]\`\{\}]

$ascdigit     = 0-9
$unidigit     = [] -- TODO
$digit        = [$ascdigit $unidigit]

$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol    = [] -- TODO
$symbol       = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large        = [A-Z \xc0-\xd6 \xd8-\xde]
$small        = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha        = [$small $large]

$graphic      = [$small $large $symbol $digit $special \"\']
$literal      = [$alpha $symbol $digit $special]

$nonzerodigit = 1-9
$octit        = 0-7
$hexit        = [0-9 A-F a-f]
$idchar       = [$alpha $digit]
$symchar      = [$symbol]
$nl           = [\n\r]

@digits       = $nonzerodigit $digit*
@integer      = @digits

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL

$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | $ascdigit | o $octit | x $hexit)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

-- clojure-specific

$nonnumericcljsymchar = [[\*\+\!\-\_\?\:] $alpha]
$cljsymchar    = [[\*\+\!\-\_\?\:] $alpha $digit]

@cljsymbol =
  $nonnumericcljsymchar [$cljsymchar \.]*

@cljkeyword =
  $nonnumericcljsymchar $cljsymchar*

@loop =
    for | doseq | dotimes | while

@branch =
    and | or | when | when\-not | when\-let | when\-first | if\-not | if\-let | cond
  | condp | case | when\-some | if\-some

@reservedid =
    def | defn | defn\- | fn\? | ifn\?
  | definline | defmacro
  | defprotocol | defrecord | reify | extend\-type
  | ns | let | binding

@keywordid =
    @reservedid | @branch | @loop

@characterliterals =
    c | newline | space | tab | formfeed | backspace | "return"

clojure :-

<regex> {
 "                                             { m (\x -> 0) regexStyle }
 $white+                                       ; -- Whitespace
 -- character literals
 \\ @characterliterals
   | \\ $literal
   -- unicode characters
   | \\ u $digit{4}
   -- octal characters
   | \\ o $digit{3}                             { c importStyle }

 .                                             { c regexStyle }
}
<string> {
 "                                             { m (\x -> 0) stringStyle }
 $white+                                       ; -- Whitespace
 -- character literals
 \\ @characterliterals
   | \\ $literal
   -- unicode characters
   | \\ u $digit{4}
   -- octal characters
   | \\ o $digit{3}                             { c importStyle }

 .                                             { c stringStyle }
}
<0> {
 $white+                                        { c defaultStyle }
  ";"[^\n]*                                     { c commentStyle }

 @keywordid                                     { c keywordStyle }

 $special                                       { c defaultStyle }

 -- literal literals
 "true"
   | "false"
   | "nil"                                      { c importStyle }

 -- character literals
 \\ @characterliterals
   | \\ $literal
   -- unicode characters
   | \\ u $digit{4}
   -- octal characters
   | \\ o $digit{3}                             { c importStyle }

 -- numeric literals
 @digits
   | @digits \N
   | @digits \/ @digits
   | @digits r @digits
   | @digits \. @digits? \M
   | @digits \. @digits?                        { c numberStyle }

 -- keywords
 ":"+ @cljkeyword                               { c preprocessorStyle }

 -- symbols
 @cljsymbol
   | @cljsymbol \/ @cljsymbol                   { c variableStyle }

 -- simple string handling
 "                                              { m (\x -> 1) stringStyle }
 \#"                                            { m (\x -> 2) regexStyle }

 .                                              { c operatorStyle }
}

{

type HlState = Int
type Token = StyleName

stateToInit :: HlState -> Int
stateToInit x | x == 1 = string
              | x == 2 = regex
              | otherwise = 0


initState :: HlState
initState = 0

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
