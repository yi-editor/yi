-- -*- haskell -*-
--
--  A poorly-written R lang lexer
--
--  This is one of the first lexers I've ever written, so this could probably be
--  rewritten much, much better.

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-} -- Alex generate warnings-ridden code.
module Yi.Lexer.R ( lexer ) where

{- Standard Library Modules Imported -}
import Yi.Lexer.Alex hiding (tokenToStyle)

{- External Library Modules Imported -}

{- Local Modules Imported -}
import qualified Yi.Syntax
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

@reservedid = auto
            | read
            | csv
            | delim
            | fwf
            | save
            | image
            | cat
            | str
            | help
            | ls
            | methods
            | format
            | write
            | table
            | sink
            | c
            | seq
            | rep
            | data
            | frame
            | list
            | arary
            | matrix
            | factor
            | gl
            | rbind
            | cbind
            | as
            | is
            | null
            | na
            | arary
            | dim
            | dimnames
            | nrow
            | ncol
            | class
            | unclass
            | attr
            | attributes
            | which
            | max
            | min
            | rev
            | sort
            | cut
            | match
            | which
            | choose
            | omit
            | fail
            | unique
            | table
            | subset
            | sample
            | sin
            | cos
            | tan
            | asin
            | acos
            | atan
            | atan2
            | log
            | log10
            | exp
            | range
            | sum
            | diff
            | prod
            | mean
            | median
            | quantile
            | weighted
            | rank
            | var
            | sd
            | cor
            | round
            | log
            | scale
            | pmin
            | pmax
            | cumsum
            | cumprod
            | cummin
            | union
            | Re
            | Im
            | Mod
            | Arg
            | Conj
            | Convolve
            | fft
            | mvfft
            | filter
            | t
            | diag
            | solve
            | rowsum
            | colsum
            | rowMeans
            | colMeans
            | apply
            | lapply
            | tapply
            | by
            | merge
            | xtabs
            | aggregate
            | stack
            | unstack
            | reshape
            | paste
            | substr
            | strsplit
            | grep
            | gsub
            | tolower
            | toupper
            | match
            | pmatch
            | nchar
            | Date
            | POSIXct
            | difftime
            | plot
            | hist
            | barplot
            | dotchart
            | pie
            | boxplot
            | sunflowerplot
            | stripplot
            | coplot
            | interaction
            | matplot
            | fourfoldprot
            | assocplot
            | mosaicplot
            | pairs
            | ts
            | qqnorm
            | qqplot
            | contour
            | filled
            | image
            | persp
            | stars
            | symbols
            | termplot
            | FALSE
            | TRUE
            | xlim
            | ylim
            | xlab
            | ylab
            | main
            | points
            | lines
            | text
            | mtext
            | segments
            | arrows
            | abline
            | rect
            | polygon
            | legend
            | title
            | axis
            | rug
            | locator
            | adj
            | bg
            | cex
            | col
            | font
            | las
            | lty
            | lwd
            | mar
            | mfcol
            | mfrow
            | pch
            | ps
            | ptf
            | tck
            | tcl
            | xaxt
            | yaxt
            | xyplot
            | barchat
            | dotplot
            | densityplot
            | histogram
            | bwplot
            | qqmath
            | stripplot
            | qq
            | splom
            | parallel
            | levelplot
            | wireframe
            | cloud
            | optim
            | nlm
            | lm
            | glm
            | nls
            | approx
            | spline
            | loess
            | predict
            | df
            | residual
            | coef
            | residuals
            | deviance
            | fitted
            | logLik
            | AIC
            | aov
            | anova
            | density
            | binom
            | test
            | pairwise
            | power
            | prop
            | search
            | rnorm
            | rexp
            | rgamma
            | rpois
            | rweibull
            | rcauchy
            | rbeta
            | rt
            | rf
            | rchisq
            | rbinom
            | rgeom
            | rhyper
            | rlogis
            | rlnorm
            | rnbinom
            | runif
            | rwilcox
            | function
            | return
            | if
            | for
            | while
            | repeat
            | break
            | next
            | ifelse
            | do
            | call
            | case
            | char
            | continue
            | default
            | else
            | switch
            | union

@reservedop = "%*%" | "%in%" |
  "+"  | "++"  | "+=" | "-"   | "--" | "-=" | "*"      | "*=" | "/"  | "/=" | "%"  | "%=" |
  "<"  | "<="  | ">"  | ">="  | "!=" | "==" |
  "!"  | "&&"  | "||" |
  "<<" | "<<=" | ">>" | ">>=" | "~"  | "&"  | "&="     | "|"  | "|=" | "^"  | "^=" |
  "="  | "->"  | "."  | ","   | "?"  | ":"  | "sizeof"
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
c :-
<0> $white+                                     { c defaultStyle } -- whitespace
<nestcomm> {
  -- We could do nested comments like this
  -- "/*"                                       { m (subtract 1) blockCommentStyle }
  "*/"                                          { m (+1) blockCommentStyle }
  $white+                                       ; -- Whitespace
  .                                             { c blockCommentStyle }
}
<0> {
  "//"[^\n]*                                    { c commentStyle }
  "/*".*"*/"                                    { c blockCommentStyle }

 "/*" @reservedop*                              { m (subtract 1) blockCommentStyle }

 $special                                       { c defaultStyle }

 @reservedid                                    { c keywordStyle }
 @varid                                         { c defaultStyle }
 @conid                                         { c typeStyle }

 @reservedop                                    { c operatorStyle }
 @varsym                                        { c operatorStyle }
 @consym                                        { c typeStyle }

 @decimal
  | 0[oO] @octal
  | 0[xX] @hexadecimal                          { c defaultStyle }

 @decimal \. @decimal @exponent?
  | @decimal @exponent                          { c defaultStyle }

 \' ($graphic # [\'\\] | " " | @escape) \'      { c stringStyle }
 \" @string* \"                                 { c stringStyle }
 .                                              { c operatorStyle }
}
{
type HlState = Int
type Token = StyleName
lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }
stateToInit :: HlState -> Int
stateToInit x | x < 0     = nestcomm
              | otherwise = 0
initState :: HlState
initState = 0
#include "common.hsinc"
}
