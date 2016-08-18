-- -*- haskell -*-

{
#define NO_ALEX_CONTEXTS
{- The Ott website: http://www.cl.cam.ac.uk/~pes20/ott -}

{-# OPTIONS -w  #-}
module Yi.Lexer.Ott  ( lexer ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
  ( Style             ( .. )
  , StyleName
  )
import Yi.Style
}

@reservedid = metavar
            | indexvar
            | grammar
            | embed
            | subrules
            | contextrules
            | substitutions
            | single
            | multiple
            | freevars
            | defns
            | defn
            | by
            | homs
            | funs
            | fun
            | parsing
            | left
            | right

@reservedop = "::"
            | "::="
            | "_::"
            | "<::"
            | "<="
            | "/>"
            | "//"
            | "</"
            | IN
            | ".."
            | "..."
            | "...."
            | "----" "-"*

@homid = tex | com | coq | hol | isa | ocaml | icho | ich | "coq-equality"
       | isasyn | isaprec | lex | texvar | isavar | holvar | ocamlvar

main :-

<0> $white+                                     ;

<0>
{
    $white+                                        ; -- whitespace
    "%"[^\n]*                                      { c commentStyle }
    ">>"                                           { m (subtract 1) commentStyle }
    @reservedid                                    { c keywordStyle }
    @reservedop                                    { c operatorStyle }
    "|" $white+                                    { c operatorStyle }
    "(+"                                           { m (const bindspec) numberStyle  }
    "{{"                                           { m (const beginHom) stringStyle }
    .                                              { c defaultStyle }
}

<nestcomm> {
  ">>"                                          { m (subtract 1) commentStyle }
  "<<"                                          { m (+1) commentStyle }
  $white+                                       ; -- whitespace
  .                                             { c commentStyle }
}

<bindspec> {
  "bind" | "in" | "union" | "{" | "}"           { c numberStyle }
  $white+                                       ; -- whitespace
  "+)"                                          { m (const 0) numberStyle }
  .                                             { c defaultStyle }
}

<beginHom> {
  $white+                                       ; -- whitespace
  @homid                                        { m (const hom) typeStyle }
  "}}"                                          { m (const 0) stringStyle }
  .                                             { m (const hom) defaultStyle }
}

<hom> {
  "[["                                          { m (const splice) stringStyle }
  $white+                                       ; -- whitespace
  "}}"                                          { m (const 0) stringStyle }
  .                                             { c defaultStyle }
}

<splice> {
  "]]"                                          { m (const hom) stringStyle }
  $white+                                       ; -- whitespace
  .                                             { c defaultStyle }
}

{
type HlState = Int

stateToInit x | x < 0     = nestcomm
              | otherwise = x

initState :: HlState
initState = 0

type Token = StyleName

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = id
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
