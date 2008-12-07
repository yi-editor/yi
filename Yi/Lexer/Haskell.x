-- -*- haskell -*-
--
-- Lexical syntax for illiterate Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Haskell ( initState, alexScanToken, tokenToStyle, 
                          TT, isErrorTok, isSpecial,
                          startsLayout, isComment, Token(..), CommentType(..), ReservedType(..), OpType(..) ) where
import Yi.Lexer.Alex
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
        as|case|class|data|default|else|hiding|if|
        import|in|infix|infixl|infixr|instance|newtype|
        qualified|then|type|family|forall|foreign|export|dynamic|
        safe|threadsafe|unsafe|stdcall|ccall|dotnet

@layoutReservedId =
    of|let|do|mdo


@reservedop =
        ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
@anyid = (@varid | @conid)
@qual   = (@conid ".")*
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
  "{-"                                          { m (subtract 1) (Comment Open) }
  "-}"                                          { m (+1) (Comment Close) }
  $white+                                       ; -- whitespace
  [^\-\{]+                                      { c $ Comment Text } -- rule to generate comments larger than 1 char
  .                                             { c $ Comment Text }
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
  "--"~[$symbol # \-][^$nl]*                    { c $ Comment Line }
-- Finally because the above rule had to add in a non symbol character 
-- it's also possible that we have just finishing a line,
-- people sometimes do this for example  when breaking up paragraphs
-- in a long comment.
  "--"$nl                                       { c $ Comment Line }

  "{-"                                          { m (subtract 1) $ Comment Open }

  ^"#".*                                        { c $ CppDirective }
  $special                                      { cs $ \(c:_) -> Special c }
  "deriving"                                    { c (Reserved Deriving) }
  @reservedid                                   { c (Reserved Other) }
  "module"                                      { c (Reserved Module) }
  "where"                                       { c (Reserved Where) }
  @layoutReservedId                             { c (Reserved OtherLayout) }
  `@qual @varid`                                { c Operator }
  `@qual @conid`                                { c ConsOperator }
  @qual @varid                                  { c VarIdent }
  @qual @conid                                  { c ConsIdent }

  "|"                                           { c (ReservedOp Pipe) }
  "="                                           { c (ReservedOp Equal) }
  @reservedop                                   { c (ReservedOp OtherOp) }
  @qual @varsym                                 { c Operator }
  @qual @consym                                 { c ConsOperator }

  @decimal
    | 0[oO] @octal
    | 0[xX] @hexadecimal                        { c Number }

  @decimal \. @decimal @exponent?
    | @decimal @exponent                        { c Number }

  \'\' @anyid                                   { c THQuote } -- type
  \' @anyid                                     { c THQuote } -- expression
  \' ($graphic # [\'\\] | " " | @escape) \'     { c CharTok }
  \" @string* \"                                { c StringTok }
  .                                             { c Unrecognized }
}

{

type HlState = Int

data CommentType = Open | Close | Text | Line
    deriving (Eq, Show)

data ReservedType = Where | Let | OtherLayout | Deriving | Module | Other
    deriving (Eq, Show)

data OpType = Pipe | Equal | OtherOp
    deriving (Eq, Show)

data Token = Number | CharTok | StringTok | VarIdent | ConsIdent
           | Reserved !ReservedType | ReservedOp !OpType | Special Char
           | ConsOperator | Operator
           | Comment !CommentType
           | THQuote
           | CppDirective | Unrecognized
             deriving (Eq, Show)

tokenToStyle :: Token -> StyleName
tokenToStyle tok = case tok of
  CppDirective -> preprocessorStyle
  Number       -> numberStyle
  CharTok      -> stringStyle
  StringTok    -> stringStyle
  VarIdent     -> variableStyle
  ConsIdent    -> typeStyle
  ReservedOp _ -> operatorStyle
  Reserved _   -> keywordStyle
  Special _    -> defaultStyle
  ConsOperator -> operatorStyle
  Operator     -> operatorStyle
  Comment _    -> commentStyle
  THQuote      -> quoteStyle
  Unrecognized -> errorStyle

startsLayout (Reserved OtherLayout) = True
startsLayout (Reserved Where) = True
startsLayout (Reserved Let) = True
startsLayout _ = False

isComment (Comment _) = True
isComment _ = False

stateToInit x | x < 0     = nestcomm
              | otherwise = 0

initState :: HlState
initState = 0

type TT = Tok Token

isSpecial :: [Char] -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

isErrorTok :: Token -> Bool
isErrorTok = isSpecial "!"


#include "alex.hsinc"
}
