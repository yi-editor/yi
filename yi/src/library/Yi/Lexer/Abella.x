-- -*- haskell -*-
--
-- Lexical syntax for Abella
--
-- (c) Nicolas Pouillard 2009
--

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.Abella ( lexer, tokenToText, TT, isComment, Token(..), HlState
                       , Reserved(..), ReservedOp(..) ) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Style
}

$whitechar = [\ \t\n\r\f\v]

$digit  = 0-9

$small     = [a-z]
$large     = [A-Z]
$alpha     = [$small $large]
$idchar    = [$alpha $digit \_ \']

$nl        = [\n\r]

@reservedid =
        Theorem|Define
@reservedop = [\=\:\,\(\)\{\}]

@varid  = $large $idchar*
@conid  = [\' \_ $small] $idchar*
@anyid = (@varid | @conid)

@decimal     = $digit+

abella :-

<0> $white+                                     ;

<0> {
  "%"[^$nl]*                                    { c CommentLine }

  "exists"                                      { c $ Reserved Exists }
  "forall"                                      { c $ Reserved Forall }

  @reservedid                                   { c $ Reserved Other }

  \\                                            { c $ ReservedOp BackSlash }
  \\\/                                          { c $ ReservedOp Or }
  \/\\                                          { c $ ReservedOp And }
  "->"                                          { c $ ReservedOp RightArrow }
  "=>"                                          { c $ ReservedOp DoubleRightArrow }
  "."                                           { c $ ReservedOp Dot }
  @reservedop                                   { c $ ReservedOp OtherOp }

  @decimal                                      { c Number }

  "skip"                                        { c Skip }

  @varid                                        { c VarIdent }
  @conid                                        { c ConsIdent }

  .                                             { c Unrecognized }
}

{

type HlState = Int

data Reserved = Forall | Exists | Other
    deriving (Eq, Show)

data ReservedOp = Or | And | BackSlash | RightArrow | DoubleRightArrow | Dot | OtherOp
    deriving (Eq, Show)

data Token = Number | VarIdent | ConsIdent
           | Reserved !Reserved | ReservedOp !ReservedOp
           | CommentLine | Skip
           | Unrecognized
             deriving (Eq, Show)

tokenToStyle :: Token -> StyleName
tokenToStyle tok = case tok of
  Number         -> numberStyle
  VarIdent       -> variableStyle
  ConsIdent      -> typeStyle
  ReservedOp _   -> operatorStyle
  Reserved _     -> keywordStyle
  CommentLine    -> commentStyle
  Skip           -> errorStyle
  Unrecognized   -> errorStyle

tokenToText :: Token -> Maybe String
tokenToText (ReservedOp RightArrow) = Just "→ "
tokenToText (ReservedOp DoubleRightArrow) = Just "⇒ "
tokenToText (ReservedOp And) = Just "∧ "
tokenToText (ReservedOp Or) = Just "∨ "
tokenToText (Reserved Exists) = Just "∃"
tokenToText (Reserved Forall) = Just "∀"
tokenToText _ = Nothing

isComment CommentLine = True
isComment _ = False

stateToInit = const 0

initState :: HlState
initState = 0

type TT = Tok Token

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = tokenToStyle
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
