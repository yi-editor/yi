-- -*- haskell -*-
--
-- Lexical syntax for compilation messages
--

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.Compilation (lexer, Token(..)) where
import Yi.Lexer.Alex hiding (tokenToStyle)
import Yi.Regex
import Yi.Style (commentStyle)
}

$digit  = 0-9
$white = [\ \n]
$filechar = ~[\: $white]

@file = $filechar+
@number    = $digit+

tokens :-
 @file":" @number ":" @number ":" .*\n  { \str st ->
     let Just (_before, arr, _after) = matchOnceText re $ map snd str
         re :: Regex
         re = makeRegex "^(.+):([0-9]+):([0-9]+):(.*)$"
     in (st, Report (fst $ arr!1) (read $ fst $ arr!2) (read $ fst $ arr!3) (fst $ arr!4)) }
 -- without a column number
 @file":" @number ":" .*\n  { \str st ->
     let Just (_before, arr, _after) = matchOnceText re $ map snd str
         re :: Regex
         re = makeRegex "^(.+):([0-9]+):(.*)$"
     in (st, Report (fst $ arr!1) (read $ fst $ arr!2) 0 (fst $ arr!3)) }

 $white+                              ; -- unparseable stuff
 [^$white]+                           ;
{

type HlState = ()
data Token
  = Report String Int Int String
  | Text String
    deriving Show

stateToInit () = 0
initState = ()

lexer :: StyleLexerASI HlState Token
lexer = StyleLexer
  { _tokenToStyle = const commentStyle
  , _styleLexer = commonLexer alexScanToken initState
  }

#include "common.hsinc"
}
