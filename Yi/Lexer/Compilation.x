-- -*- haskell -*- 
--
-- Lexical syntax for compilation messages
--

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Compilation (initState, alexScanToken, Token(..)) where
import Yi.Lexer.Alex
import Yi.Regex
}

$digit  = 0-9
$any    = [.\n]
@number    = $digit+

tokens :-
 .+":" @number ":" @number ":" .*\n  { \str st -> 
     let Just (_before, arr, _after) = matchOnceText re $ map snd str
         re :: Regex
         re = makeRegex "^(.+):([0-9]+):([0-9]+):(.*)$"
     in (st, Report (fst $ arr!1) (read $ fst $ arr!2) (read $ fst $ arr!3) (fst $ arr!4)) }
 -- without a column number
 .+":" @number ":" .*\n  { \str st -> 
     let Just (_before, arr, _after) = matchOnceText re $ map snd str
         re :: Regex
         re = makeRegex "^(.+):([0-9]+):(.*)$"
     in (st, Report (fst $ arr!1) (read $ fst $ arr!2) 0 (fst $ arr!3)) }
 .*\n                               ; -- line of text
 .+                                 ; -- last line

{

type HlState = ()
data Token 
  = Report String Int Int String
  | Text String
    deriving Show

stateToInit () = 0
initState = ()

#include "alex.hsinc"
}
