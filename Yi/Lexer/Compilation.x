-- -*- haskell -*- 
--
-- Lexical syntax for illiterate Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--

{
module Yi.Lexer.Compilation (initState, alexScanToken, Token(..)) where
import Yi.Lexer.Alex
import Yi.Regex
}

$digit  = 0-9
$any    = [.\n]
@number    = $digit+

tokens :-
 .+":" @number ":" @number ":" .*\n  { \str st -> let Just (_before, arr, _after) = matchOnceText re $ map snd str
                                             in (st, Report (fst $ arr!1) (read $ fst $ arr!2) (read $ fst $ arr!3) (fst $ arr!4)) }
 .*\n                               ; -- line of text
 .+                                 ; -- last line

{
re :: Regex
re = makeRegex "^(.+):([0-9]+):([0-9]+):(.*)$"

type HlState = ()
data Token 
  = Report String Int Int String
  | Text String
    deriving Show

stateToInit () = 0
initState = ()

#include "alex.hsinc"
}
