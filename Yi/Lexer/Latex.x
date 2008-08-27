-- -*- haskell -*- 
--
-- Simple syntax highlighting for Latex source files
--
-- This is not intended to be a lexical analyser for
-- latex, merely good enough to provide some syntax
-- highlighting for latex source files.
--

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Latex ( initState, alexScanToken, Token(..) ) where
import Yi.Lexer.Alex
import Yi.Style
}

$whitechar = [\ \t\n\r\f\v]
$special   = [\[\]\{\}\$\\\%]
$idchar = [^ $special $whitechar]

@reservedid 
        = begin|end|newcommand

haskell :-

 "%"\-*[^\n]*                                { c $ Comment }
 $special                                    { cs $ \(c:_) -> Special c }
 \\begin                                     { c $ Begin }
 \\$special                                  { cs $ \(_:cs) -> Command cs }
 \\end                                       { c $ End }
 \\newcommand                                { c $ NewCommand }
 \\$idchar+                                  { cs $ \(_:cs) -> Command cs }
 $idchar+                                    { c $ Text }
 $white+                                     ; 


{

data Token = Comment | Text | Special !Char | Command !String | Begin | End | NewCommand  
  deriving (Eq, Show, Ord)

type HlState = Int

{- See Haskell.x which uses this to say whether we are in a
   comment (perhaps a nested comment) or not.
-}
stateToInit x = 0

initState :: HlState
initState = 0


#include "alex.hsinc"
}
