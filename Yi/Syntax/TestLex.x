--
-- a simple word tokenizer
-- 
{

{-# OPTIONS -fno-warn-name-shadowing     #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-unused-binds       #-}
{-# OPTIONS -fno-warn-unused-matches     #-}

module Yi.Syntax.TestLex (
        lexer,
        showPos,
        AlexPosn(..),
        Token(..)
    ) where

}

%wrapper "posn"

$whitechar = [ \t\n\r\f\v]

test :-
$white+             ;
.+                  { \p _ -> T p }

{
newtype Token = T AlexPosn  -- a token just records the offset in the buffer

lexer :: String -> [Token]  -- just strings for now
lexer str = go (alexStartPos,'\n',str)
    where
        go inp@(pos,_,str) = case alexScan inp 0 of
            AlexEOF -> []
            AlexError (p,_,s) ->
                error "Lexer" $ showPos p ++ " `" ++ [head s] ++ "'"
            AlexSkip  inp' len     -> go inp'
            AlexToken inp' len act -> act pos (take len str) : go inp'

showPos :: AlexPosn -> String
showPos (AlexPn off _ _) = "offset " ++ show off

}


