-- Parser test

{

{-# OPTIONS -w #-}

module Yi.Syntax.TestParse where

import Yi.Syntax.Syntax
import Yi.Style
import Yi.Syntax.TestLex

}

%name parseWords
%tokentype { Token }
%token
    WORD { T $$ }
%%

-- grammar

p :: { WordTree }
  :  words          { Words (reverse $1) }
  |  {-epsilon-}    { Words [] }

words :: { [AbsWord] }
      : words WORD  { mkWord $2 : $1 }
      | WORD        { [mkWord $1] }

{

mkWord :: AlexPosn -> AbsWord
mkWord (AlexPn i j k) = W (i,(i + (k - j)))

happyError :: [Token] -> a
happyError x = case x of
    []      -> error "Parser" "parse error"
    (T p):_ -> error "Parser" $ showPos p

--
-- Really simple abstract syntax tree -- just a [Word], where word nodes
-- hold only their buffer indicies.
--

newtype AbsWord = W (Int,Int)
newtype WordTree = Words [AbsWord]

instance Syntax WordTree where
        synname _    = "Test"
        extensions _ = [".txt"]
        parse buf     = undefined -- TODO lexer . parseWords
        foldSyn f _ _ = undefined
--
-- define the 
--
instance Drawable AbsWord where 
    posOf (W i)   = i
    styleOf _     = Style darkBlue Default

}
