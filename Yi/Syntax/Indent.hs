{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Indent where

import Yi.Syntax
import Yi.Syntax.Alex
import Yi.Prelude
import Prelude ()

data IState = IState [Int]  -- nested levels, as columns
                     Bool   -- should we open a compound now ?
                     Int    -- last line number
  deriving Show
type State lexState = (IState, AlexState lexState) 

-- | isSpecial denotes a token that starts a compound, like "where", "do", ...
indenter :: forall t lexState. (t -> Bool) -> 
            (Tok t -> Bool) -> 
            [t] -> 
            Scanner (AlexState lexState) (Tok t) -> Scanner (State lexState) (Tok t)
indenter isSpecial isIgnored [openT, closeT, nextT] lexSource = Scanner 
  {
   scanInit = (IState [(-1)] True (-1), scanInit lexSource),
   scanRun  = \st -> parse (fst st) (scanRun lexSource (snd st))
  }
    where tt = tokFromT
          dummyAlexState = AlexState 
              {
               stLexer = error "dummyAlexState: should not be reused for restart!",
               lookedOffset = maxBound, -- setting this to maxBound ensures nobody ever uses it.
               stPosn = startPosn
              }

          parse :: IState -> [(AlexState lexState, Tok t)] -> [(State lexState, Tok t)]
          parse iSt@(IState levels@(lev:levs) doOpen lastLine)
                toks@((aSt, tok @ Tok {tokLen = nextLen, tokPosn = Posn nextOfs line col}) : tokss) 

            -- ignore this token
            | isIgnored tok
              = (st, tok) : parse (IState levels doOpen line) tokss

            -- start a compound
            | doOpen
              = (st', tt openT) : parse (IState (col:levels) (False) lastLine) toks

            -- pop one level
            | col < lev
              = (st', tt closeT) : parse (IState levs doOpen lastLine) toks

            -- next item
            | line > lastLine &&
              col == lev 
                = (st', tt nextT) : parse (IState levels doOpen line) toks

            -- prepare to open a compound
            | isSpecial (tokT tok) 
                = (st', tok) : parse (IState levels True   line) tokss

            | otherwise     
                = (st', tok) : parse (IState levels doOpen line) tokss
                  where st = (iSt, aSt)
                        st' = (iSt, aSt {lookedOffset = max peeked (lookedOffset aSt)})
                        peeked = case tokss of 
                                   [] -> maxBound
                                   (AlexState {lookedOffset = p},_):_ -> p
                        -- This function checked the position and kind of the
                        -- next token.  We peeked further, and so must
                        -- update the lookedOffset accordingly.
          parse iSt@(IState (_:lev:levs) doOpen posn) [] 
              = ((iSt,dummyAlexState), tt closeT) : parse (IState (lev:levs) doOpen posn) []
          parse (IState [_] _ _) [] = []

              


  


      
      
