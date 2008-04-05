{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Indent where

import Yi.Syntax
import Yi.Syntax.Alex
import Yi.Prelude
import Prelude ()

data IState = IState [Int] Bool Int
  deriving Show
type State lexState = (IState, AlexState lexState) 

-- data Tok hlState token = Tok (AlexState hlState, token)
-- type Tok hlState token = (AlexState hlState, token)

indenter :: forall t lexState. (t -> Bool) -> [t] -> 
            Scanner (AlexState lexState) (Tok t) -> Scanner (State lexState) (Tok t)
indenter isSpecial [openT, closeT, nextT] lexSource = Scanner 
  {
   scanInit = (IState [(-1)] False (-1), scanInit lexSource),
   scanRun  = \st -> parse st (scanRun lexSource (snd st))
  }
    where tt tok = Tok tok 0 startPosn
          parse :: State lexState -> [(AlexState lexState, Tok t)] -> [(State lexState, Tok t)]
          parse st@(ist@(IState levels@(lev:levs) doOpen lastLine), alexSt)
                toks@((newAlexSt, tok @ Tok {tokPosn = Posn _ line col}) : tokss) 

            -- start a compound
            | doOpen
              = (st, tt openT) : parse (IState (col:levels) (False) lastLine, newAlexSt) toks

            -- pop one level
            | col < lev
              = (st, tt closeT) : parse (IState levs doOpen lastLine, alexSt) toks

            -- next item
            | line > lastLine &&
              col == lev 
                = (st, tt nextT) : parse (IState levels doOpen line, alexSt) toks

            -- prepare to open a compound
            | isSpecial (tokT tok) 
                = ((ist, newAlexSt), tok) : parse (IState levels True   line, newAlexSt) tokss

            | otherwise     
                = ((ist, newAlexSt), tok) : parse (IState levels doOpen line, newAlexSt) tokss
          parse st@(IState levels@(_:lev:levs) doOpen posn, alexSt) [] 
              = (st, tt closeT) : parse (IState (lev:levs) doOpen posn, alexSt) []
          parse (IState [_] _ _, _) [] = []
              
              


  


      
      
