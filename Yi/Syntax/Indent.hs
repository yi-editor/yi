{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Indent where

import Yi.Syntax
import Yi.Syntax.Alex
import Yi.Prelude
import Prelude ()

data BlockOpen t = Indent Int -- block opened because of indentation; parameter is the column of it.
                 | Paren t      -- block opened because of parentheses
                 deriving Show

data IState t = IState [BlockOpen t]  -- current block nesting
                     Bool   -- should we open a compound now ?
                     Int    -- last line number
  deriving Show
type State t lexState = (IState t, AlexState lexState) 

-- | isSpecial denotes a token that starts a compound, like "where", "do", ...
indenter :: forall t lexState. Eq t => (t -> Bool) -> [(t,t)] ->
            (Tok t -> Bool) -> 
                
            [t] -> 
            Scanner (AlexState lexState) (Tok t) -> Scanner (State t lexState) (Tok t)
indenter isSpecial parens isIgnored [openT, closeT, nextT] lexSource = Scanner 
  {
   scanInit = (IState [] True (-1), scanInit lexSource),
   scanRun  = \st -> parse (fst st) (scanRun lexSource (snd st))
  }
    where tt = tokFromT
          dummyAlexState = AlexState 
              {
               stLexer = error "dummyAlexState: should not be reused for restart!",
               lookedOffset = maxBound, -- setting this to maxBound ensures nobody ever uses it.
               stPosn = startPosn
              }

          deepestIndent [] = (-1)
          deepestIndent (Indent i:_) = i
          deepestIndent (_:levs) = deepestIndent levs
                                   
          deepestParen [] = Nothing

          findParen f t = find ((== t) . f) parens

          closeIndent (Indent _) = tt closeT
          closeIndent (Paren t) = let Just (_,closeParen) = findParen fst t in tt closeParen

          parse :: IState t -> [(AlexState lexState, Tok t)] -> [(State t lexState, Tok t)]
          parse iSt@(IState levels doOpen lastLine)
                toks@((aSt, tok @ Tok {tokLen = nextLen, tokPosn = Posn nextOfs line col}) : tokss) 

            -- ignore this token
            | isIgnored tok
              = (st, tok) : parse (IState levels doOpen line) tokss

            -- start a compound
            | doOpen
              = (st', tt openT) : parse (IState (Indent col:levels) (False) lastLine) toks

            -- pop an indent block
            | col < deepestIndent levels
              = let (lev:levs) = levels in (st', closeIndent lev) : parse (IState levs doOpen lastLine) toks

            -- next item
            | line > lastLine &&
              col == deepestIndent levels -- FIXME: also pop all parens.
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

              


  


      
      
