{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Layout where

import Yi.Syntax
import Yi.Syntax.Alex
import Yi.Prelude
import Prelude ()
import Data.Maybe (isJust)
import Data.List (dropWhile)

data BlockOpen t = Indent Int -- block opened because of indentation; parameter is the column of it.
                 | Paren t      -- block opened because of parentheses
                 deriving Show

isParen :: BlockOpen t -> Bool
isParen (Paren _) = True
isParen _ = False

data IState t = IState [BlockOpen t]  -- current block nesting
                     Bool   -- should we open a compound now ?
                     Int    -- last line number
  deriving Show
type State t lexState = (IState t, AlexState lexState) 


-- | Transform a scanner into a scanner that also adds opening,
-- closing and "next" tokens to indicate layout.  

-- @isSpecial@ predicate indicates a token that starts a compound,
-- like "where", "do", ...

-- @isIgnore@ predicate indicates a token that is to be ignored for
-- layout. (eg. pre-processor directive...)

-- @parens@ is a list of couple of matching parenthesis-like tokens
-- "()[]{}...".


layoutHandler :: forall t lexState. (Show t, Eq t) => (t -> Bool) -> [(t,t)] ->
            (Tok t -> Bool) ->                 
            [t] -> 
            Scanner (AlexState lexState) (Tok t) -> Scanner (State t lexState) (Tok t)
layoutHandler isSpecial parens isIgnored [openT, closeT, nextT] lexSource = Scanner 
  {
   scanInit = (IState [] True (-1), scanInit lexSource),
   scanRun  = \st -> let result = parse (fst st) (scanRun lexSource (snd st)) 
                     in --trace ("toks = " ++ show (fmap snd result)) $ 
                        result
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
                                   
          deepestParen [] = nextT -- HACK: nextT must not be found in parens.
          deepestParen (Paren t:_) = t
          deepestParen (_:levs) = deepestParen levs

          findParen f t = find ((== t) . f) parens

          parse :: IState t -> [(AlexState lexState, Tok t)] -> [(State t lexState, Tok t)]
          parse iSt@(IState levels doOpen lastLine)
                toks@((aSt, tok @ Tok {tokPosn = Posn _nextOfs line col}) : tokss) 

            -- ignore this token
            | isIgnored tok
              = (st, tok) : parse (IState levels doOpen line) tokss

            -- start a compound
            | doOpen
              = (st', tt openT) : parse (IState (Indent col:levels) (False) lastLine) toks

            -- pop an indent block
            | col < deepestIndent levels
              = let (_lev:levs) = dropWhile isParen levels
                in (st', tt closeT) : parse (IState levs doOpen lastLine) toks
                  -- drop all paren levels inside the indent
           
            -- next item
            | line > lastLine &&
              col == deepestIndent levels 
                = (st', tt nextT) : parse (IState (dropWhile isParen levels) doOpen line) toks
                  -- drop all paren levels inside the indent

            -- open a paren block
            | isJust $ findParen fst $ (tokT tok)
              = (st', tok) : parse (IState (Paren (tokT tok):levels) False  line) tokss

            -- prepare to close a paren block
            | isJust $ findParen id $ (deepestParen levels, tokT tok) -- check that the most nested paren matches.
              = case levels of
                      Indent _:levs -> (st',tt closeT) : parse (IState levs False lastLine) toks 
                      -- close an indent level inside the paren block
                      Paren _ :levs -> (st', tok)      : parse (IState levs False line)     tokss
                      -- same as the otherwise case below.

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

          -- finish by closing all the indent states.
          parse iSt@(IState (Indent _:levs) doOpen posn) [] 
              = ((iSt,dummyAlexState), tt closeT) : parse (IState levs doOpen posn) []
          parse (IState (Paren _:levs) doOpen posn) [] 
              = parse (IState levs doOpen posn) []
          parse (IState [] _ _) [] = []
          parse st _ = error $ "Parse: " ++ show st
              


  


      
      
