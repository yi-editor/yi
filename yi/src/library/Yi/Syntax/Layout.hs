{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Note: If the first line of the file has wrong indentation, some of the
-- code might be left outside of the blocks
module Yi.Syntax.Layout (layoutHandler, State) where

import           Data.List     (find)
import           Data.Maybe    (isJust)
import           Yi.Lexer.Alex (AlexState (..), Posn (Posn), Tok (Tok, tokPosn, tokT), startPosn)
import           Yi.Syntax     (Scanner (..))

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
            (t,t,t) -> (Tok t -> Bool) ->
            Scanner (AlexState lexState) (Tok t) -> Scanner (State t lexState) (Tok t)
layoutHandler isSpecial parens isIgnored (openT, closeT, nextT) isGroupOpen lexSource = Scanner
  {
   scanLooked = scanLooked lexSource . snd,
   scanEmpty = error "layoutHandler: scanEmpty",
   scanInit = (IState [] True (-1), scanInit lexSource),
   scanRun  = \st -> let result = parse (fst st) (scanRun lexSource (snd st))
                     in --trace ("toks = " ++ show (fmap snd result)) $
                        result
  }
    where dummyAlexState = AlexState
              {
               stLexer = error "dummyAlexState: should not be reused for restart!",
               lookedOffset = maxBound, -- setting this to maxBound ensures nobody ever uses it.
               stPosn = startPosn
              }

          deepestIndent [] = -1
          deepestIndent (Indent i:_) = i
          deepestIndent (_:levs) = deepestIndent levs

          deepestParen _ [] = False
          deepestParen p (Paren t:levs) = p == t || deepestParen p levs
          deepestParen p (_:levs) = deepestParen p levs

          findParen f t = find ((== t) . f) parens

          parse :: IState t -> [(AlexState lexState, Tok t)] -> [(State t lexState, Tok t)]
          parse iSt@(IState levels doOpen lastLine)
                toks@((aSt, tok @ Tok {tokPosn = Posn _nextOfs line col}) : tokss)

            -- ignore this token
            | isIgnored tok
              = (st, tok) : parse (IState levels doOpen line) tokss

            -- start a compound if the rest of the line is empty then skip to it!
            | doOpen
              = if isGroupOpen tok -- check so that the do is not followed by a {
                  then parse (IState levels False lastLine) toks
                  else (st', tt openT) : parse (IState (Indent col : levels) False line) toks
                  -- if it's a block opening, we ignore the layout, and just let the "normal" rule
                  -- handle the creation of another level.

            -- close, or prepare to close, a paren block
            | Just (openTok,_) <- findParen snd $ tokT tok,
              deepestParen openTok levels

              = case levels of
                      Indent _:levs -> (st',tt closeT) : parse (IState levs False lastLine) toks
                      -- close an indent level inside the paren block
                      Paren openTok' :levs
                          | openTok == openTok' -> (st', tok) : parse (IState levs False line) tokss
                          | otherwise           ->              parse (IState levs False line) toks
                      -- close one level of nesting.
                      [] -> error $ "Parse: " ++ show iSt

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
            | isJust $ findParen fst $ tokT tok
              = (st', tok) : parse (IState (Paren (tokT tok):levels) (isSpecial (tokT tok)) line) tokss
              -- important note: the the token can be both special and an opening. This is the case of the
              -- haskell 'let' (which is closed by 'in'). In that case the inner block is that of the indentation.

            -- prepare to open a compound
            | isSpecial (tokT tok)
                = (st', tok) : parse (IState levels True   line) tokss

            | otherwise
                = (st', tok) : parse (IState levels doOpen line) tokss
                  where st = (iSt, aSt)
                        st' = (iSt, aSt {lookedOffset = max peeked (lookedOffset aSt)})
                        tt t = Tok t 0 (tokPosn tok)
                        peeked = case tokss of
                                   [] -> maxBound
                                   (AlexState {lookedOffset = p},_):_ -> p
                        -- This function checked the position and kind of the
                        -- next token.  We peeked further, and so must
                        -- update the lookedOffset accordingly.

          -- finish by closing all the indent states.
          parse iSt@(IState (Indent _:levs) doOpen posn) []
              = ((iSt,dummyAlexState), Tok closeT 0 maxPosn) : parse (IState levs doOpen posn) []
          parse (IState (Paren _:levs) doOpen posn) []
              = parse (IState levs doOpen posn) []
          parse (IState [] _ _) [] = []


maxPosn :: Posn
maxPosn = Posn (-1) (-1) 0
-- HACK! here we have collusion between using (-1) here and the parsing of
-- OnlineTrees, which relies on the position of the last token to stop
-- the parsing.

