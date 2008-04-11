-- Copyright (c) JP Bernardy 2008

module Yi.Syntax.Paren where

import Yi.IncrementalParse
import Yi.Syntax.Alex
import Yi.Syntax.Haskell
import Control.Applicative
import Yi.Style (hintStyle, errorStyle, Style)
import Yi.Syntax.Indent
import Yi.Syntax
import Yi.Prelude 
import Prelude ()

indentScanner :: Scanner (AlexState lexState) (Tok Token)
              -> Scanner (Yi.Syntax.Indent.State lexState) (Tok Token)
indentScanner = indenter (== IndentReserved)
                         (fmap Special ['<', '>', '.']) 
-- HACK: We insert the Special '<', '>', '.', that don't occur in normal haskell parsing.


isSpecial :: [Char] -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

isNoise :: Token -> Bool
isNoise (Special c) = c `elem` ";,`"
isNoise _ = True

type Expr t = [Tree t]

data Tree t
    = Group t (Expr t) t 
    | Stmt [Expr t]
    | Atom t
    | Error t
      deriving Show

instance Functor Tree where
    fmap f (Atom t) = Atom (f t)
    fmap f (Error t) = Error (f t)
    fmap f (Group l g r) = Group (f l) (fmap (fmap f) g) (f r)
    fmap f (Stmt s) = Stmt ((fmap (fmap (fmap f))) s)
                          
    
parse :: P (Tok Token) (Expr (Tok Token))
parse = parse' tokT tokFromT

parse' toTok fromT = pExpr <* eof
    where 
      sym c = symbol (isSpecial [c] . toTok)

      newT c = fromT (Special c)

      pleaseSym c = (recoverWith (pure $ newT '!')) <|> sym c

      pExpr = many pTree

      pStmts = pExpr `sepBy` sym '.' -- see HACK above

      pTree :: P (Tok Token) (Tree (Tok Token))
      pTree = (Group  <$>  sym '(' <*> pExpr  <*> pleaseSym ')')
          <|> (Group  <$>  sym '[' <*> pExpr  <*> pleaseSym ']')
          <|> (Group  <$>  sym '{' <*> pExpr  <*> pleaseSym '}')

          <|> (Stmt <$> (sym '<' *> pStmts <* sym '>')) -- see HACK above

          <|> (Atom <$> symbol (isNoise . toTok))
          <|> (Error <$> recoverWith (symbol (isSpecial "})]" . toTok)))

      -- note that, by construction, '<' and '>' will always be matched, so
      -- we don't try to recover errors with them.

-- TODO: (optimization) make sure we take in account the begin, so we don't return useless strokes
getStrokes :: Int -> Int -> Int -> Expr (Tok Token) -> [(Int, Style, Int)]
getStrokes point begin end t0 = result 
    where getStrokes' (Atom t) = (ts t :)
          getStrokes' (Error t) = (modStroke errorStyle (ts t) :) -- paint in red
          getStrokes' (Stmt s) = list (fmap getStrokesL s)
          getStrokes' (Group l g r)
              | isSpecial "!" $ tokT r = (modStroke errorStyle (ts l) :) . getStrokesL g
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Group" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) == point = (ts l :) . getStrokesL g . (modStroke hintStyle (ts r) :)
              | (posnOfs $ tokPosn $ r) == point = (modStroke hintStyle (ts l) :) . getStrokesL g . (ts r :)
              | otherwise  = (ts l :) . getStrokesL g . (ts r :)
          getStrokesL g = list (fmap getStrokes' g)
          ts = tokenToStroke
          list = foldr (.) id
          result = getStrokesL t0 []

modStroke :: (t1 -> t3) -> (Int, t1, Int) -> (Int, t3, Int)
modStroke f (l,s,r) = (l,f s,r) 

tokenToStroke :: Tok Token -> (Int, Style, Int)
tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn + len)


----------------------
-- Should be in lib

sepBy :: (Alternative f) => f a -> f v -> f [a]
sepBy p s   = sepBy1 p s <|> pure []

sepBy1     :: (Alternative f) => f a -> f v -> f [a]
sepBy1 p s  = (:) <$> p <*> many (s *> p)
