module Yi.Syntax.Paren where

import Yi.IncrementalParse
import Yi.Syntax.Alex
import Yi.Syntax.Haskell
import Control.Applicative
import Yi.Style (errorStyle)

isSpecial :: [Char] -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

sym :: Char -> P (Tok Token) (Tok Token)
sym c = symbol (isSpecial [c] . tokT)

errT :: Char -> Tok Token
errT c = Tok (Special c) 0 startPosn

pleaseSym :: Char -> P (Tok Token) (Tok Token)
pleaseSym c = (recoverWith (pure $ errT '!')) <|> sym c

isNoise :: Token -> Bool
isNoise (Special c) = c `elem` ";,`"
isNoise _ = True

data Tree t
    = Group t [Tree t] t 
    | Atom t
    | Error t
      deriving Show

instance Functor Tree where
    fmap f (Atom t) = Atom (f t)
    fmap f (Error t) = Error (f t)
    fmap f (Group l g r) = Group (f l) (fmap (fmap f) g) (f r)

pExpr :: P (Tok Token) [Tree (Tok Token)]
pExpr = many pTree

pTree :: P (Tok Token) (Tree (Tok Token))
pTree = (Group  <$> sym '(' <*> pExpr <*> pleaseSym ')')
    <|> (Group  <$> sym '[' <*> pExpr <*> pleaseSym ']')
    <|> (Group  <$> sym '{' <*> pExpr <*> pleaseSym '}')
    <|> (Atom <$> symbol (isNoise . tokT))
    <|> (Error <$> recoverWith (symbol (isSpecial "})]" . tokT)))

parse :: P (Tok Token) [Tree (Tok Token)]
parse = pExpr <* eof

getStrokes begin end t = getStrokesL t []
    where getStrokes' (Atom t) = (ts t :)
          getStrokes' (Error t) = (modStroke errorStyle (ts t) :) -- paint in red
          getStrokes' (Group l g r)
              | isSpecial "!" $ tokT r = (modStroke errorStyle (ts l) :) . getStrokesL g . (ts r :)
                                    -- left paren wasn't matched.
              | otherwise  = (ts l :) . getStrokesL g . (ts r :)
          getStrokesL g = foldr (.) id (fmap getStrokes' g)
          ts = tokenToStroke

modStroke f (l,s,r) = (l,f s,r) 

tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn + len)
