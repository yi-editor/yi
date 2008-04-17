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
import Data.Monoid
import Data.Maybe
import Data.List (filter)

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

-- | Return the 1st token of a subtree.
getFirstToken :: Tree t -> Maybe t
getFirstToken tree = getFirst $ foldMap (\x -> First (Just x)) tree

getLastToken :: Tree t -> Maybe t
getLastToken tree = getLast $ foldMap (\x -> Last (Just x)) tree

-- Return all subtrees in a subtree.
getAllSubTrees :: Tree t -> [Tree t]
getAllSubTrees t = t : concatMap getAllSubTrees (subtrees t)
    where subtrees (Group _ g _) = g
          subtrees (Stmt s) = concat s
          subtrees _ = []


type TT = Tok Token

getIndentingSubtree :: [Tree TT] -> Int -> Int -> Maybe (Tree TT)
getIndentingSubtree roots offset line
    = listToMaybe [t' | root <- roots, t'@(Stmt ((t:_):_)) <- getAllSubTrees root, let Just tok = getFirstToken t, let posn = tokPosn tok,
       posnOfs posn > offset, posnLine posn == line]

getSubtreeSpan :: Tree TT -> (Int, Int)
getSubtreeSpan tree = (posnOfs $ first, lastLine - firstLine)
    where bounds@[first, _last] = fmap (tokPosn . assertJust) [getFirstToken tree, getLastToken tree]
          [firstLine, lastLine] = fmap posnLine bounds
          assertJust (Just x) = x

    

instance Foldable Tree where
    foldMap = foldMapDefault

instance Traversable Tree where
    traverse f (Atom t) = Atom <$> f t
    traverse f (Error t) = Error <$> f t
    traverse f (Group l g r) = Group <$> f l <*> traverse (traverse f) g <*> f r
    traverse f (Stmt s) = Stmt <$> traverse (traverse (traverse f)) s

-- dropWhile' f = foldMap (\x -> if f x then mempty else Endo (x :))
-- 
-- isBefore l (Atom t) = isBefore' l t
-- isBefore l (Error t) = isBefore l t
-- isBefore l (Group l g r) = isBefore l r
-- isBefore l (Stmt s) = False
-- 
-- isBefore' l (Tok {tokPosn = Posn {posnLn = l'}}) = 


parse :: P (Tok Token) (Expr (Tok Token))
parse = parse' tokT tokFromT

parse' toTok fromT = pExpr <* eof
    where 
      sym c = symbol (isSpecial [c] . toTok)

      newT c = fromT (Special c)

      pleaseSym c = (recoverWith (pure $ newT '!')) <|> sym c

      pExpr :: P TT (Expr TT)
      pExpr = many pTree

      pStmts = filter (not . null) <$> pExpr `sepBy` sym '.' -- see HACK above
      -- also, we discard the empty statements

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
              | (posnOfs $ tokPosn $ l) == point || (posnOfs $ tokPosn $ r) == point - 1
               = (modStroke hintStyle (ts l) :) . getStrokesL g . (modStroke hintStyle (ts r) :)
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
