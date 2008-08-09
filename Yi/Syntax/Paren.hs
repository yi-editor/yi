-- Copyright (c) JP Bernardy 2008

module Yi.Syntax.Paren where

import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.Haskell
import Yi.Style (hintStyle, errorStyle, StyleName)
import Yi.Syntax.Layout
import Yi.Syntax.Tree
import Yi.Syntax
import Yi.Prelude 
import Prelude ()
import Data.Monoid
import Data.Maybe
import Data.List (filter, takeWhile)

indentScanner :: Scanner (AlexState lexState) (TT)
              -> Scanner (Yi.Syntax.Layout.State Token lexState) (TT)
indentScanner = layoutHandler startsLayout [(Special '(', Special ')'),
                                            (Special '[', Special ']'),
                                            (Special '{', Special '}')] ignoredToken
                         (fmap Special ['<', '>', '.'])

-- HACK: We insert the Special '<', '>', '.', that don't occur in normal haskell
-- parsing.

ignoredToken :: TT -> Bool
ignoredToken (Tok t _ (Posn _ _ col)) = col == 0 && isComment t || t == CppDirective
    
isNoise :: Token -> Bool
isNoise (Special c) = c `elem` ";,`"
isNoise _ = True

type Expr t = [Tree t]

data Tree t
    = Paren t (Expr t) t -- A parenthesized expression (maybe with [ ] ...)
    | Block [Expr t]      -- A list of things (as in do; etc.)
    | Atom t
    | Error t
      deriving Show

instance Functor Tree where
  fmap = fmapDefault

instance IsTree Tree where
    subtrees (Paren _ g _) = g
    subtrees (Block s) = concat s
    subtrees _ = []

-- | Search the given list, and return the 1st tree after the given
-- point on the given line.  This is the tree that will be moved if
-- something is inserted at the point.  Precondition: point is in the
-- given line.  

-- TODO: this should be optimized by just giving the point of the end
-- of the line
getIndentingSubtree :: [Tree TT] -> Point -> Int -> Maybe (Tree TT)
getIndentingSubtree roots offset line =
    listToMaybe $ [t | (t,posn) <- takeWhile ((<= line) . posnLine . snd) $ allSubTreesPosn,
                   -- it's very important that we do a linear search
                   -- here (takeWhile), so that the tree is evaluated
                   -- lazily and therefore parsing it can be lazy.
                   posnOfs posn > offset, posnLine posn == line]
    where allSubTreesPosn = [(t',posn) | root <- roots, t'@(Block ((t:_):_)) <- getAllSubTrees root, 
                               let Just tok = getFirstElement t, let posn = tokPosn tok]


-- | given a tree, return (first offset, number of lines).
getSubtreeSpan :: Tree TT -> (Point, Int)
getSubtreeSpan tree = (posnOfs $ first, lastLine - firstLine)
    where bounds@[first, _last] = fmap (tokPosn . assertJust) [getFirstElement tree, getLastElement tree]
          [firstLine, lastLine] = fmap posnLine bounds
          assertJust (Just x) = x
          assertJust _ = error "assertJust: Just expected"
    

instance Foldable Tree where
    foldMap = foldMapDefault

instance Traversable Tree where
    traverse f (Atom t) = Atom <$> f t
    traverse f (Error t) = Error <$> f t
    traverse f (Paren l g r) = Paren <$> f l <*> traverse (traverse f) g <*> f r
    traverse f (Block s) = Block <$> traverse (traverse (traverse f)) s

-- dropWhile' f = foldMap (\x -> if f x then mempty else Endo (x :))
-- 
-- isBefore l (Atom t) = isBefore' l t
-- isBefore l (Error t) = isBefore l t
-- isBefore l (Paren l g r) = isBefore l r
-- isBefore l (Block s) = False
-- 
-- isBefore' l (Tok {tokPosn = Posn {posnLn = l'}}) = 


parse :: P TT [Tree TT]
parse = parse' tokT tokFromT

parse' :: (TT -> Token) -> (Token -> TT) -> P TT [Tree TT]
parse' toTok fromT = pExpr <* eof
    where 
      -- | parse a special symbol
      sym c = symbol (isSpecial [c] . toTok)

      -- | Create a special character symbol
      newT c = fromT (Special c)

      pleaseSym c = (recoverWith (pure $ newT '!')) <|> sym c

      pExpr :: P TT (Expr TT)
      pExpr = many pTree

      pBlocks = filter (not . null) <$> pExpr `sepBy` sym '.' -- see HACK above
      -- also, we discard the empty statements

      pTree :: P TT (Tree TT)
      pTree = (Paren  <$>  sym '(' <*> pExpr  <*> pleaseSym ')')
          <|> (Paren  <$>  sym '[' <*> pExpr  <*> pleaseSym ']')
          <|> (Paren  <$>  sym '{' <*> pExpr  <*> pleaseSym '}')

          <|> (Block <$> (sym '<' *> pBlocks <* sym '>')) -- see HACK above

          <|> (Atom <$> symbol (isNoise . toTok))
          <|> (Error <$> recoverWith (symbol (isSpecial "})]" . toTok)))

      -- note that, by construction, '<' and '>' will always be matched, so
      -- we don't try to recover errors with them.

-- TODO: (optimization) make sure we take in account the begin, so we don't return useless strokes
getStrokes :: Point -> Point -> Point -> [Tree TT] -> [Stroke]
getStrokes point _begin _end t0 = result 
    where getStrokes' (Atom t) = (ts t :)
          getStrokes' (Error t) = (modStroke errorStyle (ts t) :) -- paint in red
          getStrokes' (Block s) = list (fmap getStrokesL s)
          getStrokes' (Paren l g r)
              | isErrorTok $ tokT r = (modStroke errorStyle (ts l) :) . getStrokesL g
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) == point || (posnOfs $ tokPosn $ r) == point - 1

               = (modStroke hintStyle (ts l) :) . getStrokesL g . (modStroke hintStyle (ts r) :)
              | otherwise  = (ts l :) . getStrokesL g . (ts r :)
          getStrokesL g = list (fmap getStrokes' g)
          ts = tokenToStroke
          list = foldr (.) id
          result = getStrokesL t0 []

modStroke :: StyleName -> Stroke -> Stroke
modStroke f (l,s,r) = (l,f `mappend` s,r) 

tokenToStroke :: TT -> Stroke
tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn +~ len)


----------------------
-- Should be in lib

sepBy :: (Alternative f) => f a -> f v -> f [a]
sepBy p s   = sepBy1 p s <|> pure []

sepBy1     :: (Alternative f) => f a -> f v -> f [a]
sepBy1 p s  = (:) <$> p <*> many (s *> p)
