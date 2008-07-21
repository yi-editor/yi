-- Copyright (c) JP Bernardy 2008

module Yi.Syntax.Paren where

import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.Haskell
import Control.Applicative
import Yi.Style (hintStyle, errorStyle, Style)
import Yi.Syntax.Layout
import Yi.Syntax
import Yi.Prelude 
import Prelude ()
import Data.Monoid
import Data.Maybe
import Data.List (filter, takeWhile)

indentScanner :: Scanner (AlexState lexState) (Tok Token)
              -> Scanner (Yi.Syntax.Layout.State Token lexState) (Tok Token)
indentScanner = layoutHandler startsLayout [(Special '(', Special ')'),
                                            (Special '[', Special ']'),
                                            (Special '{', Special '}')] ignoredToken
                         (fmap Special ['<', '>', '.'])
-- HACK: We insert the Special '<', '>', '.', that don't occur in normal haskell parsing.

ignoredToken :: Tok Token -> Bool
ignoredToken (Tok t _ (Posn _ _ col)) = col == 0 && isComment t
    

isSpecial :: [Char] -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

isErrorTok :: Token -> Bool
isErrorTok = isSpecial "!"

isNoise :: Token -> Bool
isNoise (Special c) = c `elem` ";,`"
isNoise _ = True

type Expr t = [Tree t]

data Tree t
    = Group t (Expr t) t -- A parenthesized expression (maybe with [ ] ...)
    | Stmt [Expr t]      -- A list of things (as in do; etc.)
    | Atom t
    | Error t
      deriving Show

instance Functor Tree where
  fmap = fmapDefault

-- | Return the 1st token of a subtree.
getFirstToken :: Tree t -> Maybe t
getFirstToken tree = getFirst $ foldMap (\x -> First (Just x)) tree

-- | Return the last token of a subtree.
getLastToken :: Tree t -> Maybe t
getLastToken tree = getLast $ foldMap (\x -> Last (Just x)) tree

getLastOffset :: Tree TT -> Point
getLastOffset = maybe 0 tokenLastOffset . getLastToken

tokenLastOffset :: TT -> Point
tokenLastOffset tok = posnOfs (tokPosn tok) +~ tokLen tok

-- | Return all subtrees in a tree, in preorder.
getAllSubTrees :: Tree t -> [Tree t]
getAllSubTrees t = t : concatMap getAllSubTrees (subtrees t)

-- | Direct subtrees of a tree
subtrees :: Tree t -> [Tree t]
subtrees (Group _ g _) = g
subtrees (Stmt s) = concat s
subtrees _ = []

-- | Return all subtrees in a tree; each element of the return list
-- contains paths to nodes. (Root is at the start of each path)
getAllPaths :: Tree t -> [[Tree t]]
getAllPaths t = fmap (++[t]) ([] : concatMap getAllPaths (subtrees t))


type TT = Tok Token

-- | Search the given list, and return the last tree before the given
-- point; with path to the root. (Root is at the start of the path)
getLastPath :: [Tree (Tok t)] -> Point -> Maybe [Tree (Tok t)]
getLastPath roots offset =
    case takeWhile ((< offset) . posnOfs . snd) allSubPathPosn of
      [] -> Nothing
      list -> Just $ fst $ last list
    where allSubPathPosn = [(p,posn) | root <- roots, p@(t':_) <- getAllPaths root, 
                            let Just tok = getFirstToken t', let posn = tokPosn tok]


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
    where allSubTreesPosn = [(t',posn) | root <- roots, t'@(Stmt ((t:_):_)) <- getAllSubTrees root, 
                               let Just tok = getFirstToken t, let posn = tokPosn tok]


-- | given a tree, return (first offset, number of lines).
getSubtreeSpan :: Tree TT -> (Point, Int)
getSubtreeSpan tree = (posnOfs $ first, lastLine - firstLine)
    where bounds@[first, _last] = fmap (tokPosn . assertJust) [getFirstToken tree, getLastToken tree]
          [firstLine, lastLine] = fmap posnLine bounds
          assertJust (Just x) = x
          assertJust _ = error "assertJust: Just expected"
    

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

parse' :: (Tok Token -> Token) -> (Token -> Tok Token) -> P TT (Expr TT)
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
getStrokes :: Point -> Point -> Point -> Expr (Tok Token) -> [Stroke]
getStrokes point _begin _end t0 = result 
    where getStrokes' (Atom t) = (ts t :)
          getStrokes' (Error t) = (modStroke errorStyle (ts t) :) -- paint in red
          getStrokes' (Stmt s) = list (fmap getStrokesL s)
          getStrokes' (Group l g r)
              | isErrorTok $ tokT r = (modStroke errorStyle (ts l) :) . getStrokesL g
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

modStroke :: Style -> Stroke -> Stroke
modStroke f (l,s,r) = (l,f ++ s,r) 

tokenToStroke :: Tok Token -> Stroke
tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn +~ len)


----------------------
-- Should be in lib

sepBy :: (Alternative f) => f a -> f v -> f [a]
sepBy p s   = sepBy1 p s <|> pure []

sepBy1     :: (Alternative f) => f a -> f v -> f [a]
sepBy1 p s  = (:) <$> p <*> many (s *> p)
