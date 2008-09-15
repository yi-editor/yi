-- Copyright (c) JP Bernardy 2008
module Yi.Syntax.Latex where

import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.Latex
import Yi.Style hiding (eof)
import Yi.Syntax.Tree
import Yi.Syntax
import Yi.Prelude 
import Prelude ()
import Data.Monoid
import Data.List (zip)

isNoise :: Token -> Bool
isNoise Text = True
isNoise Comment = True
isNoise (Command _) = True
isNoise NewCommand = True
isNoise (Special ',') = True
isNoise (Special _) = False
isNoise (Begin _) = False
isNoise (End _) = False

type Expr t = [Tree t]

type TT = Tok Token

data Tree t
    = Paren t (Expr t) t -- A parenthesized expression (maybe with [ ] ...)
    | Atom t
    | Error t
      deriving Show

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
    foldMap = foldMapDefault

instance Traversable Tree where
    traverse f (Atom t) = Atom <$> f t
    traverse f (Error t) = Error <$> f t
    traverse f (Paren l g r) = Paren <$> f l <*> traverse (traverse f) g <*> f r
--    traverse f (Block b n g e m) = Block <$> f b <*> traverse f n <*> traverse (traverse f) g <*> f e <*> traverse f m

instance IsTree Tree where
    subtrees (Paren _ g _) = g
    subtrees _ = []

parse :: P TT [Tree TT]
parse = pExpr True <* eof
    where 
      -- | Create a special character symbol
      newT c = tokFromT (Special c)
      errT = newT '!'

      -- | parse a special symbol
      sym' p = symbol (p . tokT)
      sym t = sym' (== t)

      pleaseSym c = recoverWith (pure $ errT) <|> sym c
      pleaseSym' c = recoverWith (pure $ errT) <|> sym' c

      -- pExpr :: P TT [Expr TT]
      pExpr = many . pTree

      parens = [(Special x, Special y) | (x,y) <- zip "({[" ")}]"]
      openParens = fmap fst parens

      pBlock = Paren <$> sym' isBegin <*> pExpr True <*> pleaseSym' isEnd

      pTree :: Bool -> P TT (Tree TT)
      pTree outsideMath = 
          (if outsideMath then pBlock <|> (Paren <$> sym (Special '$') <*> pExpr False <*> pleaseSym (Special '$')) 
                           else empty)
          <|> foldr1 (<|>) [(Paren <$> sym l <*> pExpr outsideMath <*> pleaseSym r) | (l,r) <- parens]
          <|> (Atom <$> sym' isNoise)
          <|> (Error <$> recoverWith (sym' (not . ((||) <$> isNoise <*> (`elem` openParens)))))

-- TODO: (optimization) make sure we take in account the begin, so we don't return useless strokes
getStrokes :: Point -> Point -> Point -> [Tree TT] -> [Stroke]
getStrokes point _begin _end t0 = result 
    where getStrokes' (Atom t) = ts id t
          getStrokes' (Error t) = ts (modStroke errorStyle) t -- paint in red
          getStrokes' (Paren l g r)
              -- we have special treatment for (Begin, End) because these blocks are typically very large.
              -- we don't force the "end" part to prevent parsing the whole file.
              | isBegin (tokT l) = if (posnOfs $ tokPosn $ l) /= point 
                  then normalPaint
                  else case (tokT l, tokT r) of
                         (Begin b, End e) | b == e -> hintPaint
                         _ -> errPaint 
              | isErrorTok (tokT r) = errPaint
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) == point || (posnOfs $ tokPosn $ r) == point - 1
               = hintPaint
              | otherwise = normalPaint
              where normalPaint = ts id l . getStrokesL g . tsEnd id l r
                    hintPaint = ts (modStroke hintStyle) l . getStrokesL g . tsEnd (modStroke hintStyle) l r
                    errPaint = ts (modStroke errorStyle) l . getStrokesL g

          tsEnd _ (Tok{tokT = Begin b}) t@(Tok{tokT = End e}) 
              | b /= e = ts (modStroke errorStyle) t
          tsEnd f _ t = ts f t
          getStrokesL g = list (fmap getStrokes' g)
          ts f t 
              | isErrorTok (tokT t) = id
              | otherwise = (f (tokenToStroke t) :)
          list = foldr (.) id
          result = getStrokesL t0 []

modStroke :: StyleName -> Stroke -> Stroke
modStroke f (l,s,r) = (l,f `mappend` s,r) 

tokenToStroke :: TT -> Stroke
tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn +~ len)

tokenToStyle :: Token -> StyleName
tokenToStyle t =
  case t of
    Comment -> commentStyle
    Text -> defaultStyle
    Special _ -> defaultStyle
    Command _ -> typeStyle
    Begin _ -> keywordStyle
    End _ -> keywordStyle
    NewCommand -> keywordStyle

isSpecial :: [Char] -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

isBegin, isEnd :: Token -> Bool
isBegin (Begin _) = True
isBegin _ = False
isEnd (End _) = True
isEnd _ = False

isErrorTok :: Token -> Bool
isErrorTok = isSpecial "!"


