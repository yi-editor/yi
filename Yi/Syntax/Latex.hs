{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
-- Copyright (c) JP Bernardy 2008
module Yi.Syntax.Latex where

import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.Latex
import Yi.Style
import Yi.Syntax.Tree
import Yi.Syntax.BList
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
isNoise (Special ' ') = True
isNoise (Special _) = False
isNoise (Begin _) = False
isNoise (End _) = False

type Expr t = BList (Tree t)

type TT = Tok Token


data Tree t
    = Paren t (Expr t) t -- A parenthesized expression (maybe with [ ] ...)
    | Atom t
    | Error t
      deriving Show



--instance Functor Tree where
--  fmap = fmapDefault

instance Foldable Tree where
    foldMap f (Atom t) = f t
    foldMap f (Error t ) = f t
    foldMap f (Paren l g r) = f l <> foldMap (foldMap f) g <> f r

--instance Traversable Tree where
--    traverse f (Atom t) = Atom <$> f t
--    traverse f (Error t) = Error <$> f t
--    traverse f (Paren l g r) = Paren <$> f l <*> traverse (traverse f) g <*> f r

instance SubTree (Tree TT) where
    type Element (Tree TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (Atom t) = f t
              work (Error t) = f t
              work (Paren l g r) = f l <> foldMapAfter begin (foldMapToksAfter begin f) g <> f r
    foldMapToks f = foldMap (foldMapToks f)


instance IsTree Tree where
    subtrees (Paren _ g _) = toList g
    subtrees _ = []

parse :: P TT (Expr TT)
parse = pExpr True <* eof
    where 
      -- | Create a special character symbol
      newT c = tokFromT (Special c)
      -- errT = (\next -> case next of 
      --     Nothing -> newT '!'
      --     Just (Tok {tokPosn = posn}) -> Tok { tokT = Special '!', tokPosn = posn-1, tokSize = 1 -- FIXME: size should be 1 char, not one byte!
      --                      }) <$> lookNext 
      errT = pure (newT '!')
      -- | parse a special symbol
      sym' p = symbol (p . tokT)
      sym t = sym' (== t)

      pleaseSym c = recoverWith errT <|> sym c
      -- pleaseSym' c = recoverWith errT <|> sym' c

      -- pExpr :: P TT [Expr TT]
      pExpr = Yi.Syntax.BList.many . pTree

      parens = [(Special x, Special y) | (x,y) <- zip "({[" ")}]"]
      openParens = fmap fst parens

      pBlock = sym' isBegin >>= \beg@Tok {tokT = Begin env} -> Paren <$> pure beg <*> pExpr True <*> pleaseSym (End env)

      pTree :: Bool -> P TT (Tree TT)
      pTree outsideMath = 
          (if outsideMath then pBlock <|> (Paren <$> sym (Special '$') <*> pExpr False <*> pleaseSym (Special '$')) 
                           else empty)
          <|> foldr1 (<|>) [(Paren <$> sym l <*> pExpr outsideMath <*> pleaseSym r) | (l,r) <- parens]
          <|> (Atom <$> sym' isNoise)
          <|> (Error <$> recoverWith (sym' (not . ((||) <$> isNoise <*> (`elem` openParens)))))

getStrokes :: Point -> Point -> Point -> Expr TT -> [Stroke]
getStrokes point begin _end t0 = appEndo result []
    where getStrokes' :: Tree TT -> Endo [Stroke]
          getStrokes' (Atom t) = ts id t
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
              where normalPaint = ts id l <> getStrokesL g <> tsEnd id l r
                    hintPaint = ts (modStroke hintStyle) l <> getStrokesL g <> tsEnd (modStroke hintStyle) l r
                    errPaint = ts (modStroke errorStyle) l <> getStrokesL g

          tsEnd _ (Tok{tokT = Begin b}) t@(Tok{tokT = End e}) 
              | b /= e = ts (modStroke errorStyle) t
          tsEnd f _ t = ts f t
          getStrokesL :: Expr TT -> Endo [Stroke]
          getStrokesL = foldMapAfter begin  getStrokes'
          ts f t 
              | isErrorTok (tokT t) = mempty
              | otherwise = Endo (f (tokenToStroke t) :)
          result = getStrokesL t0

modStroke :: StyleName -> Stroke -> Stroke
modStroke f = fmap (f `mappend`)

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

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


