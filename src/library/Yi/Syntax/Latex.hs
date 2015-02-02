{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- uniplate patterns
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Syntax.Latex
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser used by the LaTeX modes.

module Yi.Syntax.Latex where

import           Control.Applicative (Alternative ((<|>), empty, many),
                                      Applicative ((<*), (<*>), pure), (<$>))
import           Data.Foldable       (Foldable, foldMap)
import           Data.Monoid         (Endo (..), Monoid (mappend, mempty), (<>))
import           Data.Traversable    (Traversable (sequenceA))
import           Yi.IncrementalParse (P, eof, recoverWith, symbol)
import           Yi.Lexer.Alex       hiding (tokenToStyle)
import           Yi.Lexer.Latex      (Token (..), tokenToText)
import           Yi.Style
import           Yi.Syntax           (Point, Span)
import           Yi.Syntax.Tree      (IsTree (emptyNode, uniplate))

isNoise :: Token -> Bool
isNoise Text = True
isNoise Comment = True
isNoise (Command _) = True
isNoise NewCommand = True
isNoise (Special ' ') = True
isNoise (Special _) = False
isNoise (Begin _) = False
isNoise (End _) = False

type TT = Tok Token

type Expr t = [Tree t]

data Tree t
    = Paren t (Tree t) t -- A parenthesized expression (maybe with [ ] ...)
    | Atom t
    | Error t
    | Expr (Expr t)
      deriving (Show, Functor, Foldable)


instance IsTree Tree where
    uniplate (Paren l g r) = ([g], \[g'] -> Paren l g' r)
    uniplate (Expr g) = (g, Expr)
    uniplate t = ([],const t)
    emptyNode = Expr []

parse :: P TT (Tree TT)
parse = pExpr True <* eof
    where
      -- Create a special character symbol
      newT c = tokFromT (Special c)
      -- errT = (\next -> case next of
      --     Nothing -> newT '!'
      --     Just (Tok {tokPosn = posn}) -> Tok { tokT = Special '!', tokPosn = posn-1, tokSize = 1 -- FIXME: size should be 1 char, not one byte!
      --                      }) <$> lookNext
      errT = pure (newT '!')
      -- parse a special symbol
      sym' p = symbol (p . tokT)
      sym t = sym' (== t)

      pleaseSym c = recoverWith errT <|> sym c
      -- pleaseSym' c = recoverWith errT <|> sym' c

      -- pExpr :: P TT [Expr TT]
      pExpr outsideMath = Expr <$> many (pTree outsideMath)

      parens = [(Special x, Special y) | (x,y) <- zip "({[" ")}]"]
      openParens = fmap fst parens

      pBlock = sym' isBegin >>= \beg@Tok {tokT = Begin env} -> Paren <$> pure beg <*> pExpr True <*> pleaseSym (End env)

      pTree :: Bool -> P TT (Tree TT)
      pTree outsideMath =
          (if outsideMath then pBlock <|> (Paren <$> sym (Special '$') <*> pExpr False <*> pleaseSym (Special '$'))
                           else empty)
          <|> foldr1 (<|>) [Paren <$> sym l <*> pExpr outsideMath <*> pleaseSym r | (l,r) <- parens]
          <|> (Atom <$> sym' isNoise)
          <|> (Error <$> recoverWith (sym' (not . ((||) <$> isNoise <*> (`elem` openParens)))))

getStrokes :: Point -> Point -> Point -> Tree TT -> [Stroke]
getStrokes point _begin _end t0 = appEndo result []
    where getStrokes' :: Tree TT -> Endo [Stroke]
          getStrokes' (Expr g) = getStrokesL g
          getStrokes' (Atom t) = ts id t
          getStrokes' (Error t) = ts (modStroke errorStyle) t -- paint in red
          getStrokes' (Paren l g r)
              -- we have special treatment for (Begin, End) because these blocks are typically very large.
              -- we don't force the "end" part to prevent parsing the whole file.
              | isBegin (tokT l) = if posnOfs (tokPosn l) /= point
                  then normalPaint
                  else case (tokT l, tokT r) of
                         (Begin b, End e) | b == e -> hintPaint
                         _ -> errPaint
              | isErrorTok (tokT r) = errPaint
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | posnOfs (tokPosn l) == point || posnOfs (tokPosn r) == point - 1 = hintPaint
              | otherwise = normalPaint
              where normalPaint = ts id l <> getStrokes' g <> tsEnd id l r
                    hintPaint = ts (modStroke hintStyle) l <> getStrokes' g <> tsEnd (modStroke hintStyle) l r
                    errPaint = ts (modStroke errorStyle) l <> getStrokes' g

          tsEnd _ (Tok{tokT = Begin b}) t@(Tok{tokT = End e})
              | b /= e = ts (modStroke errorStyle) t
          tsEnd f _ t = ts f t
          getStrokesL :: Expr TT -> Endo [Stroke]
          getStrokesL = foldMap getStrokes'
          ts f t
              | isErrorTok (tokT t) = mempty
              | otherwise = Endo (f (tokenToStroke t) :)
          result = getStrokes' t0

modStroke :: StyleName -> Stroke -> Stroke
modStroke f = fmap (f `mappend`)

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

tokenToAnnot :: TT -> Maybe (Span String)
tokenToAnnot = sequenceA . tokToSpan . fmap tokenToText

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

isSpecial :: String -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

isBegin, isEnd :: Token -> Bool
isBegin (Begin _) = True
isBegin _ = False
isEnd (End _) = True
isEnd _ = False

isErrorTok :: Token -> Bool
isErrorTok = isSpecial "!"
