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
isNoise (Special _) = False
isNoise _ = False

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

instance IsTree Tree where
    subtrees (Paren _ g _) = g
    subtrees _ = []

parse :: P TT [Tree TT]
parse = parse' tokT tokFromT

parse' :: (TT -> Token) -> (Token -> TT) -> P TT [Tree TT]
parse' toTok fromT = pExpr True <* eof
    where 
      -- | parse a special symbol
      sym' p = symbol (p . toTok)
      sym t = sym' (== t)

      -- | Create a special character symbol
      newT c = fromT (Special c)

      pleaseSym c = recoverWith (pure $ newT '!') <|> sym c

      -- pExpr :: P TT (Expr TT)
      pExpr = many . pTree

      parens = -- (Begin, End) : -- uses a lot of CPU because of \begin{document} is matched only at the end.
               -- why exactly? it should be investigated.
               [(Special x, Special y) | (x,y) <- zip "({[" ")}]"]
      openParens = fmap fst parens

      pTree :: Bool -> P TT (Tree TT)
      pTree acceptDollar = 
          (if acceptDollar then (Paren <$> sym (Special '$') <*> pExpr False <*> pleaseSym (Special '$')) else empty)
          <|> foldr1 (<|>) [(Paren <$> sym l <*> pExpr True <*> pleaseSym r) | (l,r) <- parens]
          <|> (Atom <$> sym' isNoise)
          <|> (Error <$> recoverWith (sym' (not . ((||) <$> isNoise <*> (`elem` openParens)))))

-- TODO: (optimization) make sure we take in account the begin, so we don't return useless strokes
getStrokes :: Point -> Point -> Point -> [Tree TT] -> [Stroke]
getStrokes point _begin _end t0 = result 
    where getStrokes' (Atom t) = (ts t :)
          getStrokes' (Error t) = (modStroke errorStyle (ts t) :) -- paint in red
          getStrokes' (Paren l g r)
              | Begin /= tokT r && isErrorTok (tokT r) = (modStroke errorStyle (ts l) :) . getStrokesL g
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

tokenToStyle :: Token -> StyleName
tokenToStyle t =
  case t of
    Comment -> commentStyle
    Text -> defaultStyle
    Special _ -> operatorStyle
    Command _ -> upperIdStyle
    Begin -> keywordStyle
    End -> keywordStyle
    NewCommand -> keywordStyle

isSpecial :: [Char] -> Token -> Bool
isSpecial cs (Special c) = c `elem` cs
isSpecial _  _ = False

isErrorTok :: Token -> Bool
isErrorTok = isSpecial "!"


