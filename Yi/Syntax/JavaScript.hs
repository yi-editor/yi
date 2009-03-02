-- (C) Copyright 2009 Deniz Dogan

module Yi.Syntax.JavaScript where

import Data.Monoid (Endo(..), mappend)
import Prelude ()
import Yi.Buffer.Basic (Point(..))
import Yi.IncrementalParse (P, eof, symbol)
import Yi.Lexer.Alex
import Yi.Lexer.JavaScript (TT, Token(..), Reserved(..), tokenToStyle)
import Yi.Prelude
import Yi.Style (errorStyle, StyleName)
import Yi.Syntax.Tree (sepBy)


data Tree t = FunDecl t t t t t [Tree t] t
            | VarDecl t [t]
            | Anything t
              deriving Show

instance Functor Tree where
    fmap = fmapDefault

instance Foldable Tree where
    foldMap = foldMapDefault

instance Traversable Tree where


parse :: P TT [Tree TT]
parse = parse' tokT tokFromT


parse' :: (TT -> Token) -> (Token -> TT) -> P TT [Tree TT]
parse' toTok fromTok = pForest <* eof
    where
      pForest :: P TT [Tree TT]
      pForest = many pTree

      pTree :: P TT (Tree TT)
      pTree = topLevel

      topLevel = funDecl <|> anyLevel
      anyLevel = varDecl <|> Anything <$> anyToken

      funDecl = FunDecl <$> res Function' <*> name
            <*> spec '(' <*> spec ')' <*> spec '{'
            <*> many anyLevel
            <*> spec '}'

      varDecl = VarDecl <$> res Var' <*> name `sepBy` spec ','

      name = symbol (\t -> case toTok t of
                             ValidName _ -> True
                             _           -> False)

      res x = symbol (\t -> case toTok t of
                              Res y -> x == y
                              _     -> False)

      spec x = symbol (\t -> case toTok t of
                               Special y -> x == y
                               _         -> False)

      anyToken = symbol (const True)


getStrokes :: Point -> Point -> Point -> [Tree TT] -> [Stroke]
getStrokes point begin end t0 = trace (show t0) result
    where
      result = appEndo (foldMap getStrokes' t0) []

      getStrokes' (FunDecl t _ _ _ _ _ _)  = one (tokenToStroke t)
      getStrokes' (VarDecl t _)  = one (tokenToStroke t)
      getStrokes' (Anything t) = one (modStroke errorStyle (tokenToStroke t))

      one x = Endo (x :)

modStroke :: StyleName -> Stroke -> Stroke
modStroke f = fmap (f `mappend`)

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan
