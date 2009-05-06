{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- (C) Copyright 2009 Deniz Dogan

module Yi.Syntax.JavaScript where

import Parser.Incremental (Parser(..))
import Data.DeriveTH
import Data.Derive.Foldable
import Data.Monoid (Endo(..))
import Prelude (unlines, map)
import Yi.Buffer.Basic (Point(..))
import Yi.IncrementalParse (P, eof, symbol, recoverWith)
import Yi.Lexer.Alex
import Yi.Lexer.JavaScript (TT, Token(..), Reserved(..), Operator(..), tokenToStyle)
import Yi.Prelude
import Yi.Style (errorStyle, StyleName)
import Yi.Syntax.Tree (sepBy, sepBy1)


-- * Data types, classes and instances

-- | Instances of @Strokable@ are datatypes which can be syntax highlighted.
class Strokable a where
    toStrokes :: a -> Endo [Stroke]

type Tree t = [Statement t]

data Statement t = FunDecl t t t [t] t (Block t)
                 | VarDecl t [VarDecAss t] t
                 | Return t (Expr t) t
                 | While t t (Expr t) t (Block t)
                 | DoWhile t (Block t) t t (Expr t) t t
                 | For t t (Expr t) t (Expr t) t (Expr t) t (Block t)
                 | Expr (Expr t) t -- semi-colon
                 | Err t
                   deriving (Eq, Show)

data Block t = Block t [Statement t] t
             | BlockOne (Statement t)
             | BlockErr t
               deriving (Eq, Show)

-- | Represents either a variable name or a variable name assigned to an
--   expression.  @AssignNo@ means it's a simple declaration.  @AssignYes@ means
--   a declaration and an assignment.  @AssignErr@ is used as a recovery.
data VarDecAss t = Ass1 t (Maybe (VarDecAss t))
                 | Ass2 t (Expr t)
                 | AssignErr t
                   deriving (Eq, Show)

data Expr t = ExprObj t [KeyValue t] t
            | ExprSimple t
            | ExprParen t (Expr t) t
            | ExprName t -- (Maybe (Expr t))
            | ExprThis t -- (Maybe (Expr t))
            | ExprAnonFun t t [t] t (Block t)
            | ExprFunCall t t [Expr t] t (Maybe (Expr t))
            | ExprAssign t t (Expr t)
            | ExprErr t
              deriving (Eq, Show)

data Qual t = Qual t (Expr t)
            | QErr t
              deriving (Eq, Show)

data KeyValue t = KeyValue t t (Expr t)
                | KeyValueErr t
                  deriving (Eq, Show)

$(derive makeFoldable ''Qual)
$(derive makeFoldable ''Statement)
$(derive makeFoldable ''Expr)
$(derive makeFoldable ''KeyValue)
$(derive makeFoldable ''VarDecAss)
$(derive makeFoldable ''Block)

-- TODO: (Optimization) Only make strokes for stuff that don't have
-- defaultStyle.  I'm not entirely sure how much this would help performance,
-- but it should be kept in mind for the future...
instance Strokable (Tok Token) where
    toStrokes t = if isError t
                    then one (modStroke errorStyle . tokenToStroke) t
                    else one tokenToStroke t

instance Strokable (Statement TT) where
    toStrokes (Err t) = one (modStroke errorStyle . tokenToStroke) t
    toStrokes t = foldMap toStrokes t

instance Strokable (Block TT) where
    toStrokes = foldMap toStrokes

instance Strokable (VarDecAss TT) where
    toStrokes = foldMap toStrokes

instance Strokable (Expr TT) where
    toStrokes expr = foldMap toStrokes expr

instance Strokable (KeyValue TT) where
    toStrokes = foldMap toStrokes


-- * Helper functions.

one :: (t -> a) -> t -> Endo [a]
one f x = Endo (f x :)

modStroke :: StyleName -> Stroke -> Stroke
modStroke style stroke = fmap (style <>) stroke

oneStroke :: TT -> Endo [Stroke]
oneStroke = one tokenToStroke


-- * Stroking functions

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

getStrokes :: Point -> Point -> Point -> Tree TT -> [Stroke]
getStrokes _point _begin _end t0 = trace (unlines $ map show t0) result
    where
      result = appEndo (foldMap toStrokes t0) []


-- * The parser

-- | Main parser.
parse :: P TT (Tree TT)
parse = many statement <* eof

-- | Parser for statements such as "return", "while", "do-while", "for", etc.
statement :: P TT (Statement TT)
statement = FunDecl <$> resWord Function' <*> plzTok name -- no need to plz because of [1]
                    <*> plzSpc '(' <*> parameters <*> plzSpc ')' <*> block
        <|> VarDecl <$> resWord Var' <*> sepBy1 varDecAss (spc ',') <*> plzSpc ';'
        <|> Return  <$> resWord Return' <*> plzExpr <*> plzSpc ';'
        <|> While   <$> resWord While' <*> plzSpc '(' <*> plzExpr <*> plzSpc ')' <*> block
        <|> DoWhile <$> resWord Do' <*> block <*> plzTok (resWord While')
                    <*> plzSpc '(' <*> plzExpr <*> plzSpc ')' <*> plzSpc ';'
        <|> For     <$> resWord For' <*> plzSpc '(' <*> plzExpr <*> plzSpc ';'
                    <*> plzExpr <*> plzSpc ';' <*> plzExpr <*> plzSpc ')' <*> block
        <|> Expr    <$> expression <*> plzSpc ';' -- [1]
        <|> Err     <$> recoverWith (symbol $ const True)
    where
      varDecAss :: P TT (VarDecAss TT)
      varDecAss = Ass1      <$> name <*> optional (Ass2 <$> oper Assign' <*> plzExpr)
              <|> AssignErr <$> anything

-- | Parser for old-style function bodies and "lambda style" ones.
block :: P TT (Block TT)
block = Block    <$> spc '{' <*> many statement <*> plzSpc '}'
    <|> BlockOne <$> statement
    <|> BlockErr <$> anything

-- | Parser for expressions.
expression :: P TT (Expr TT)
expression = ExprSimple  <$> simpleTok
         <|> ExprName    <$> name
         <|> ExprParen   <$> spc '(' <*> plzExpr <*> plzSpc ')'
         <|> ExprThis    <$> resWord This'
         <|> ExprObj     <$> spc '{' <*> commas keyValue <*> plzSpc '}'
         <|> ExprAnonFun <$> resWord Function' <*> plzSpc '(' <*> parameters <*> plzSpc ')' <*> block
         <|> ExprFunCall <$> name <*> plzSpc '(' <*> arguments <*> plzSpc ')' <*> optional qual
         <|> ExprAssign  <$> name <*> plzSpc '=' <*> expression
    where
      keyValue = KeyValue    <$> name <*> plzSpc ':' <*> plzExpr
             <|> KeyValueErr <$> anything


-- * Parsing helpers

-- | Parses a qualified expression.  TODO: Not all expressions can be the RHS of
--   the qualification operator.
qual :: P TT (Expr TT)
qual = Enter "Qual" $ Qual <$> spc '.' *> (expression <|> ExprErr <$> anything)

-- | Parser for comma-separated identifiers.
parameters :: P TT [TT]
parameters = Enter "parameters" $ commas (plzTok name)

-- | Parser for comma-separated expressions.
arguments :: P TT [Expr TT]
arguments = Enter "arguments" $ commas plzExpr

-- | Intersperses parses with comma parsers.
commas :: P TT a -> P TT [a]
commas x = Enter "commas" $ x `sepBy` spc ','


-- * Simple parsers

-- | Parses any literal.
simpleTok = Enter "simpleTok" $ symbol (\t -> case fromTT t of
                         Str _       -> True
                         Number _    -> True
                         Res y       -> y `elem` [True', False', Undefined', Null']
                         _           -> False)

-- | Parses any string.
strTok :: P TT TT
strTok = Enter "strTok" $ symbol (\t -> case fromTT t of
                         Str _ -> True
                         _     -> False)

-- | Parses any valid number.
numTok :: P TT TT
numTok = Enter "numTok" $ symbol (\t -> case fromTT t of
                         Number _ -> True
                         _        -> False)

-- | Parses any valid identifier.
name :: P TT TT
name = Enter "name" $ symbol (\t -> case fromTT t of
                       ValidName _ -> True
                       _           -> False)

-- | Parses any boolean.
boolean :: P TT TT
boolean = Enter "boolean" $ symbol (\t -> case fromTT t of
                          Res y -> y `elem` [True', False']
                          _     -> False)

-- | Parses a reserved word.
resWord :: Reserved -> P TT TT
resWord x = Enter ("resWord " ++ show x) $ symbol (\t -> case fromTT t of
                            Res y -> x == y
                            _     -> False)

-- | Parses a special token.
spc :: Char -> P TT TT
spc x = Enter ("spc " ++ show x) $ symbol (\t -> case fromTT t of
                        Special y -> x == y
                        _         -> False)

-- | Parses an operator.
oper :: Operator -> P TT TT
oper x = Enter ("oper " ++ show x) $ symbol (\t -> case fromTT t of
                         Op y -> y == x
                         _    -> False)


-- * Recovery parsers

-- | Expects a token x, recovers with 'errorToken'.
plzTok :: P TT TT -> P TT TT
plzTok x = x <|> anything

-- | Expects a special token.
plzSpc :: Char -> P TT TT
plzSpc x = Enter ("plzSpc " ++ show x) $ plzTok (spc x)

-- | Expects an expression.
plzExpr :: P TT (Expr TT)
plzExpr = expression <|> (ExprErr <$> anything)

-- | General recovery parser, inserts an error token.
anything :: P s TT
anything = recoverWith (pure errorToken)


-- * Utility stuff

errorToken :: TT
errorToken = toTT $ Special '!'

isError :: TT -> Bool
isError (Tok (Special '!') _ _) = True
isError _ = False

-- | Better name for 'tokFromT'.
toTT :: t -> Tok t
toTT = tokFromT

-- | Better name for 'fromTT'.
fromTT :: Tok t -> t
fromTT = tokT
