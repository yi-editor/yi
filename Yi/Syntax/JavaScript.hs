{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- (C) Copyright 2009 Deniz Dogan

module Yi.Syntax.JavaScript where

import Data.Monoid (Endo(..), mempty)
import Prelude (unlines, map, maybe)
import Yi.Buffer.Basic (Point(..))
import Yi.IncrementalParse (P, eof, symbol, recoverWith)
import Yi.Lexer.Alex
import Yi.Lexer.JavaScript ( TT, Token(..), Reserved(..), Operator(..)
                           , tokenToStyle, prefixOperators, infixOperators, postfixOperators )
import Yi.Prelude hiding (error, Const)
import Yi.Style (errorStyle, StyleName)
import Yi.Syntax.Tree (sepBy, sepBy1)
import Data.Ix (inRange)


-- * Data types, classes and instances

-- | Instances of @Strokable@ are datatypes which can be syntax highlighted.
class Strokable a where
    toStrokes :: (Point, Point) -> a -> Endo [Stroke]

-- | Instances of @Failable@ can represent failure.  This is a useful class for
--   future work, since then we can make stroking much easier.
class Failable f where
    stupid :: t -> f t
    hasFailed :: f t -> Bool

type Tree t = [Statement t]

type Semicolon t = Maybe t

data Statement t = FunDecl t t t [t] t (Block t)
                 | VarDecl t [VarDecAss t] (Semicolon t)
                 | Return t (Maybe (Expr t)) (Semicolon t)
                 | While t t (Expr t) t (Block t)
                 | DoWhile t (Block t) t t (Expr t) t (Semicolon t)
                 | For t t (Expr t) t (Expr t) t (Expr t) t (Block t)
                 | If t t (Expr t) t (Block t) (Maybe (Statement t))
                 | Else t (Block t)
                 | With t t (Expr t) t (Block t)
                 | Expr (Expr t) (Semicolon t)
                   deriving (Eq, Show)

data Block t = Block t [Statement t] t
             | BlockOne (Statement t)
             | BlockErr t
               deriving (Eq, Show)

instance Failable Block where
    stupid = BlockErr
    hasFailed t = case t of
                    BlockErr _ -> True
                    _          -> False

-- | Represents either a variable name or a variable name assigned to an
--   expression.  @Ass1@ is a variable name /maybe/ followed by an assignment.
--   @Ass2@ is an equals sign and an expression.  @(Ass1 'x' (Just (Ass2 '='
--   '5')))@ (pseudo-syntax of course) means @x = 5@.
data VarDecAss t = Ass1 t (Maybe (VarDecAss t))
                 | Ass2 t (Expr t)
                 | AssignErr t
                   deriving (Eq, Show)

instance Failable VarDecAss where
    stupid = AssignErr
    hasFailed t = case t of
                    AssignErr _ -> True
                    _           -> False

data Expr t = ExprObj t [KeyValue t] t
            | ExprPrefix t (Expr t)
            | ExprSimple t (Maybe (Expr t))
            | ExprConst t
            | ExprParen t (Expr t) t (Maybe (Expr t))
            | ExprAnonFun t t [t] t (Block t)
            | ExprFunCall t t [Expr t] t (Maybe (Expr t))
            | OpExpr t (Expr t)
            | ExprCond t (Expr t) t (Expr t)
            | ExprArr t (Expr t) t
            | PostExpr t
            | ExprErr t
              deriving (Eq, Show)

instance Failable Expr where
    stupid = ExprErr
    hasFailed t = case t of
                    ExprErr _ -> True
                    _         -> False

data KeyValue t = KeyValue t t (Expr t)
                | KeyValueErr t
                  deriving (Eq, Show)

instance Failable KeyValue where
    stupid = KeyValueErr
    hasFailed t = case t of
                    KeyValueErr _ -> True
                    _             -> False

instance Strokable (Tok Token) where
    toStrokes _ t = if isError t
                      then one (modStroke errorStyle . tokenToStroke) t
                      else one tokenToStroke t


-- | TODO: This code is *screaming* for some generic programming.
--
--   TODO: Somehow fix Failable and failStroker to be more "generic".  This will
--   make these instances much nicer and we won't have to make ad-hoc stuff like
--   this.
instance Strokable (Statement TT) where
    toStrokes o (FunDecl f n l ps r blk) =
        let pos = posnOfs (tokPosn f) in
        if not (inRange o pos)
          then trace "%%%%" mempty
          else let s = trace "!!!!" (if hasFailed blk then error else failStroker [n, l, r]) in
               s f <> s n <> s l <> foldMap (toStrokes o) ps <> s r <> toStrokes o blk
    toStrokes o (VarDecl v vs sc) = normal v <> foldMap (toStrokes o) vs <> maybe mempty normal sc
    toStrokes o (Return t exp sc) = normal t <> maybe mempty (toStrokes o) exp <> maybe mempty normal sc
    toStrokes o (While w l exp r blk) =
        let s = if hasFailed blk then error else failStroker [w, l, r] in
        s w <> s l <> toStrokes o exp <> s r <> toStrokes o blk
    toStrokes o (DoWhile d blk w l exp r sc) =
        let s = if hasFailed blk then error else failStroker [d, w, l, r] in
        s d <> toStrokes o blk <> s w <> s l <> toStrokes o exp <> s r <> maybe mempty normal sc
    toStrokes o (For f l x1 s1 x2 s2 x3 r blk) =
        let s = if hasFailed blk then error else failStroker [f, l, s1, s2, r] in
        s f <> s l <> toStrokes o x1 <> s s1 <> toStrokes o x2 <> s s2 <> toStrokes o x3 <> s r <> toStrokes o blk
    toStrokes o (If i l x r blk e) =
        let s = failStroker [i, l, r] in
        s i <> s l <> toStrokes o x <> s r <> toStrokes o blk <> maybe mempty (toStrokes o) e
    toStrokes o (Else e blk) =
        let s = if hasFailed blk then error else normal in
        s e <> toStrokes o blk
    toStrokes o (With w l x r blk) =
        let s = if hasFailed blk then error else failStroker [w, l, r] in
        s w <> s l <> toStrokes o x <> s r <> toStrokes o blk
    toStrokes o (Expr exp sc) = toStrokes o exp <> maybe mempty normal sc

instance Strokable (Block TT) where
    toStrokes o (BlockOne stmt) = toStrokes o stmt
    toStrokes o (Block l stmts r) =
        let s = failStroker [l, r] in
        s l <> foldMap (toStrokes o) stmts <> s r
    toStrokes _ (BlockErr t) = error t

instance Strokable (VarDecAss TT) where
    toStrokes o (Ass1 t x) = normal t <> maybe mempty (toStrokes o) x
    toStrokes o (Ass2 t exp) = normal t <> toStrokes o exp
    toStrokes _ (AssignErr t) = error t

instance Strokable (Expr TT) where
    toStrokes o (ExprSimple x exp) = normal x <> maybe mempty (toStrokes o) exp
    toStrokes o (ExprObj l kvs r) =
        let s = failStroker [l, r] in
        s l <> foldMap (toStrokes o) kvs <> s r
    toStrokes o (ExprPrefix t exp) = normal t <> toStrokes o exp
    toStrokes _ (ExprConst t) = normal t
    toStrokes o (ExprParen l exp r op) =
        let s = failStroker [l, r] in
        s l <> toStrokes o exp <> s r <> maybe mempty (toStrokes o) op
    toStrokes o (ExprAnonFun f l ps r blk) =
        let s = failStroker [f, l, r] in
        s f <> s l <> foldMap (toStrokes o) ps <> s r <> toStrokes o blk
    toStrokes o (ExprFunCall n l exps r m) =
        let s = failStroker [n, l, r] in
        s n <> s l <> foldMap (toStrokes o) exps <> s r <> maybe mempty (toStrokes o) m
    toStrokes o (OpExpr op exp) = normal op <> toStrokes o exp
    toStrokes _ (PostExpr t) = normal t
    toStrokes o (ExprCond a x b y) =
        let s = failStroker [a, b] in
        s a <> toStrokes o x <> s b <> toStrokes o y
    toStrokes o (ExprArr l x r) =
        let s = failStroker [l, r] in
        s l <> toStrokes o x <> s r
    toStrokes _ (ExprErr t) = error t

instance Strokable (KeyValue TT) where
    toStrokes o (KeyValue n c exp) =
        let s = failStroker [n, c] in
        s n <> s c <> toStrokes o exp
    toStrokes _ (KeyValueErr t) = error t


-- * Helper functions.

-- | Normal stroker.
normal :: TT -> Endo [Stroke]
normal x = one tokenToStroke x

-- | Error stroker.
error :: TT -> Endo [Stroke]
error x = one (modStroke errorStyle . tokenToStroke) x

one :: (t -> a) -> t -> Endo [a]
one f x = Endo (f x :)

-- | Given a new style and a stroke, return a stroke with the new style appended
--   to the old one.
modStroke :: StyleName -> Stroke -> Stroke
modStroke style stroke = fmap (style <>) stroke


-- * Stroking functions

-- | Given a list of tokens to check for errors (@xs@) and a list of tokens to
--   stroke (@xs'@), returns normal strokes for @xs'@ if there were no errors.
--   Otherwise returns error strokes for @xs'@.
nError :: [TT] -> [TT] -> Endo [Stroke]
nError xs xs' = foldMap (failStroker xs) xs'

-- | Given a list of @TT@, if any of them is an error, returns an error stroker,
--   otherwise a normal stroker.  Using e.g. existentials, we could make this
--   more general and have support for heterogeneous lists of elements which
--   implement Failable, but I haven't had the time to fix this.
failStroker :: [TT] -> TT -> Endo [Stroke]
failStroker xs = if any isError xs then error else normal

-- | Given a @TT@, return a @Stroke@ for it.
tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

-- | The main stroking function.
getStrokes :: Point -> Point -> Point -> Tree TT -> [Stroke]
getStrokes _point b e t0 = trace ("\n" ++ (unlines (map show t0))) result
    where
      result = appEndo (foldMap (toStrokes (b, e)) t0) []


-- * The parser

-- | Main parser.
parse :: P TT (Tree TT)
parse = many statement <* eof

-- | Parser for statements such as "return", "while", "do-while", "for", etc.
statement :: P TT (Statement TT)
statement = FunDecl <$> resWord Function' <*> plzTok name
                    <*> plzSpc '(' <*> parameters <*> plzSpc ')' <*> block
        <|> VarDecl <$> resWord Var' <*> sepBy1 (plz varDecAss) (spc ',') <*> semicolon
        <|> Return  <$> resWord Return' <*> optional expression <*> semicolon
        <|> While   <$> resWord While' <*> plzSpc '(' <*> plzExpr <*> plzSpc ')' <*> block
        <|> DoWhile <$> resWord Do' <*> block <*> plzTok (resWord While')
                    <*> plzSpc '(' <*> plzExpr <*> plzSpc ')' <*> semicolon
        <|> For     <$> resWord For' <*> plzSpc '(' <*> plzExpr <*> plzSpc ';'
                    <*> plzExpr <*> plzSpc ';' <*> plzExpr <*> plzSpc ')' <*> block
        <|> If      <$> resWord If' <*> plzSpc '(' <*> plzExpr <*> plzSpc ')'
                    <*> block <*> optional (Else <$> resWord Else' <*> block)
        <|> With    <$> resWord With' <*> plzSpc '(' <*> plzExpr <*> plzSpc ')'
                    <*> block
        <|> Expr    <$> stmtExpr <*> semicolon
    where
      varDecAss :: P TT (VarDecAss TT)
      varDecAss = Ass1 <$> name <*> optional (Ass2 <$> oper Assign' <*> plzExpr)

-- | Parser for "blocks", i.e. a bunch of statements wrapped in curly brackets
--   /or/ just a single statement.
--
--   Note that this works for JavaScript 1.8 "lambda" style function bodies as
--   well, e.g. "function hello() 5", since expressions are also statements and
--   we don't require a trailing semi-colon.
--
--   TODO: function hello() var x; is not a valid program.
block :: P TT (Block TT)
block = Block    <$> spc '{' <*> many statement <*> plzSpc '}'
    <|> BlockOne <$> hate 1 (statement)
    <|> BlockErr <$> hate 2 (pure errorToken)

-- | Parser for expressions which may be statements.  In reality, any expression
--   is also a valid statement, but this is a slight compromise to get rid of
--   the massive performance loss which is introduced when allowing JavaScript
--   objects to be valid statements.
stmtExpr :: P TT (Expr TT)
stmtExpr = ExprSimple  <$> simpleTok <*> optional (opExpr)
       <|> ExprConst   <$> symbol (\t -> case fromTT t of
                                           Const _ -> True
                                           _       -> False)
       <|> ExprParen   <$> spc '(' <*> plzExpr <*> plzSpc ')' <*> optional (opExpr)
       <|> ExprFunCall <$> name <*> plzSpc '(' <*> arguments <*> plzSpc ')' <*> optional (opExpr)
       <|> ExprPrefix  <$> preOp <*> plzExpr
       <|> ExprErr     <$> hate 1 (symbol (const True))
    where
      opExpr :: P TT (Expr TT)
      opExpr = OpExpr   <$> inOp <*> plzExpr
           <|> ExprCond <$> spc '?' <*> plzExpr <*> plzSpc ':' <*> plzExpr
           <|> ExprArr  <$> spc '[' <*> plzExpr <*> plzSpc ']'
           <|> PostExpr <$> postOp

-- | Parser for expressions.
expression :: P TT (Expr TT)
expression = ExprObj     <$> spc '{' <*> commas keyValue <*> plzSpc '}'
         <|> ExprAnonFun <$> resWord Function' <*> plzSpc '(' <*> parameters <*> plzSpc ')' <*> block
         <|> stmtExpr
    where
      keyValue :: P TT (KeyValue TT)
      keyValue = KeyValue    <$> name <*> plzSpc ':' <*> plzExpr
             <|> KeyValueErr <$> hate 1 (symbol (const True))
             <|> KeyValueErr <$> hate 2 (pure $ errorToken)


-- * Parsing helpers

semicolon :: P TT (Maybe TT)
semicolon = optional $ spc ';'

-- | Parser for comma-separated identifiers.
parameters :: P TT [TT]
parameters = commas (plzTok name)

-- | Parser for comma-separated expressions.
arguments :: P TT [Expr TT]
arguments = commas plzExpr

-- | Intersperses parses with comma parsers.
commas :: P TT a -> P TT [a]
commas x = x `sepBy` spc ','


-- * Simple parsers

-- | Parses a prefix operator.
preOp :: P TT TT
preOp = symbol (\t -> case fromTT t of
                        Op x -> x `elem` prefixOperators
                        _    -> False)

-- | Parses a infix operator.
inOp :: P TT TT
inOp = symbol (\t -> case fromTT t of
                       Op x -> x `elem` infixOperators
                       _    -> False)

-- | Parses a postfix operator.
postOp :: P TT TT
postOp = symbol (\t -> case fromTT t of
                         Op x -> x `elem` postfixOperators
                         _    -> False)

-- | Parses any literal.
opTok :: P TT TT
opTok = symbol (\t -> case fromTT t of
                        Op _ -> True
                        _    -> False)

-- | Parses any literal.
simpleTok :: P TT TT
simpleTok = symbol (\t -> case fromTT t of
                            Str _       -> True
                            Number _    -> True
                            ValidName _ -> True
                            Res y       -> y `elem` [True', False', Undefined', Null', This']
                            _           -> False)

-- | Parses any string.
strTok :: P TT TT
strTok = symbol (\t -> case fromTT t of
                         Str _ -> True
                         _     -> False)

-- | Parses any valid number.
numTok :: P TT TT
numTok = symbol (\t -> case fromTT t of
                         Number _ -> True
                         _        -> False)

-- | Parses any valid identifier.
name :: P TT TT
name = symbol (\t -> case fromTT t of
                       ValidName _ -> True
                       Const     _ -> True
                       _           -> False)

-- | Parses any boolean.
boolean :: P TT TT
boolean = symbol (\t -> case fromTT t of
                          Res y -> y `elem` [True', False']
                          _     -> False)

-- | Parses a reserved word.
resWord :: Reserved -> P TT TT
resWord x = symbol (\t -> case fromTT t of
                            Res y -> x == y
                            _     -> False)

-- | Parses a special token.
spc :: Char -> P TT TT
spc x = symbol (\t -> case fromTT t of
                        Special y -> x == y
                        _         -> False)

-- | Parses an operator.
oper :: Operator -> P TT TT
oper x = symbol (\t -> case fromTT t of
                         Op y -> y == x
                         _    -> False)


-- * Recovery parsers

-- | Expects a token x, recovers with 'errorToken'.
plzTok :: P TT TT -> P TT TT
plzTok x = x
       <|> hate 1 (symbol (const True))
       <|> hate 2 (pure errorToken)

-- | Expects a special token.
plzSpc :: Char -> P TT TT
plzSpc x = plzTok (spc x)

-- | Expects an expression.
plzExpr :: P TT (Expr TT)
plzExpr = plz expression

plz :: Failable f => P TT (f TT) -> P TT (f TT)
plz x = x
    <|> stupid <$> hate 1 (symbol (const True))
    <|> stupid <$> hate 2 (pure errorToken)

-- | General recovery parser, inserts an error token.
anything :: P s TT
anything = recoverWith (pure errorToken)

-- | Weighted recovery.
hate :: Int -> P s a -> P s a
hate n x = power n recoverWith $ x
    where
      power 0 _ = id
      power m f = f . power (m - 1) f


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
