{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Syntax.JavaScript
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for the JavaScript language as described at
-- <http://publications.lib.chalmers.se/records/fulltext/112284.pdf Dogan 2009>.
--
-- The mode using this parser can be found at "Yi.Mode.JavaScript".

module Yi.Syntax.JavaScript where

import           Prelude hiding (elem,error,any,exp)

import           Control.Applicative (Alternative ((<|>), many), Applicative ((<*), (<*>), pure), optional, (<$>))
import           Data.Data           (Data)
import           Data.Foldable       (Foldable (foldMap), any, elem, toList)
import           Data.Monoid         (Endo (..), Monoid (mempty), (<>))
import qualified Data.Text           as T (cons)
import           Data.Typeable       (Typeable)
import           Yi.Buffer.Basic     (Point (..))
import           Yi.Debug            (trace)
import           Yi.IncrementalParse (P, eof, recoverWith, symbol)
import           Yi.Lexer.Alex       (Stroke, Tok (..), tokFromT, tokToSpan)
import           Yi.Lexer.JavaScript
import           Yi.String           (showT)
import           Yi.Style            (StyleName, errorStyle)
import           Yi.Syntax.Tree      (IsTree (..), sepBy, sepBy1)

-- * Data types, classes and instances

-- | Instances of @Strokable@ are datatypes which can be syntax highlighted.
class Strokable a where
    toStrokes :: a -> Endo [Stroke]

-- | Instances of @Failable@ can represent failure.  This is a useful class for
--   future work, since then we can make stroking much easier.
class Failable f where
    stupid :: t -> f t
    hasFailed :: f t -> Bool

type BList a = [a]

type Tree t = BList (Statement t)

type Semicolon t = Maybe t

data Statement t = FunDecl t t (Parameters t) (Block t)
                 | VarDecl t (BList (VarDecAss t)) (Semicolon t)
                 | Return t (Maybe (Expr t)) (Semicolon t)
                 | While t (ParExpr t) (Block t)
                 | DoWhile t (Block t) t (ParExpr t) (Semicolon t)
                 | For t t (Expr t) (ForContent t) t (Block t)
                 | If t (ParExpr t) (Block t) (Maybe (Statement t))
                 | Else t (Block t)
                 | With t (ParExpr t) (Block t)
                 | Comm t
                 | Expr (Expr t) (Semicolon t)
                   deriving (Show, Data, Typeable, Foldable)

data Parameters t = Parameters t (BList t) t
                  | ParErr t
                    deriving (Show, Data, Typeable, Foldable)

data ParExpr t = ParExpr t (BList (Expr t)) t
               | ParExprErr t
                 deriving (Show, Data, Typeable, Foldable)

data ForContent t = ForNormal t (Expr t) t (Expr t)
                  | ForIn t (Expr t)
                  | ForErr t
                    deriving (Show, Data, Typeable, Foldable)

data Block t = Block t (BList (Statement t)) t
             | BlockOne (Statement t)
             | BlockErr t
               deriving (Show, Data, Typeable, Foldable)

-- | Represents either a variable name or a variable name assigned to an
--   expression.  @AssBeg@ is a variable name /maybe/ followed by an assignment.
--   @AssRst@ is an equals sign and an expression.  @(AssBeg 'x' (Just (AssRst
--   '=' '5')))@ means @x = 5@.
data VarDecAss t = AssBeg t (Maybe (VarDecAss t))
                 | AssRst t (Expr t)
                 | AssErr t
                   deriving (Show, Data, Typeable, Foldable)

data Expr t = ExprObj t (BList (KeyValue t)) t
            | ExprPrefix t (Expr t)
            | ExprNew t (Expr t)
            | ExprSimple t (Maybe (Expr t))
            | ExprParen t (Expr t) t (Maybe (Expr t))
            | ExprAnonFun t (Parameters t) (Block t)
            | ExprTypeOf t (Expr t)
            | ExprFunCall t (ParExpr t) (Maybe (Expr t))
            | OpExpr t (Expr t)
            | ExprCond t (Expr t) t (Expr t)
            | ExprArr t (Maybe (Array t)) t (Maybe (Expr t))
            | PostExpr t
            | ExprErr t
              deriving (Show, Data, Typeable, Foldable)

data Array t = ArrCont (Expr t) (Maybe (Array t))
             | ArrRest t (Array t) (Maybe (Array t))
             | ArrErr t
               deriving (Show, Data, Typeable, Foldable)

data KeyValue t = KeyValue t t (Expr t)
                | KeyValueErr t
                  deriving (Show, Data, Typeable, Foldable)


instance IsTree Statement where
    subtrees (FunDecl _ _ _ x) = fromBlock x
    subtrees (While _ _ x) = fromBlock x
    subtrees (DoWhile _ x _ _ _) = fromBlock x
    subtrees (For _ _ _ _ _ x) = fromBlock x
    subtrees (If _ _ x mb) = fromBlock x <> maybe [] subtrees mb
    subtrees (Else _ x) = fromBlock x
    subtrees (With _ _ x) = fromBlock x
    subtrees _ = []

instance Failable ForContent where
    stupid = ForErr
    hasFailed t = case t of
                    ForErr _ -> True
                    _        -> False

instance Failable Block where
    stupid = BlockErr
    hasFailed t = case t of
                    BlockErr _ -> True
                    _          -> False

instance Failable VarDecAss where
    stupid = AssErr
    hasFailed t = case t of
                    AssErr _ -> True
                    _        -> False

instance Failable Parameters where
    stupid = ParErr
    hasFailed t = case t of
                    ParErr _ -> True
                    _        -> False

instance Failable ParExpr where
    stupid = ParExprErr
    hasFailed t = case t of
                    ParExprErr _ -> True
                    _            -> False

instance Failable Expr where
    stupid = ExprErr
    hasFailed t = case t of
                    ExprErr _ -> True
                    _         -> False

instance Failable KeyValue where
    stupid = KeyValueErr
    hasFailed t = case t of
                    KeyValueErr _ -> True
                    _             -> False


-- | TODO: This code is *screaming* for some generic programming.
--
--   TODO: Somehow fix Failable and failStroker to be more "generic".  This will
--   make these instances much nicer and we won't have to make ad-hoc stuff like
--   this.
instance Strokable (Statement TT) where
    toStrokes (FunDecl f n ps blk) =
        let s = if hasFailed blk then error else failStroker [n] in
        s f <> s n <> toStrokes ps <> toStrokes blk
    toStrokes (VarDecl v vs sc) =
        let s = if any hasFailed vs then error else normal in
        s v <> foldMap toStrokes vs <> maybe mempty s sc
    toStrokes (Return t exp sc) = normal t <> maybe mempty toStrokes exp <> maybe mempty normal sc
    toStrokes (While w exp blk) =
        let s = if hasFailed blk || hasFailed blk then error else normal in
        s w <> toStrokes exp <> toStrokes blk
    toStrokes (DoWhile d blk w exp sc) =
        let s1 = if hasFailed blk then error else normal
            s2 = if hasFailed exp then error else normal in
        s1 d <> toStrokes blk <> s2 w <> toStrokes exp <> maybe mempty normal sc
    toStrokes (For f l x c r blk) =
        let s = if hasFailed blk || hasFailed c || hasFailed x
                  then error
                  else failStroker [f, l, r] in
        s f <> s l <> toStrokes x <> toStrokes c <> s r <> toStrokes blk
    toStrokes (If i x blk e) =
        let s = if hasFailed blk then error else normal in
        s i <> toStrokes x <> toStrokes blk <> maybe mempty toStrokes e
    toStrokes (Else e blk) = normal e <> toStrokes blk
    toStrokes (With w x blk) = normal w <> toStrokes x <> toStrokes blk
    toStrokes (Expr exp sc) = toStrokes exp <> maybe mempty normal sc
    toStrokes (Comm t) = normal t

instance Strokable (ForContent TT) where
    toStrokes (ForNormal s1 x2 s2 x3) =
        let s = if any hasFailed [x2, x3] then error else failStroker [s2] in
        s s1 <> toStrokes x2 <> s s2 <> toStrokes x3
    toStrokes (ForIn i x) =
        let s = if hasFailed x then error else normal in
        s i <> toStrokes x
    toStrokes (ForErr t) = error t

instance Strokable (Block TT) where
    toStrokes (BlockOne stmt) = toStrokes stmt
    toStrokes (Block l stmts r) =
        let s = failStroker [l, r] in
        s l <> foldMap toStrokes stmts <> s r
    toStrokes (BlockErr t) = error t

instance Strokable (VarDecAss TT) where
    toStrokes (AssBeg t x) = normal t <> maybe mempty toStrokes x
    toStrokes (AssRst t exp) =
        let s = if hasFailed exp then error else normal in
        s t <> toStrokes exp
    toStrokes (AssErr t) = error t

instance Strokable (Expr TT) where
    toStrokes (ExprSimple x exp) = normal x <> maybe mempty toStrokes exp
    toStrokes (ExprObj l kvs r) =
        let s = failStroker [l, r] in
        s l <> foldMap toStrokes kvs <> s r
    toStrokes (ExprPrefix t exp) = normal t <> toStrokes exp
    toStrokes (ExprNew t x) = normal t <> toStrokes x
    toStrokes (ExprParen l exp r op) =
        let s = failStroker [l, r] in
        s l <> toStrokes exp <> s r <> maybe mempty toStrokes op
    toStrokes (ExprAnonFun f ps blk) =
        normal f <> toStrokes ps <> toStrokes blk
    toStrokes (ExprTypeOf t x) =
        let s = if hasFailed x then error else normal in
        s t <> toStrokes x
    toStrokes (ExprFunCall n x m) =
        let s = if hasFailed x then error else normal in
        s n <> toStrokes x <> maybe mempty toStrokes m
    toStrokes (OpExpr op exp) =
        let s = if hasFailed exp then error else normal in
        s op <> toStrokes exp
    toStrokes (PostExpr t) = normal t
    toStrokes (ExprCond a x b y) =
        let s = failStroker [a, b] in
        s a <> toStrokes x <> s b <> toStrokes y
    toStrokes (ExprArr l x r m) =
        let s = failStroker [l, r] in
        s l <> maybe mempty toStrokes x <> s r <> maybe mempty toStrokes m
    toStrokes (ExprErr t) = error t

instance Strokable (Parameters TT) where
    toStrokes (Parameters l ps r) = normal l <> foldMap toStrokes ps <> normal r
    toStrokes (ParErr t) = error t

instance Strokable (ParExpr TT) where
    toStrokes (ParExpr l xs r) =
        let s = if isError r || any hasFailed xs
                  then error
                  else normal in
        s l <> foldMap toStrokes xs <> s r
    toStrokes (ParExprErr t) = error t

instance Strokable (KeyValue TT) where
    toStrokes (KeyValue n c exp) =
        let s = failStroker [n, c] in
        s n <> s c <> toStrokes exp
    toStrokes (KeyValueErr t) = error t

instance Strokable (Tok Token) where
    toStrokes t = if isError t
                      then one (modStroke errorStyle . tokenToStroke) t
                      else one tokenToStroke t

instance Strokable (Array TT) where
    toStrokes (ArrCont x m) = toStrokes x <> maybe mempty toStrokes m
    toStrokes (ArrRest c a m) = normal c <> toStrokes a <> maybe mempty toStrokes m
    toStrokes (ArrErr t) = error t


-- * Helper functions.

-- | Normal stroker.
normal :: TT -> Endo [Stroke]
normal = one tokenToStroke

-- | Error stroker.
error :: TT -> Endo [Stroke]
error = one (modStroke errorStyle . tokenToStroke)

one :: (t -> a) -> t -> Endo [a]
one f x = Endo (f x :)

-- | Given a new style and a stroke, return a stroke with the new style appended
--   to the old one.
modStroke :: StyleName -> Stroke -> Stroke
modStroke style = fmap (style <>)


-- * Stroking functions

-- | Given a list of tokens to check for errors (@xs@) and a list of tokens to
--   stroke (@xs'@), returns normal strokes for @xs'@ if there were no errors.
--   Otherwise returns error strokes for @xs'@.
nError :: [TT] -> [TT] -> Endo [Stroke]
nError xs = foldMap (failStroker xs)

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
getStrokes :: Tree TT -> Point -> Point -> Point -> [Stroke]
getStrokes t0 _point _begin _end = trace ('\n' `T.cons` showT t0) result
    where
      result = appEndo (foldMap toStrokes t0) []


-- * The parser

-- | Main parser.
parse :: P TT (Tree TT)
parse = many statement <* eof

-- | Parser for statements such as "return", "while", "do-while", "for", etc.
statement :: P TT (Statement TT)
statement = FunDecl <$> res Function' <*> plzTok name <*> parameters <*> block
        <|> VarDecl <$> res Var' <*> plz varDecAss `sepBy1` spc ',' <*> semicolon
        <|> Return  <$> res Return' <*> optional expression <*> semicolon
        <|> While   <$> res While' <*> parExpr <*> block
        <|> DoWhile <$> res Do' <*> block <*> plzTok (res While') <*> parExpr <*> semicolon
        <|> For     <$> res For' <*> plzSpc '(' <*> plzExpr <*> forContent
                    <*> plzSpc ')' <*> block
        <|> If      <$> res If' <*> parExpr <*> block <*> optional (Else <$> res Else' <*> block)
        <|> With    <$> res With' <*> parExpr <*> block
        <|> Comm    <$> comment
        <|> Expr    <$> stmtExpr <*> semicolon
    where
      forContent :: P TT (ForContent TT)
      forContent = ForNormal <$> spc ';' <*> plzExpr <*> plzSpc ';' <*> plzExpr
               <|> ForIn     <$> res In' <*> plzExpr
               <|> ForErr    <$> hate 1 (symbol (const True))
               <|> ForErr    <$> hate 2 (pure errorToken)
      varDecAss :: P TT (VarDecAss TT)
      varDecAss = AssBeg <$> name <*> optional (AssRst <$> oper Assign' <*> plzExpr)

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
    <|> BlockOne <$> hate 1 statement
    <|> BlockErr <$> hate 2 (pure errorToken)

-- | Parser for expressions which may be statements.  In reality, any expression
--   is also a valid statement, but this is a slight compromise to get rid of
--   the massive performance loss which is introduced when allowing JavaScript
--   objects to be valid statements.
stmtExpr :: P TT (Expr TT)
stmtExpr = ExprSimple <$> simpleTok <*> optional opExpr
       <|> ExprPrefix <$> preOp <*> plzExpr
       <|> ExprNew    <$> res New' <*> plz funCall
       <|> funCall
       -- We hate the parenthesized expression just a tad because otherwise
       -- confirm('hello') will be seen as "confirm; ('hello');"
       <|> hate 1 (ExprParen  <$> spc '(' <*> plzExpr <*> plzSpc ')'
                              <*> optional opExpr)
       <|> ExprErr <$> hate 2 (symbol (const True))
    where
      funCall :: P TT (Expr TT)
      funCall = ExprFunCall <$> name <*> parExpr <*> optional opExpr

-- | The basic idea here is to parse "the rest" of expressions, e.g. @+ 3@ in @x
--   + 3@ or @[i]@ in @x[i]@.  Anything which is useful in such a scenario goes
--   here.  TODO: This accepts [], but shouldn't, since x[] is invalid.
opExpr :: P TT (Expr TT)
opExpr = OpExpr   <$> inOp <*> plzExpr
     <|> ExprCond <$> spc '?' <*> plzExpr <*> plzSpc ':' <*> plzExpr
     <|> PostExpr <$> postOp
     <|> array

-- | Parser for expressions.
expression :: P TT (Expr TT)
expression = ExprObj     <$> spc '{' <*> keyValue `sepBy` spc ',' <*> plzSpc '}'
         <|> ExprAnonFun <$> res Function' <*> parameters <*> block
         <|> ExprTypeOf  <$> res TypeOf' <*> plzExpr
         <|> stmtExpr
         <|> array
    where
      keyValue :: P TT (KeyValue TT)
      keyValue = KeyValue    <$> name <*> plzSpc ':' <*> plzExpr
             <|> KeyValueErr <$> hate 1 (symbol (const True))
             <|> KeyValueErr <$> hate 2 (pure errorToken)

-- | Parses both empty and non-empty arrays.  Should probably be split up into
--   further parts to allow for the separation of @[]@ and @[1, 2, 3]@.
array :: P TT (Expr TT)
array = ExprArr <$> spc '[' <*> optional arrayContents <*> plzSpc ']'
                <*> optional opExpr
    where
      arrayContents :: P TT (Array TT)
      arrayContents = ArrCont <$> expression <*> optional arrRest
      arrRest :: P TT (Array TT)
      arrRest = ArrRest <$> spc ',' <*> (arrayContents
                                     <|> ArrErr <$> hate 1 (symbol (const True))
                                     <|> ArrErr <$> hate 2 (pure errorToken))
                                    <*> optional arrRest


-- * Parsing helpers

-- | Parses a semicolon if it's there.
semicolon :: P TT (Maybe TT)
semicolon = optional $ spc ';'

-- | Parses a comma-separated list of valid identifiers.
parameters :: P TT (Parameters TT)
parameters = Parameters <$> spc '(' <*> plzTok name `sepBy` spc ',' <*> plzSpc ')'
         <|> ParErr <$> hate 1 (symbol (const True))
         <|> ParErr <$> hate 2 (pure errorToken)

parExpr :: P TT (ParExpr TT)
parExpr = ParExpr <$> spc '(' <*> plzExpr `sepBy` spc ',' <*> plzSpc ')'
      <|> ParExprErr <$> hate 1 (symbol (const True))
      <|> ParExprErr <$> hate 2 (pure errorToken)


-- * Simple parsers

-- | Parses a comment.
comment :: P TT TT
comment = symbol (\t -> case fromTT t of
                          Comment _ -> True
                          _         -> False)

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
            Const _     -> True
            Rex _       -> True
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
res :: Reserved -> P TT TT
res x = symbol (\t -> case fromTT t of
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
hate n = power n recoverWith
    where
      power 0 _ = id
      power m f = f . power (m - 1) f


-- * Utility stuff

fromBlock :: Block t -> [Statement t]
fromBlock (Block _ x _) = toList x
fromBlock (BlockOne x) = [x]
fromBlock (BlockErr _) = []

firstTok :: Foldable f => f t -> t
firstTok x = head (toList x)

errorToken :: TT
errorToken = toTT $ Special '!'

isError :: TT -> Bool
isError (Tok (Special '!') _ _) = True
isError _ = False

-- | Better name for 'tokFromT'.
toTT :: t -> Tok t
toTT = tokFromT

-- | Better name for 'tokT'.
fromTT :: Tok t -> t
fromTT = tokT
