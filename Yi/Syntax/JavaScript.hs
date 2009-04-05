{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- (C) Copyright 2009 Deniz Dogan

module Yi.Syntax.JavaScript where

import Data.DeriveTH
import Data.Derive.Foldable
import Data.Monoid (Endo(..))
import Prelude ()
import Yi.Buffer.Basic (Point(..))
import Yi.IncrementalParse (P, eof, symbol, recoverWith)
import Yi.Lexer.Alex
import Yi.Lexer.JavaScript (TT, Token(..), Reserved(..), Operator(..), tokenToStyle)
import Yi.Prelude
import Yi.Style (errorStyle, StyleName)
import Yi.Syntax.Tree (sepBy)


-- * Data types, classes and instances

-- | Instances of @Strokable@ are datatypes which can be syntax highlighted.
class Strokable a where
    toStrokes :: a -> Endo [Stroke]

-- Unfortunately the selector functions for many of these datatypes have been
-- prefixed with two characters to differentiate them from selector functions of
-- other datatypes.  E.g. you can't have "body" as a selector function for
-- different datatypes, so I call them fdbody, fbbody and afbody for function
-- declarations, function bodies and anonymous function bodies respectively.  I
-- haven't been able to find any extension which automagically solves this
-- problem nicely...  But on the other hand, maybe there's no point in having
-- these in record syntax, since we never use the selector functions anyways.

data JTree t = JFunDecl { fdres :: t, fdname :: t
                        , fdlpar :: t, fdpars :: [t], fdrpar :: t
                        , fdbody :: (JFunBody t) }
             | JVarDecl { vdres :: t, vdvars :: [JVarDecAss t], vdsc :: t }
             | JStatement (Statement t)
             | JError t
               deriving (Eq, Show)

data Statement t = StmtReturn { returnres :: t, returnexpr :: (JExpr t), returnsc :: t }
                 | StmtWhile { whileres :: t, whilelpar :: t, whileexpr :: (JExpr t), whilerpar :: t
                             , whilebody :: (JFunBody t) }
                 | StmtDoWhile { dwdo :: t, dwbody :: (JFunBody t), dwwhile :: t
                               , dwlcurl :: t, dwexpr :: (JExpr t), dwrcurl :: t, dwsc :: t }
                 | StmtFor { forres :: t, forlpar :: t, forinit :: (JExpr t), forsc1 :: t
                           , forcond :: (JExpr t), forsc2 :: t, forstep :: (JExpr t), forrpar :: t
                           , forbody :: (JFunBody t) }
                   deriving (Eq, Show)

-- | @FunBodyOld@ represents the "standard" function body, using the curly
--   brackets wrapped around multiple statements.  @FunBodyNew@ represents the
--   JS 1.8 "lambda style" function body, containing only a simple expression.
data JFunBody t = FunBodyOld { fblcurl :: t, fbbody :: [JTree t], fbrcurl :: t }
                | FunBodyNew { expr :: (JExpr t) }
                | FunBodyErr t
                  deriving (Eq, Show)

-- | Represents either a variable name or a variable name assigned to an
--   expression.  @AssignNo@ means it's a simple declaration.  @AssignYes@ means
--   a declaration and an assignment.  @AssignErr@ is used as a recovery.
data JVarDecAss t = AssignNo  t             -- ^ No, no assignment
                  | AssignYes t t (JExpr t) -- ^ Yes, an assignment
                  | AssignErr t             -- ^ What?!
                    deriving (Eq, Show)

data JExpr t = ExprObj { objlcurl :: t, objkv :: [JKeyValue t], objrcurl :: t }
             | ExprStr t
             | ExprNum t
             | ExprName t
             | ExprAnonFun { afres :: t, aflpar :: t, afpars :: [t], afrpar :: t, afbody :: (JFunBody t) }
             | ExprFunCall { fcname :: t, fclpar :: t, fcargs :: [JExpr t], fcrpar :: t, fcsc :: t }
             | ExprErr t
               deriving (Eq, Show)

data JKeyValue t = JKeyValue { key :: t, colon :: t, value :: (JExpr t) }
                   deriving (Eq, Show)

$(derive makeFoldable ''JTree)
$(derive makeFoldable ''Statement)
$(derive makeFoldable ''JExpr)
$(derive makeFoldable ''JKeyValue)
$(derive makeFoldable ''JVarDecAss)
$(derive makeFoldable ''JFunBody)

-- TODO: (Optimization) Only make strokes for stuff that don't have
-- defaultStyle.  I'm not entirely sure how much this would help performance,
-- but it should be kept in mind for the future...

instance Strokable (Tok Token) where
    toStrokes = one tokenToStroke

instance Strokable (JTree TT) where
    toStrokes = foldMap toStrokes

instance Strokable (Statement TT) where
    toStrokes = foldMap toStrokes

instance Strokable (JFunBody TT) where
    toStrokes = foldMap toStrokes

instance Strokable (JVarDecAss TT) where
    toStrokes = foldMap toStrokes

instance Strokable (JExpr TT) where
    toStrokes = foldMap toStrokes

instance Strokable (JKeyValue TT) where
    toStrokes = foldMap toStrokes


-- * Helper functions.

one :: (t -> a) -> t -> Endo [a]
one f x = Endo (f x :)

few :: (Foldable t) => (a -> b) -> t a -> Endo [b]
few f xs = foldMap (one f) xs

modStroke :: StyleName -> Stroke -> Stroke
modStroke style stroke = fmap (style <>) stroke

oneStroke :: TT -> Endo [Stroke]
oneStroke = one tokenToStroke


-- * Stroking functions

tokenToStroke :: TT -> Stroke
tokenToStroke tt = let stroker = fmap tokenToStyle . tokToSpan in
                   case fromTT tt of
                     Unknown -> (modStroke errorStyle . stroker) tt
                     _       -> stroker tt

getStrokes :: Point -> Point -> Point -> [JTree TT] -> [Stroke]
getStrokes _point _begin _end t0 = trace (show t0) result
    where
      result = appEndo (foldMap toStrokes t0) []


-- * The parser

-- | Main parser.
parse :: P TT [JTree TT]
parse = many pTree <* eof

-- | Parser for a single tree.
pTree :: P TT (JTree TT)
pTree = anyLevel
    <|> (JError <$> recoverWith (symbol (const True)))

-- | Parser for stuff that's allowed anywhere in the program.
anyLevel :: P TT (JTree TT)
anyLevel = funDecl
       <|> varDecl
       <|> JStatement <$> statements

-- | Parser for statements such as "return", "while", "do-while", "for", etc.
statements :: P TT (Statement TT)
statements = StmtReturn  <$> resWord Return' <*> expression <*> plzSpc ';'
         <|> StmtWhile   <$> resWord While' <*> plzSpc '(' <*> expression <*> plzSpc ')' <*> funBody
         <|> StmtDoWhile <$> resWord Do' <*> funBody <*> plzTok (resWord While')
                         <*> plzSpc '(' <*> expression <*> plzSpc ')' <*> plzSpc ';'
         <|> StmtFor     <$> resWord For' <*> plzSpc '(' <*> expression <*> plzSpc ';'
                         <*> expression <*> plzSpc ';' <*> expression <*> plzSpc ')' <*> funBody

-- | Parser for function declarations.
funDecl :: P TT (JTree TT)
funDecl = JFunDecl <$> resWord Function' <*> plzTok name
                   <*> plzSpc '(' <*> parameters <*> plzSpc ')'
                   <*> funBody

-- | Parser for old-style function bodies and "lambda style" ones.
funBody :: P TT (JFunBody TT)
funBody = FunBodyOld <$> plzSpc '{' <*> many anyLevel <*> plzSpc '}'
      <|> FunBodyNew <$> expression
      <|> FunBodyErr <$> pure unknownToken

-- | Parser for variable declarations.
varDecl :: P TT (JTree TT)
varDecl = JVarDecl <$> resWord Var'
                   <*> pleaseSepBy1 varDecAss (spec ',') (pure []) -- TODO: Ugly recovery?
                   <*> plzSpc ';'
    where
      varDecAss :: P TT (JVarDecAss TT)
      varDecAss = AssignNo  <$> name
              <|> AssignYes <$> name <*> plzTok (oper Assign') <*> expression
              <|> AssignErr <$> pure unknownToken

-- | Parser for expressions.
expression :: P TT (JExpr TT)
expression = ExprStr     <$> strTok
         <|> ExprNum     <$> numTok
         <|> ExprObj     <$> spec '{' <*> keyValue `sepBy` spec ',' <*> plzSpc '}' -- TODO
         <|> ExprName    <$> name
         <|> ExprAnonFun <$> resWord Function' <*> plzSpc '(' <*> parameters <*> plzSpc ')' <*> funBody
         <|> ExprFunCall <$> name <*> plzSpc '(' <*> arguments <*> plzSpc ')' <*> plzSpc ';'
         <|> ExprErr     <$> recoverWith (pure unknownToken)
    where
      keyValue = JKeyValue <$> name <*> plzSpc ':' <*> expression


-- * Parsing helpers

-- | Like 'sepBy1', but with recovery.
pleaseSepBy1 :: P s a -> P s b -> P s [a] -> P s [a]
pleaseSepBy1 p s r  = (:) <$> p <*> (many (s *> p) <|> recoverWith r)

-- | Parser for comma-separated identifiers.
parameters :: P TT [TT]
parameters = commas (plzTok name)

-- | Parser for comma-separated expressions.
arguments :: P TT [JExpr TT]
arguments = commas expression

commas :: P TT a -> P TT [a]
commas x = x `sepBy` (spec ',')

-- TODO: Why do these give type-errors even when they're not used?
-- parens  x = plzSpc '(' <*> x <*> plzSpc ')'
-- curlies x = plzSpc '{' <*> x <*> plzSpc '}'


-- * Simple parsers

strTok :: P TT TT
strTok = symbol (\t -> case fromTT t of
                         Str _ -> True
                         _     -> False)

numTok :: P TT TT
numTok = symbol (\t -> case fromTT t of
                         Number _ -> True
                         _        -> False)

name :: P TT TT
name = symbol (\t -> case fromTT t of
                       ValidName _ -> True
                       _           -> False)

resWord :: Reserved -> P TT TT
resWord x = symbol (\t -> case fromTT t of
                            Res y -> x == y
                            _     -> False)

spec :: Char -> P TT TT
spec x = symbol (\t -> case fromTT t of
                         Special y -> x == y
                         _         -> False)

oper :: Operator -> P TT TT
oper x = symbol (\t -> case fromTT t of
                         Op y -> y == x
                         _    -> False)


-- * Recovery parsers

-- | Recover operator.  Prefers RHS.
(<>>) :: P s a -> P s a -> P s a
x <>> y = recoverWith x <|> y

-- | Recovery operator.  Prefers LHS.
(<<>) :: P s a -> P s a -> P s a
x <<> y = y <>> x

-- | Expects a token x, recovers with 'unknownToken'.
plzTok :: P s TT -> P s TT
plzTok x = x <<> (pure unknownToken)

-- | Expects a special token, recovers with 'unknownToken'.
plzSpc :: Char -> P TT TT
plzSpc = plzTok . spec


-- * Utility stuff

-- | Constant 'Unknown' wrapped in a 'Tok'.
unknownToken :: TT
unknownToken = toTT Unknown

-- | Better name for 'tokFromT'.
toTT :: t -> Tok t
toTT = tokFromT

-- | Better name for 'fromTT'.
fromTT :: Tok t -> t
fromTT = tokT
