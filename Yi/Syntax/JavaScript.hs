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
-- problem nicely...

data JTree t = JFunDecl { fdres :: t, fdname :: t
                        , fdlpar :: t, fdpars :: [t], fdrpar :: t
                        , fdbody :: (JFunBody t) }
             | JVarDecl { vdres :: t, vdvars :: [JVarDecAss t], vdsc :: t }
             | JFunCall { fcname :: t, fclpar :: t, fcargs :: [JExpr t], fcrpar :: t, fcsc :: t }
             | JStatement (Statement t)
             | JError t
               deriving (Eq, Show)

data Statement t = StmtReturn { returnres :: t, returnexpr :: (JExpr t), returnsc :: t }
                 | StmtWhile { whileres :: t, whilelpar :: t, whileexpr :: (JExpr t), whilerpar :: t
                             , whilebody :: (JFunBody t) }
                 | StmtDoWhile { dwdo :: t, dwbody :: (JFunBody t), dwwhile :: t
                               , dwlcurl :: t, dwexpr :: (JExpr t), dwrcurl :: t, dwsc :: t }
                   deriving (Eq, Show)

-- | @JOldFunBody@ represents the "standard" function body, using the curly
--   brackets wrapped around multiple statements.  @JNewFunBody@ represents the
--   JS 1.8 "lambda style" function body, containing only a simple expression.
data JFunBody t = JOldFunBody { fblcurl :: t, fbbody :: [JTree t], fbrcurl :: t }
                | JNewFunBody { expr :: (JExpr t) }
                | JErrFunBody t
                  deriving (Eq, Show)

-- | Represents either a variable name or a variable name assigned to an
--   expression.  @AssignNo@ means it's a simple declaration.  @AssignYes@ means
--   a declaration and an assignment.  @AssignErr@ is used as a recovery.
data JVarDecAss t = AssignNo  t             -- ^ No, no assignment
                  | AssignYes t t (JExpr t) -- ^ Yes, an assignment
                  | AssignErr t             -- ^ What?!
                    deriving (Eq, Show)

data JExpr t = JObj { objlcurl :: t, objrcurl :: t }
             | JStr t
             | JNum t
             | JName t
             | JAnonFun { afres :: t, aflpar :: t, afpars :: [t], afrpar :: t, afbody :: (JFunBody t) }
             | ExprErr t
               deriving (Eq, Show)

data JKeyValue t = JKV { key :: t, colon :: t, value :: (JExpr t) }
                 | JKVErr t
                   deriving (Eq, Show)

$(derive makeFoldable ''JTree)
$(derive makeFoldable ''Statement)
$(derive makeFoldable ''JExpr)
$(derive makeFoldable ''JKeyValue)
$(derive makeFoldable ''JVarDecAss)
$(derive makeFoldable ''JFunBody)

instance Strokable (JVarDecAss TT) where
    toStrokes (AssignNo x) = oneStroke x
    toStrokes (AssignYes x y z) = oneStroke x
                               <> oneStroke y
                               <> toStrokes z
    toStrokes (AssignErr x) = one (modStroke errorStyle . tokenToStroke) x

instance Strokable (JFunBody TT) where
    toStrokes (JOldFunBody l s r) = oneStroke l
                                 <> foldMap toStrokes s
                                 <> oneStroke r
    toStrokes (JNewFunBody x) = toStrokes x
    toStrokes (JErrFunBody x) = one (modStroke errorStyle . tokenToStroke) x

instance Strokable (JTree TT) where
    toStrokes f@(JFunDecl {}) = oneStroke (fdres f)
                             <> oneStroke (fdlpar f)
                             <> few tokenToStroke (fdpars f)
                             <> oneStroke (fdrpar f)
                             <> toStrokes (fdbody f)
    toStrokes v@(JVarDecl {}) = oneStroke (vdres v)
                             <> foldMap toStrokes (vdvars v)
                             <> oneStroke (vdsc v)
    toStrokes f@(JFunCall {}) = oneStroke (fcname f)
                             <> oneStroke (fclpar f)
                             <> foldMap toStrokes (fcargs f)
                             <> oneStroke (fcrpar f)
                             <> oneStroke (fcsc f)
    toStrokes (JStatement x) = toStrokes x
    toStrokes (JError x) = one (modStroke errorStyle . tokenToStroke) x

instance Strokable (JExpr TT) where
    toStrokes o@(JObj {})     = oneStroke (objlcurl o)
    toStrokes   (JNum x)      = oneStroke x
    toStrokes   (JName x)     = oneStroke x
    toStrokes   (JStr x)      = oneStroke x
    toStrokes f@(JAnonFun {}) = oneStroke (afres f)
                             <> oneStroke (aflpar f)
                             <> few tokenToStroke (afpars f)
                             <> oneStroke (afrpar f)
                             <> toStrokes (afbody f)
    toStrokes   (ExprErr x)   = one (modStroke errorStyle . tokenToStroke) x

instance Strokable (Statement TT) where
    toStrokes s@(StmtReturn {}) = oneStroke (returnres s)
                               <> toStrokes (returnexpr s)
                               <> oneStroke (returnsc s)
    toStrokes s@(StmtWhile {})  = oneStroke (whileres s)
                               <> oneStroke (whilelpar s)
                               <> toStrokes (whileexpr s)
                               <> oneStroke (whilerpar s)
                               <> toStrokes (whilebody s)
    toStrokes s@(StmtDoWhile {}) = oneStroke (dwdo s)
                                <> toStrokes (dwbody s)
                                <> oneStroke (dwwhile s)
                                <> oneStroke (dwlcurl s)
                                <> toStrokes (dwexpr s)
                                <> oneStroke (dwrcurl s)
                                <> oneStroke (dwsc s)

-- | StmtDoWhile { dwdo :: t, dwbody :: (JFunBody t), dwwhile :: t
--               , dwlcurl :: t, dwexpr :: (JExpr t), dwrcurl :: t, dwsc :: t }

instance Strokable (JKeyValue TT) where
    toStrokes = foldMap (oneStroke) -- TODO: What?!


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

parse :: P TT [JTree TT]
parse = pForest <* eof

pForest :: P TT [JTree TT]
pForest = many pTree

pTree :: P TT (JTree TT)
pTree = anyLevel
    <|> (JError <$> recoverWith (symbol (const True)))


-- * High-level stuff

-- We don't have anything that's only allowed at the top level yet...  And
-- yes, apparently function declarations inside of function declarations
-- are okay.  TODO: Anonymous functions are syntactically valid to have at
-- the top level, but don't make any sense on their own.
--
-- topLevel :: P TT (JTree TT)
-- topLevel = undefined

-- | Parser for stuff that's allowed anywhere in the program.
anyLevel :: P TT (JTree TT)
anyLevel = funDecl <|> varDecl <|> funCall
       <|> JStatement <$> statements

-- | Parser for statements.
statements = StmtReturn  <$> resWord Return' <*> expression <*> plzSpc ';'
         <|> StmtWhile   <$> resWord While' <*> plzSpc '(' <*> expression <*> plzSpc ')' <*> funBody
         <|> StmtDoWhile <$> resWord Do' <*> funBody <*> plzTok (resWord While')
                         <*> plzSpc '(' <*> expression <*> plzSpc ')' <*> plzSpc ';'

-- | Parser for function declarations.
funDecl :: P TT (JTree TT)
funDecl = JFunDecl <$> resWord Function' <*> plzTok name
                   <*> plzSpc '(' <*> parameters <*> plzSpc ')'
                   <*> funBody

-- | Parser for old-style function bodies and "lambda style" ones.
funBody :: P TT (JFunBody TT)
funBody = JOldFunBody <$> plzSpc '{' <*> many anyLevel <*> plzSpc '}'
      <|> JNewFunBody <$> expression
      <|> JErrFunBody <$> pure unknownToken

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

-- | Parser for function calls.
funCall :: P TT (JTree TT)
funCall = JFunCall <$> name <*> plzSpc '(' <*> arguments <*> plzSpc ')' <*> plzSpc ';'


-- | Parser for expressions.
expression :: P TT (JExpr TT)
expression = JStr     <$> strTok
         <|> JNum     <$> numTok
         <|> JObj     <$> spec '{' <*> plzSpc '}' -- TODO
         <|> JName    <$> name
         <|> JAnonFun <$> resWord Function' <*> plzSpc '(' <*> parameters <*> plzSpc ')' <*> funBody
         <|> ExprErr  <$> recoverWith (pure unknownToken)


-- * Helper parsers

-- | Like 'sepBy1', but with recovery.
pleaseSepBy1 p s r  = (:) <$> p <*> (many (s *> p) <|> recoverWith r)

-- | Parser for comma-separated identifiers.
parameters = commas (plzTok name)

-- | Parser for comma-separated expressions.
arguments :: P TT [JExpr TT]
arguments = commas expression

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

resWord x = symbol (\t -> case fromTT t of
                            Res y -> x == y
                            _     -> False)

spec x = symbol (\t -> case fromTT t of
                         Special y -> x == y
                         _         -> False)

oper x = symbol (\t -> case fromTT t of
                         Op y -> y == x
                         _    -> False)


-- * Recovery stuff

-- | Recover operator.  Prefers RHS.
(<>>) :: P s a -> P s a -> P s a
x <>> y = recoverWith x <|> y

-- | Recovery operator.  Prefers LHS.
(<<>) :: P s a -> P s a -> P s a
x <<> y = y <>> x

-- | Expects a token x, recovers with 'unknownToken'.
plzTok x = x <<> (pure unknownToken)

-- | Expects a special token, recovers with 'unknownToken'.
plzSpc = plzTok . spec

-- | Constant 'Unknown' wrapped in a 'Tok'.
unknownToken :: TT
unknownToken = toTT Unknown


-- * Utility stuff

-- | Better name for 'tokFromT'.
toTT = tokFromT

-- | Better name for 'fromTT'.
fromTT = tokT
