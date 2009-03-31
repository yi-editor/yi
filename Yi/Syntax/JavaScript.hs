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

data JTree t = JFunDecl { res :: t, fname :: t
                        , lpar :: t, pars :: [t], rpar :: t
                        , fbody :: (JFunBody t) }
             | JVarDecl { res :: t, vars :: [JVarDecAss t], sc :: t }
             | JFunCall { fname :: t, lpar :: t, args :: [JExpr t], rpar :: t, sc :: t }
             | JError t
               deriving (Eq, Show)

-- | @JOldFunBody@ represents the "standard" function body, using the curly
--   brackets wrapped around multiple statements.  @JNewFunBody@ represents the
--   JS 1.8 "lambda style" function body, containing only a simple expression.
data JFunBody t = JOldFunBody { lcurl :: t, body :: [JTree t], rcurl :: t }
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
             | ExprErr t
               deriving (Eq, Show)

data JKeyValue t = JKV { key :: t, colon :: t, value :: (JExpr t) }
                 | JKVErr t
                   deriving (Eq, Show)

$(derive makeFoldable ''JTree)
$(derive makeFoldable ''JExpr)
$(derive makeFoldable ''JKeyValue)
$(derive makeFoldable ''JVarDecAss)
$(derive makeFoldable ''JFunBody)

instance Strokable (JVarDecAss TT) where
    toStrokes (AssignNo x) = one (tokenToStroke x)
    toStrokes (AssignYes x y z) = one (tokenToStroke x)
                               <> one (tokenToStroke y)
                               <> toStrokes z
    toStrokes (AssignErr x) = one (modStroke errorStyle (tokenToStroke x))

instance Strokable (JFunBody TT) where
    toStrokes (JOldFunBody l s r) = one (tokenToStroke l)
                                 <> foldMap toStrokes s
                                 <> one (tokenToStroke r)
    toStrokes (JNewFunBody x) = toStrokes x
    toStrokes (JErrFunBody x) = one (modStroke errorStyle (tokenToStroke x))

instance Strokable (JTree TT) where
    toStrokes f@(JFunDecl {}) = one (tokenToStroke (res f))
                             <> one (tokenToStroke (lpar f))
                             <> few tokenToStroke (pars f)
                             <> one (tokenToStroke (rpar f))
                             <> toStrokes (fbody f)
    toStrokes v@(JVarDecl {}) = one (tokenToStroke (res v))
                             <> foldMap toStrokes (vars v)
                             <> one (tokenToStroke (sc v))
    toStrokes f@(JFunCall {}) = one (tokenToStroke (fname f))
                             <> one (tokenToStroke (lpar f))
                             <> foldMap toStrokes (args f)
                             <> one (tokenToStroke (rpar f))
                             <> one (tokenToStroke (sc f))
    toStrokes (JError x) = one (modStroke errorStyle (tokenToStroke x))

instance Strokable (JExpr TT) where
    toStrokes o@(JObj {})   = one (tokenToStroke (objlcurl o))
    toStrokes   (JNum x)    = one (tokenToStroke x)
    toStrokes   (JName x)   = one (tokenToStroke x)
    toStrokes   (JStr x)    = one (tokenToStroke x)
    toStrokes   (ExprErr x) = one (modStroke errorStyle (tokenToStroke x))

instance Strokable (JKeyValue TT) where
    toStrokes = foldMap (one . tokenToStroke)


-- * Helper functions.

one :: a -> Endo [a]
one x = Endo (x :)

few :: (Foldable t) => (a -> b) -> t a -> Endo [b]
few g = foldMap (one . g)

getStrokes :: Point -> Point -> Point -> [JTree TT] -> [Stroke]
getStrokes _point _begin _end t0 = trace (show t0) result
    where
      result = appEndo (foldMap toStrokes t0) []

modStroke :: StyleName -> Stroke -> Stroke
modStroke style stroke = fmap (style <>) stroke

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

parse :: P TT [JTree TT]
parse = pForest <* eof
    where

      pForest :: P TT [JTree TT]
      pForest = many pTree

      pTree :: P TT (JTree TT)
      pTree = topLevel
          <|> anyLevel
          <|> (JError <$> recoverWith (symbol (const True)))


      -- * High-level stuff

      -- | Parser for stuff that's allowed only at the top level of the program.
      topLevel :: P TT (JTree TT)
      topLevel = funDecl

      -- | Parser for stuff that's allowed anywhere in the program.
      anyLevel :: P TT (JTree TT)
      anyLevel = varDecl <|> funCall

      -- | Parser for function declarations.  TODO: Currently doesn't really
      --   support the JS 1.8 "lambda" function style.
      funDecl :: P TT (JTree TT)
      funDecl = JFunDecl <$> resWord Function' <*> plzTok name
                         <*> plzSpc '(' <*>  parameters   <*> plzSpc ')'
                         <*> funBody

      -- | Parser for old-style function bodies and "lambda style" ones.
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


      -- * Helper parsers

      -- | Like 'sepBy1', but with recovery.
      pleaseSepBy1 p s r  = (:) <$> p <*> (many (s *> p) <|> recoverWith r)

      -- | Parser for comma-separated identifiers.
      parameters :: P TT [TT]
      parameters = commas (plzTok name)

      -- | Parser for comma-separated expressions.
      arguments :: P TT [JExpr TT]
      arguments = commas expression

      -- | Parser for expressions.
      expression :: P TT (JExpr TT)
      expression = JStr    <$> strTok
               <|> JNum    <$> numTok
               <|> JObj    <$> spec '{' <*> plzSpc '}' -- TODO
               <|> JName   <$> name
               <|> ExprErr <$> recoverWith (pure unknownToken)


      -- * Simple parsers

      strTok = symbol (\t -> case tokT t of
                               Str _ -> True
                               _     -> False)

      numTok = symbol (\t -> case tokT t of
                               Number _ -> True
                               _        -> False)

      name = symbol (\t -> case tokT t of
                             ValidName _ -> True
                             _           -> False)

      resWord x = symbol (\t -> case tokT t of
                                  Res y -> x == y
                                  _     -> False)

      spec x = symbol (\t -> case tokT t of
                               Special y -> x == y
                               _         -> False)

      oper x = symbol (\t -> case tokT t of
                               Op y -> y == x
                               _    -> False)


      -- * Recovery stuff

      -- | Recover operator.  Prefers RHS.
      x <>> y = recoverWith x <|> y

      -- | Recovery operator.  Prefers LHS.
      x <<> y = y <>> x

      plzTok x = x <<> (pure unknownToken)
      plzSpc   = plzTok . spec
      commas x = x `sepBy` (spec ',')

      unknownToken = tokFromT Unknown

