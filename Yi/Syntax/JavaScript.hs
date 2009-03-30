{-# LANGUAGE FlexibleInstances #-}
-- (C) Copyright 2009 Deniz Dogan

module Yi.Syntax.JavaScript where

import Data.List (intersperse)
import Data.Monoid (Endo(..))
import Prelude (map)
import Yi.Buffer.Basic (Point(..))
import Yi.IncrementalParse (P, eof, symbol, recoverWith)
import Yi.Lexer.Alex
import Yi.Lexer.JavaScript (TT, Token(..), Reserved(..), Operator(..), tokenToStyle, Failable(..), Strokable(..))
import Yi.Prelude
import Yi.Style (errorStyle, StyleName)
import Yi.Syntax.Tree (sepBy, sepBy1)


-- * Data types, classes and instances

data JTree t = JFunDecl { res :: t, fname :: t
                        , lpar :: t, pars :: [t], rpar :: t
                        , lcurl :: t, fbody :: [JTree t], rcurl :: t }
             | JVarDecl { res :: t, vars :: [JVarDecAss t], sc :: t }
             | JFunCall { fname :: t, lpar :: t, args :: [JExpr t], rpar :: t, sc :: t }
             | JError t
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
             | ExprErr t
               deriving (Eq, Show)

data JKeyValue t = JKV { key :: t, colon :: t, value :: (JExpr t) }
                 | JKVErr t
                   deriving (Eq, Show)

instance Strokable (JVarDecAss TT) where
    toStrokes (AssignNo x)      = one (tokenToStroke x)
    toStrokes (AssignYes x y z) = few tokenToStroke [x, y]
                               <> toStrokes z
    toStrokes (AssignErr t)     = one (modStroke errorStyle (tokenToStroke t))

instance Strokable (JTree TT) where
    -- TODO: toStrokes for JFunDecl probably needs an appropriate instance of
    -- Foldable to work.

    -- TODO: Error styling combinator.
    toStrokes f@(JFunDecl {}) = one (tokenToStroke (res f) )
                             <> one (tokenToStroke (lpar f))
                             <> few tokenToStroke (pars f)
                             <> one (tokenToStroke (rpar f))
                             <> one (tokenToStroke (lcurl f))
                             <> foldMap toStrokes (fbody f) -- TODO
                             <> one (tokenToStroke (rcurl f))
    toStrokes v@(JVarDecl {}) = fewNotError tokenToStroke [res v, sc v]
                             <> foldMap toStrokes (vars v) -- TODO
    toStrokes f@(JFunCall {}) = few tokenToStroke [fname f, lpar f, rpar f, sc f]
                             <> foldMap toStrokes (args f) -- TODO
    toStrokes e@(JError t)    = one (modStroke errorStyle (tokenToStroke t))

instance Strokable (JExpr TT) where
    toStrokes o@(JObj {})   = one (tokenToStroke (objlcurl o))
    toStrokes   (JNum t)    = one (tokenToStroke t)
    toStrokes   (JStr t)    = one (tokenToStroke t)
    toStrokes   (ExprErr t) = one (modStroke errorStyle (tokenToStroke t))

instance Strokable (JKeyValue TT) where
    toStrokes    (JKVErr t) = one (modStroke errorStyle (tokenToStroke t))
    toStrokes kv@(JKV {})   = few tokenToStroke [key kv, colon kv]
                           <> toStrokes (value kv)

instance Failable (JTree TT) where
    hasFailed (JError _) = True
    hasFailed _          = False

instance Failable (JVarDecAss TT) where
    hasFailed (AssignErr _) = True
    hasFailed _             = False

instance Failable (JExpr TT) where
    hasFailed (ExprErr _) = True
    hasFailed _           = False


-- * Helper functions.

one x = Endo (x :)
few g = foldMap (one . g)

-- | Given a list of tokens, if any of the tokens is an error token, style every
--   token in error style, otherwise just tokenToStroke all of them.  TODO:
--   Figure out whether this is usable at all.
fewNotError f xs = few (stroker . f) xs
    where
      stroker = if any hasFailed xs
                  then modStroke errorStyle
                  else id

getStrokes :: Point -> Point -> Point -> [JTree TT] -> [Stroke]
getStrokes point begin end t0 = trace (show t0) result
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

      topLevel = funDecl
      anyLevel = varDecl <|> funCall

      funDecl :: P TT (JTree TT)
      funDecl = JFunDecl <$> resWord Function' <*> plzTok name
                         <*> plzSpc '(' <*>  parameters   <*> plzSpc ')'
                         <*> plzSpc '{' <*> many anyLevel <*> plzSpc '}'

      varDecl :: P TT (JTree TT)
      varDecl = JVarDecl <$> resWord Var'
                         <*> pleaseSepBy1 varDecAss (spec ',') (pure []) -- TODO: Ugly recovery? How about pure Error instead?
                         <*> plzSpc ';'

      varDecAss :: P TT (JVarDecAss TT)
      varDecAss = AssignNo  <$> name
              <|> AssignYes <$> name <*> plzTok (oper Assign') <*> (expression)
              <|> AssignErr <$> pure unknownToken

      funCall = JFunCall <$> name <*> plzSpc '(' <*> arguments <*> plzSpc ')' <*> plzSpc ';'


      -- * Helper parsers

      pleaseSepBy1 p s r  = (:) <$> p <*> (many (s *> p) <|> recoverWith r)

      parameters = commas (plzTok name)
      arguments  = commas (expression)
      expression = JStr    <$> strTok
               <|> JNum    <$> numTok
               <|> JObj    <$> spec '{' <*> plzSpc '}'
               <|> ExprErr <$> recoverWith (symbol (const True))


      -- * Simple parsers

      strTok = symbol (\t -> case tokT t of
                               Str _ -> True
                               _     -> False)

      numTok = symbol (\t -> case tokT t of
                               Number _ -> True
                               _        -> False)

      comments = symbol (\t -> case tokT t of
                                 Comment _ -> True
                                 _         -> False)

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

      commas  = (`sepBy`  (spec ','))
      commas1 = (`sepBy1` (spec ','))

      -- | Recover operator.  Prefers RHS.
      x <>> y = recoverWith x <|> y

      -- | Recovery operator.  Prefers LHS.
      x <<> y = y <>> x
      plzLst x = pure [] <>> x
      plzTok x = (pure unknownToken) <>> x
      plzSpc   = plzTok . spec

      unknownToken = tokFromT Unknown
