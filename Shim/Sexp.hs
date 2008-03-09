{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances -Wall #-}

-- Copyright (C) 2006 Benedikt Schmidt
-- see LICENSE.BSD3 for license

module Shim.Sexp
  (
    ConvSexp(..)
  , Sexp(..)
  , Symbol(..)
  , Pair(..)
  , Se(..)
  , parseS
  , pp
  , nil
  )
where

import Text.ParserCombinators.Parsec
import Char (intToDigit)
import Control.Monad
import Data.Monoid
import Shim.Utils

-- how do we handle heterogeneous lists?
data Sexp = SInt Int | SString String | SSymbol String | SList [Sexp]
          | SPair Sexp Sexp | SBool Bool deriving Show

instance Monoid Sexp where
  mempty = SList []
  mappend (SList x) (SList y) = SList (x++y)
  mappend (SList x) y = SList (x ++ [y])
  mappend y (SList x) = SList (y : x)
  mappend y x = SList [y,x]

newtype Symbol = S String
newtype Pair a b = P (a,b)

nil :: [Int]
nil = []
-- ConvSexp class and instances
class ConvSexp a where
  toS :: a -> Sexp
  fromS :: Sexp -> Maybe a

instance ConvSexp Int where
  toS = SInt
  fromS (SInt i) = Just i
  fromS _ = Nothing

instance ConvSexp Bool where
  toS = SBool
  fromS (SBool b) = Just b
  fromS _ = Nothing

instance ConvSexp String where
  toS = SString
  fromS (SString s) = Just s
  fromS _ = Nothing

instance ConvSexp Symbol where
  toS (S s) = SSymbol s
  fromS (SSymbol s) = Just $ S s
  fromS _ = Nothing

instance ConvSexp () where
  toS ()  = SList []
  fromS _ = return ()

instance ConvSexp a => ConvSexp (Maybe a) where
  toS (Just x) = SList [toS x]
  toS Nothing = SList []
  fromS (SList [a]) = do a' <- fromS a
                         return (Just a')
  fromS (SList []) = Just Nothing
  fromS (SBool False) = Just Nothing
  fromS _ = Nothing

instance ConvSexp a => ConvSexp [a] where
  toS xs = SList $ map toS xs
  fromS (SList l) = foldl (\l0 x0 -> do { l1 <- l0; x <- fromS x0; return $ x:l1})
                          (Just []) (reverse l)
  fromS _ = Nothing

instance (ConvSexp a, ConvSexp b) => ConvSexp (a,b) where
  toS (a,b) = SList [toS a, toS b]
  fromS (SList [a,b]) = do { a' <- fromS a; b' <- fromS b; return $ (a',b')}
  fromS (SPair a b) = do { a' <- fromS a; b' <- fromS b; return $ (a',b')}
  fromS _ = Nothing

instance (ConvSexp a, ConvSexp b) => ConvSexp (Pair a b) where
  toS (P (a,b)) = SPair (toS a) (toS b)
  fromS (SPair a b) = do { a' <- fromS a; b' <- fromS b; return $ P (a',b')}
  fromS _ = Nothing

instance ConvSexp Sexp where 
  toS   x = x
  fromS x = Just x

-- existential wrapper
data Se = forall a. ConvSexp a => Se a

instance ConvSexp Se where
  toS (Se x) = toS x
  fromS _ = Nothing

-- PrettyPrinting
pp :: Sexp -> String
pp (SInt i) = show i
pp (SBool False) = "nil"
pp (SBool True) = "t"
pp (SString s) = show s
pp (SSymbol s) = s -- elisp doesn't support |foo bar|, add quoting later
pp (SPair a b) = "(" ++ pp a ++ " . " ++ pp b ++ ")"
pp (SList l) = "(" ++ (unSplit ' ' $ map pp l) ++ ")"

-- Parsing
parseS :: [Char] -> Either ParseError Sexp
parseS = parse (consumeAll parseElem) ""

parseElem :: GenParser Char st Sexp
parseElem = parseInt
            <|> try parseBool
            <|> parseString
            <|> parseSymbol
            <|> try parseList
            <|> try parsePair

parseBool :: GenParser Char st Sexp
parseBool = do _c <- char 't'
               lookAhead (oneOf "\n \t")
               return $ SBool True
           <|> do string "nil"
                  return $ SBool False

parseInt :: GenParser Char st Sexp
parseInt = do c <- try $ oneOf cdigits
              s <- many (oneOf cdigits)
              return $ SInt $ read (c:s)
  where cdigits = map intToDigit [0..9]

parseString :: GenParser Char st Sexp
parseString = between (try $ char '"') (char '"') quotedContent

parseList :: GenParser Char st Sexp
parseList = between (char '(') (char ')') pElems
  where pElems = do l <- sepBy parseElem whiteSpace
                    return $ SList l

parseSymbol :: GenParser Char st Sexp
parseSymbol = do let fchars = "\"(). "
                 c <- try $ noneOf fchars
                 s <- many (noneOf fchars)
                 return $ SSymbol (c:s)

parsePair :: GenParser Char st Sexp
parsePair = between (char '(') (char ')') pPair
  where pPair = do a <- parseElem
                   whiteSpace
                   char '.'
                   whiteSpace
                   b <- parseElem
                   return $ SPair a b

quotedContent :: GenParser Char st Sexp
quotedContent = do s1 <- escape <|> (many1 (noneOf ['"', '\\']))
                   SString s2 <- quotedContent
                   return $ SString $ s1 ++ s2
               <|> (return $ SString "")

escape :: GenParser Char st [Char]
escape = do char '\\'
            c <- anyChar
            return [c]

whiteSpace :: GenParser Char st ()
whiteSpace = try $ skipMany1 (oneOf " \n\t")

consumeAll :: Parser a -> Parser a
consumeAll p = do r <- p
                  whiteSpace
                  eof
                  return r

-- Testing
--testPV :: (ConvSexp t_aLIK) => t_aLIK -> Either ParseError Sexp
--testPV v = parseS $ pp $ toS v
--t :: IO ()
--t = do let sexp = toS (S ":complete-module", ["Data.Arr", "/home/steele/a.hs"])
--       print sexp
--       putStrLn $ pp sexp

-- some more tuple instances, use template-haskell?
instance (ConvSexp a, ConvSexp b, ConvSexp c) => ConvSexp (a,b,c) where
  toS (a,b,c) = SList [toS a, toS b, toS c]
  fromS (SList [a,b,c]) = do a' <- fromS a
                             b' <- fromS b
                             c' <- fromS c
                             return $ (a',b',c')
  fromS _ = Nothing

instance (ConvSexp a, ConvSexp b, ConvSexp c, ConvSexp d)
  => ConvSexp (a,b,c,d) where
  toS (a,b,c,d) = SList [toS a,toS b,toS c,toS d]
  fromS (SList [a,b,c,d]) = do a' <- fromS a
                               b' <- fromS b
                               c' <- fromS c
                               d' <- fromS d
                               return $ (a',b',c',d')
  fromS _ = Nothing

instance (ConvSexp a, ConvSexp b, ConvSexp c, ConvSexp d, ConvSexp e)
  => ConvSexp (a,b,c,d,e) where
  toS (a,b,c,d,e) = SList [toS a,toS b,toS c,toS d,toS e]
  fromS (SList [a,b,c,d,e]) = do a' <- fromS a
                                 b' <- fromS b
                                 c' <- fromS c
                                 d' <- fromS d
                                 e' <- fromS e
                                 return $ (a',b',c',d',e')
  fromS _ = Nothing

instance (ConvSexp a, ConvSexp b, ConvSexp c, ConvSexp d, ConvSexp e, ConvSexp f)
  => ConvSexp (a,b,c,d,e,f) where
  toS (a,b,c,d,e,f) = SList [toS a,toS b,toS c,toS d,toS e,toS f]
  fromS (SList [a,b,c,d,e,f]) = do a' <- fromS a
                                   b' <- fromS b
                                   c' <- fromS c
                                   d' <- fromS d
                                   e' <- fromS e
                                   f' <- fromS f
                                   return $ (a',b',c',d',e',f')
  fromS _ = Nothing
