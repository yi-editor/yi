{-# LANGUAGE
  ScopedTypeVariables,
  FlexibleInstances,
  DeriveFunctor,
  DeriveFoldable,
  DeriveTraversable,
  TypeFamilies,
  CPP,
  NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- uniplate patterns
module Yi.Syntax.OnlineTree (Tree(..), manyToks,
                             tokAtOrBefore) where
import Prelude ()

import Control.Applicative
import Data.Traversable
import Data.Foldable

#ifdef TESTING
import Test.QuickCheck
import Parser.Incremental
#endif

import Yi.Buffer.Basic
import Yi.Prelude
import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Syntax.Tree

#ifdef TESTING
instance Arbitrary Point where
    arbitrary = Point  <$> arbitrary
#endif


data MaybeOneMore f x = None | OneMore x (f x)
    deriving Show

data Tree a = Bin (Tree a) (Tree a)
            | Leaf a
            | Tip
              deriving (Show, Functor, Foldable, Traversable)

instance IsTree Tree where
    emptyNode = Tip
    uniplate (Bin l r) = ([l,r],\[l',r'] -> Bin l' r')
    uniplate t = ([],\_->t)

manyToks :: P (Tok t) (Tree (Tok t))
manyToks = manyToks' 1

manyToks' :: Int -> P a (Tree a)
manyToks' n = Look (pure Tip) (\_ -> Bin <$> subTree n <*> manyToks' (n * 2))

subTree :: Int -> P a (Tree a)
subTree n = Look (pure Tip) (\_ ->
   case n of
       0 -> pure Tip
       1 -> Leaf <$> symbol (const True)
       _ -> let m = n `div` 2 in Bin <$> subTree m <*> subTree m)


