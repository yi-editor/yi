{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- uniplate patterns
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Syntax.OnlineTree
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module defining the 'Tree' used as part of many 'Mode's.

module Yi.Syntax.OnlineTree (Tree(..), manyToks,
                             tokAtOrBefore) where

import Control.Applicative (Applicative ((<*>), pure), (<$>))
import Data.Foldable       (Foldable)
import Data.Traversable    (Traversable)
import Yi.IncrementalParse (P, Parser (Look), symbol)
import Yi.Lexer.Alex       (Tok)
import Yi.Syntax.Tree      (IsTree (emptyNode, uniplate), tokAtOrBefore)

data Tree a = Bin (Tree a) (Tree a)
            | Leaf a
            | Tip
              deriving (Show, Functor, Foldable, Traversable)

instance IsTree Tree where
    emptyNode = Tip
    uniplate (Bin l r) = ([l,r],\[l',r'] -> Bin l' r')
    uniplate t = ([],const t)

manyToks :: P (Tok t) (Tree (Tok t))
manyToks = manyToks' 1

manyToks' :: Int -> P a (Tree a)
manyToks' n = Look (pure Tip) (\_ -> Bin <$> subTree n <*> manyToks' (n * 2))

subTree :: Int -> P a (Tree a)
subTree n = Look (pure Tip) . const $ case n of
  0 -> pure Tip
  1 -> Leaf <$> symbol (const True)
  _ -> let m = n `div` 2 in Bin <$> subTree m <*> subTree m
