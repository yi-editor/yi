{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | The BList is like a zipper on lists: it is a list with one point of focus.
-- We provide parsing functions for BLists, and the corresponding traversal functions.
-- When used together, accessing the elements around the last point of modification
-- is fast: they are at the beginning of the lists.

module Yi.Syntax.BList (foldMapAfter, many, some, sepBy, BList) where

import Data.Monoid
import Parser.Incremental 
import Prelude (reverse, takeWhile, dropWhile)
import Yi.Prelude hiding (some, many)
import Yi.Syntax.Tree hiding (sepBy)
import Yi.Lexer.Alex
import Yi.Buffer.Basic

data BList a = One [a] | Two ([a] -> [a]) [a]

instance Show a => Show (BList a) where
    show (One r) =  "[]<>" ++ show r
    show (Two l r) = show (reverse $ l []) ++ "<>" ++ show r

consR x ~(One r) = One  (x:r)
consL x (One r) = Two (x:) r
consL x (Two l r) = Two (l . (x:))  r

cons = Pure consL consR
nil = One []

many v = some v <|> pure nil
some v = cons <*> v <*> many v
sepBy p q = pure nil <|> cons <*> p <*> many (q *> p)


instance Foldable BList where
    foldMap f (One r) = foldMap f r
    foldMap f (Two l r) = getDual (foldMap (Dual . f) (l [])) <> foldMap f r

foldMapAfter :: (SubTree a, Element a ~ Tok t, Monoid m) => Point -> (a -> m) -> BList a -> m
foldMapAfter begin f (One r)   = foldMap f r    
foldMapAfter begin f (Two l r) = getDual (foldMap (Dual . f) (takeWhile (\t -> getLastOffset  t > begin) $ l [])) 
                              <> foldMap f r 


instance (Element a ~ Tok t, SubTree a) =>SubTree (BList a) where
    type Element (BList a) = Element a
    foldMapToksAfter begin f t = foldMapAfter begin (foldMapToksAfter begin f) t
    foldMapToks f = foldMap (foldMapToks f)
