{-# LANGUAGE TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}

-- | The BList is like a zipper on lists: it is a list with one point of focus.
-- We provide parsing functions for BLists, and the corresponding traversal functions.
-- When used together, accessing the elements around the last point of modification
-- is fast: they are at the beginning of the lists.

module Yi.Syntax.BList (foldMapAfter, many, some, sepBy, BList, sepBy1, nil) where

import Data.Monoid
import Parser.Incremental 
import Prelude (reverse, takeWhile)
import Yi.Prelude hiding (some, many)
import Yi.Syntax.Tree hiding (sepBy, sepBy1)
import Yi.Lexer.Alex
import Yi.Buffer.Basic
import Data.Data
import Data.Typeable

data BList a = One [a] | Two ([a] -> [a]) [a]
    deriving (Data, Typeable)

instance Show a => Show (BList a) where
    show (One r) =  "[]<>" ++ show r
    show (Two l r) = show (reverse $ l []) ++ "<>" ++ show r

-- Note that consR /must/ be lazy in its rhs argument.
-- It's possible because it /cannot/ take the 'Two' case.
consR, consL :: a -> BList a -> BList a
consR x ~(One r) = One  (x:r)
consL x (One r) = Two (x:) r
consL x (Two l r) = Two (l . (x:))  r

cons :: Parser s (a -> BList a -> BList a)
cons = Pure consL consR

nil :: BList a
nil = One []

many, some :: Parser s a -> Parser s (BList a)
many v = some v <|> pure nil
some v = cons <*> v <*> many v

sepBy :: Parser s a -> Parser s a1 -> Parser s (BList a)
sepBy p q = pure nil <|> sepBy1 p q

sepBy1 :: Parser s a -> Parser s a1 -> Parser s (BList a)
sepBy1 p q = cons <*> p <*> many (q *> p)


instance Foldable BList where
    foldMap f (One r) = foldMap f r
    foldMap f (Two l r) = getDual (foldMap (Dual . f) (l [])) <> foldMap f r

foldMapAfter :: (SubTree a, Element a ~ Tok t, Monoid m) => Point -> (a -> m) -> BList a -> m
foldMapAfter _ f (One r)   = foldMap f r    
foldMapAfter begin f (Two l r) = getDual (foldMap (Dual . f) (takeWhile (\t -> getLastOffset  t > begin) $ l [])) 
                              <> foldMap f r 


instance (Element a ~ Tok t, SubTree a) =>SubTree (BList a) where
    type Element (BList a) = Element a
    foldMapToksAfter begin f t = foldMapAfter begin (foldMapToksAfter begin f) t
    foldMapToks f = foldMap (foldMapToks f)