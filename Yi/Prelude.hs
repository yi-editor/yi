module Yi.Prelude 
    (
     String,
     Yi.Prelude.map,
     Monad(..),
     (=<<),
     Either(..),
     Endom,
     module Data.Function,
     Eq(..),
     module Data.Int,
Char,
Integer,
otherwise,
Ord(..),
Functor(..),
module Text.Show,
    ) where


import Text.Show
import Data.Function
import Data.Int
    
type Endom a = a -> a

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
