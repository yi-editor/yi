module Yi.Prelude 
    (

(++),
(=<<),
Bool(..),
Char,
Either(..),
Endom,
Eq(..),
Functor(..),
IO,
Integer,
Integral(..),
Bounded(..),
Maybe(..),
Monad(..),
Num(..),
Ord(..),
Read(..),
ReaderT(..),
String,
Yi.Prelude.map,
concat,
elem,
fst,
head,
lookup,
module Data.Function,
module Data.Int,
module Text.Show,
module Yi.Debug,
otherwise,
putStrLn,
read,
seq,
snd,
tail,
undefined,
unlines,

    ) where

import Yi.Debug
import Text.Show
import Data.Function
import Data.Int
import Control.Monad.Reader
    
type Endom a = a -> a

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
