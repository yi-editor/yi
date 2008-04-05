module Yi.Prelude 
    (

(++),
(=<<),
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
fromIntegral,
fst,
fst3,
head,
init,
last,
lookup,
mapM_,
module Data.Bool,
module Data.Function,
module Data.Int,
module Data.Traversable,
module Text.Show,
module Yi.Debug,
null,
print,
putStrLn,
read,
seq,
snd,
snd3,
tail,
trd3,
undefined,
unlines,

    ) where

import Yi.Debug
import Text.Show
import Data.Bool
import Data.Function
import Data.Int
import Control.Monad.Reader
import Data.Traversable 
    
type Endom a = a -> a

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x
