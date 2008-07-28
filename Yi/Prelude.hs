{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Yi.Prelude 
    (

(++),
(=<<),
Double,
Char,
Either(..),
Endom,
Eq(..),
Fractional(..),
Functor(..),
IO,
Integer,
Integral(..),
Bounded(..),
Enum(..),
Maybe(..),
Monad(..),
Num(..),
Ord(..),
Read(..),
Real(..),
ReaderT(..),
SemiNum(..),
String,
fromIntegral,
fst,
fst3,
head,
init,
io,
last,
lookup,
module Control.Applicative,
module Data.Bool,
module Data.Foldable,
module Data.Function,
module Data.Int,
module Data.Traversable,
module Text.Show,
module Yi.Accessor,
module Yi.Debug,
null,
print,
putStrLn,
replicate,
read,
seq,
singleton,
snd,
snd3,
tail,
trd3,
undefined,
unlines,
when,

    ) where

import Yi.Debug
import Yi.Accessor
import Text.Show
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Int
import Control.Monad.Reader
import Control.Applicative
import Data.Traversable 
import Control.Monad
    
type Endom a = a -> a

io :: MonadIO m => IO a -> m a
io = liftIO

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

class SemiNum absolute relative | absolute -> relative where
    (+~) :: absolute -> relative -> absolute
    (-~) :: absolute -> relative -> absolute
    (~-) :: absolute -> absolute -> relative

singleton :: a -> [a]
singleton x = [x]
