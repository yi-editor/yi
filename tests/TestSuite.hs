module TestSuite where

import Driver

import Control.Monad.Identity
import Data.Traversable
import Text.Show.Functions ()

main :: IO ()
main = Driver.main tests

type Prop_Functor f a = f a -> Bool

prop_functor :: (Eq (f a), Functor f) => f a -> Bool
prop_functor ws = fmap id ws == ws

prop_traversable :: (Eq (t b), Traversable t) => t a -> (a -> b) -> Bool
prop_traversable ws f = runIdentity (Data.Traversable.mapM (Identity . f) ws) == fmap f ws

tests :: [(String, Int -> IO (Bool, Int))]
tests = 
    [ -- Format like this in the future, but no tests are applicable here:
      -- ("WindowSet prop_traversable" , mytest (prop_traversable :: WindowSet Int -> (Int -> Int) -> Bool))
    ]


