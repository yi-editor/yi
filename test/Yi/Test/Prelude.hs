module Yi.Test.Prelude ( module Yi.Test.Prelude
                       , module Control.Applicative
                       , module Control.Monad
                       , module Control.Monad.Error
                       , module Data.Maybe
                       , module Data.Foldable
                       , module Distribution.TestSuite
                       , module Text.Printf
                       , module Text.Regex.Posix
                       , module System.FilePath
                       ) where

import Control.Applicative

import Control.Monad ( when )
import Control.Monad.Error ( MonadError(..), ErrorT(..), throwError )

import Data.Maybe
import Data.Foldable
import Distribution.TestSuite

import System.IO

import Text.Printf
import Text.Regex.Posix hiding ( empty )

import System.FilePath

info f = do
    hPrintf stderr (if last f == '\n' then f else f ++ "\n")
    hFlush stderr

assert :: (MonadError String m, Monad m) => Bool -> String -> m ()
assert b f_str = when (not b) (throwError f_str)

