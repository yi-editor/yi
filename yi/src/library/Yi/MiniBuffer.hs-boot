-- -*- haskell -*-

module Yi.MiniBuffer where

import {-# SOURCE #-} Yi.Keymap (YiM)

withMinibufferFree :: String -> (String -> YiM ()) -> YiM ()
