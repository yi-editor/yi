-- -*- haskell -*-

module Yi.MiniBuffer where

import qualified Data.Text as T
import           {-# SOURCE #-} Yi.Keymap (YiM)

withMinibufferFree :: T.Text -> (T.Text -> YiM ()) -> YiM ()
