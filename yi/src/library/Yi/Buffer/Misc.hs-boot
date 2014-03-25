-- -*- haskell -*-

module Yi.Buffer.Misc where

import Control.Monad.RWS.Strict hiding (mapM_, mapM, get, put, forM_, forM)

import Yi.Window (Window)
import Yi.Buffer.Implementation (Update)

data FBuffer
newtype BufferM a = BufferM { fromBufferM :: RWS Window [Update] FBuffer a }
