module Yi.Cbits where

import Foreign.C.Types
import Foreign.Ptr


foreign import ccall unsafe "string.h strstr"
    cstrstr :: Ptr CChar -> Ptr CChar -> IO (Ptr CChar)

foreign import ccall unsafe "YiUtils.h countLines"
   ccountLines :: Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "YiUtils.h findStartOfLineN"
   cfindStartOfLineN :: Ptr CChar -> Int -> Int -> Int -> IO Int
