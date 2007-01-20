module Yi.UI where

import Yi.Window
import {-# source #-} Yi.Editor

deleteWindow' :: Editor -> Window -> IO Editor

deleteWindow :: (Maybe Window) -> IO ()

data UI
