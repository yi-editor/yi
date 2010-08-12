module Yi.File where

import Yi.Buffer.Basic
import Yi.Keymap

editFile :: FilePath -> YiM BufferRef
