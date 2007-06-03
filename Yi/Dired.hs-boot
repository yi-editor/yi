module Yi.Dired (
        diredE
       ,diredDirE
       ,diredDirBufferE
    ) where

import Yi.Keymap (EditorM)
import Yi.Buffer (FBuffer)

diredE :: EditorM ()

diredDirE :: FilePath -> EditorM ()

diredDirBufferE :: FilePath -> EditorM FBuffer
