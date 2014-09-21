module Yi.Buffer.HighLevel where

import Yi.Rope (YiString)
import {-# SOURCE #-} Yi.Buffer.Misc (BufferM)

toggleCommentSelectionB :: YiString -> YiString -> BufferM ()
toggleCommentB :: YiString -> BufferM ()