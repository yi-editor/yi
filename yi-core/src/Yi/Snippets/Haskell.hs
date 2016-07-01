{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Snippets.Haskell
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Snippets.Haskell where

import qualified Yi.Rope     as R (singleton)
import           Yi.Snippets (SnippetCmd, cursor, cursorWith, snippet, (&))

hsFunction :: SnippetCmd ()
hsFunction = snippet $
  cursorWith 1 (R.singleton 'f') & " :: " & cursor 2 & "\n" &
  cursor 3 & " = " & cursor 4 & "\n"

hsClass :: SnippetCmd ()
hsClass = snippet $
  "class " & cursor 1 & " " & cursor 2 & " where\n  " & cursor 3
