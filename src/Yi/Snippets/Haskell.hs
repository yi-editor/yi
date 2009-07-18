module Yi.Snippets.Haskell where

import Yi.Snippets

hsFunction :: SnippetCmd ()
hsFunction = snippet $
  (cursorWith 1 "f") & " :: " & (cursor 2) & "\n" &
  (cursor 3) & " = " & (cursor 4) & "\n"

hsClass :: SnippetCmd ()
hsClass = snippet $
  "class " & (cursor 1) & " " & (cursor 2) & " where\n  " & (cursor 3)
