module Main where

import Yi
import Yi.Keymap
import Yi.Keymap.Vim 
import Yi.Snippets

main :: IO ()
main = yi $ defaultVimConfig {
    defaultKm = myVimKeymap
}

deleteSnippets = False

myVimKeymap = mkKeymap $ defKeymap `override` \super self -> super {
          v_ins_char  = (v_ins_char super ||> tabKeymap)
                          <|> (ctrlCh 's' ?>>! moveToNextBufferMark deleteSnippets)
        }

tabKeymap = superTab True $ fromSnippets deleteSnippets $
  [("test", testSnippet)]

testSnippet :: SnippetCmd ()
testSnippet =
    "if ( " & (cursorWith 1 "...") & " ) {\n" &
    "\t" & (cursorWith 2 "/* code */") &
    "\n}\n" & (cursor 3)
